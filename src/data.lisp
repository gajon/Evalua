(in-package #:evalua)

(defmacro %lowassoc (field-name obj)
  "We want to use lowercase field names on the database(CouchDB) which means
that we need symbols like :|foo| when accessing values from the alists we get
back from CouchDB, but it is annoying to have to type them.
This macro saves some typing:
(%lowassoc foo alist) is expanded into `(cdr (assoc :|foo| alist))"
  ;; the `remove-quote` function is needed because when field-name
  ;; is a quoted symbol we would get a symbol like :|'foo|
  (flet ((remove-quote (symbol)
                       (if (and (consp symbol) (eq (car symbol) 'QUOTE))
                         (cadr symbol)
                         symbol)))
    `(cdr (assoc
            ,(intern
               (format nil "~(~a~)" (mkstr (remove-quote field-name)))
               "KEYWORD")
            ,obj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOCUMENT RETRIEVAL

;;;
;;; Forms
;;;

(defun data/get-form (id)
  (handler-case
    (data/build-form-from-alist (clouchdb:get-document id))
    (error () nil)))

(defun data/get-form-by-public-id (public-id)
  (let ((data
          (clouchdb:query-document
            `(:|rows| :|id| ,#'clouchdb:get-document)
            (clouchdb:invoke-view "forms" "forms-by-pub-id" :key public-id))))
    (when data
      (data/build-form-from-alist (car data)))))

(defun data/get-active-forms (user)
  (let ((user (if (eq (type-of user) 'user) (user-username user) user)))
    (mapcar #'data/build-form-from-alist
            (clouchdb:query-document
              `(:|rows| :|id| ,#'clouchdb:get-document)
              (clouchdb:invoke-view "forms" "active-forms" :key user)))))

(defun data/get-inactive-forms (user)
  (let ((user (if (eq (type-of user) 'user) (user-username user) user)))
    (mapcar #'data/build-form-from-alist
            (clouchdb:query-document
              `(:|rows| :|id| ,#'clouchdb:get-document)
              (clouchdb:invoke-view "forms" "inactive-forms" :key user)))))

(defun data/get-forms-by-date (user date)
  (declare (ignore user date))
  (error "Not implemented"))

;;;
;;; Questions
;;;

(defun data/get-questions-by-form (form-id &key raw-alist)
  ;; The reason we are doing nreverse below is that the function
  ;; clouchdb:query-document reverses the results it receives
  ;; after matching them against the query. For instance, if the
  ;; view returns data like
  ;; '(:|rows| ((:|id| . "id1")
  ;;            (:|id| . "id2")
  ;;            (:|id| . "id3")))
  ;; and we filter that with (query-document '(:|rows| :|id|) *)
  ;; we will get
  ;; ("id3" "id2" "id1")
  ;;
  ;; Of course we could get the order we want by inverting the order
  ;; in the invoke-view call, but it feels kludgy.
  (mapcar (if raw-alist #'identity #'data/build-question-from-alist)
          (nreverse
            (clouchdb:query-document
              `(:|rows| :|id| ,#'clouchdb:get-document)
              (clouchdb:invoke-view "questions" "questions-by-form"
                                    :start-key (list form-id)
                                    :end-key (list form-id
                                                   (make-hash-table)))))))

(defun data/get-answers-by-question (question-id &key raw-alist)
  (mapcar (if raw-alist #'identity #'data/build-answer-from-alist)
          (nreverse
            (clouchdb:query-document
              `(:|rows| :|id| ,#'clouchdb:get-document)
              (clouchdb:invoke-view "answers" "answers-by-question"
                                    :start-key (list question-id)
                                    :end-key (list question-id
                                                   (make-hash-table)))))))

;;;
;;; Users
;;;

(defun data/get-user (username)
  (handler-case (data/build-user-from-alist (clouchdb:get-document username))
    (error () nil)))

(defun data/validate-credentials (username password)
  (handler-case
    (let ((alist (clouchdb:get-document username))
          (digest (hunchentoot::md5-hex password)))
      (string= digest (%lowassoc password-digest alist)))
    (error () nil)))


;;;
;;; Submissions
;;;

(defun data/get-submission (id)
  (handler-case
      (data/build-submission-from-alist
       (clouchdb:get-document id))
    (error () nil)))

(defun data/get-submissions-by-form (form)
  (let ((form (if (eq (type-of form) 'form) (form-id form) form)))
    ;; clouchdb:query-document reverses our results, so we re-reverse.
    (mapcar #'data/build-submission-from-alist
            (nreverse
              (clouchdb:query-document
                `(:|rows| :|id| ,#'clouchdb:get-document)
                (clouchdb:invoke-view "submissions" "submissions-by-form"
                                      :reduce nil
                                      :descending nil
                                      :start-key (list form)
                                      :end-key (list form
                                                     (make-hash-table))))))))

(defun data/get-submissions-by-form-count (form)
  (let* ((form (if (eq (type-of form) 'form) (form-id form) form))
         (value
           (clouchdb:query-document
             '(:|rows| :|value|)
             (clouchdb:invoke-view "submissions" "submissions-by-form"
                                   :reduce t
                                   :start-key (list form)
                                   :end-key (list form (make-hash-table))))))
    (car value)))

(defun data/get-submitted-answers-by-question-count (question)
  (let ((qid (if (eq (type-of question) 'question)
                 (question-id question)
                 question)))
    (car
     (clouchdb:query-document
      '(:|rows| :|value|)
      (clouchdb:invoke-view "submissions"
                            "submitted-answers-by-question"
                            :group t :reduce t :key qid)))))

(defun data/get-submitted-answers-count (answer)
  (let ((aid (if (eq (type-of answer) 'answer) (answer-id answer) answer)))
    (car
     (clouchdb:query-document
      `(:|rows| :|value|)
      (clouchdb:invoke-view "submissions"
                            "submitted-answers"
                            :group t :reduce t :key aid)))))

(defun data/get-submitted-answers-by-submission-question (sub question)
  (let ((sub (if (eq (type-of sub) 'submission) (submission-id sub) sub))
        (qid (if (eq (type-of question) 'question)
               (question-id question)
               question)))
    (clouchdb:query-document
      `(:|rows| :|id| ,#'clouchdb:get-document)
      (clouchdb:invoke-view "submissions"
                            "submitted-answers-by-submission-question"
                            :key (list sub qid)))))


;;;
;;; Util functions to build objects from alists returned by clouchdb
;;;

(defun data/build-form-from-alist (alist)
  (make-instance 'form
                 :id          (%lowassoc _id  alist)
                 :rev         (%lowassoc _rev alist)
                 :user        (%lowassoc user alist)
                 :date        (%lowassoc date alist)
                 :update-date (%lowassoc update-date alist)
                 :start-date  (%lowassoc start-date alist)
                 :valid-date  (%lowassoc valid-date alist)
                 :public-id   (%lowassoc public-id  alist)
                 :title       (%lowassoc title alist)
                 :notes       (%lowassoc notes alist)
                 :time-zone   (%lowassoc time-zone alist)
                 :status      (%lowassoc status alist)
                 :time-limit  (%lowassoc time-limit alist)
                 :tries-limit (%lowassoc tries-limit alist)
                 :score-p     (%lowassoc score-p alist)
                 :comments-p  (%lowassoc comments-p alist)
                 :email-dest  (%lowassoc email-dest alist)))

(defun data/build-question-from-alist (alist)
  (make-instance 'question
                 :id      (%lowassoc _id alist)
                 :rev     (%lowassoc _rev alist)
                 :control (%lowassoc control alist)
                 :text    (%lowassoc text alist)
                 :sort    (%lowassoc sort alist)))

(defun data/build-answer-from-alist (alist)
  (make-instance 'answer
                 :id      (%lowassoc _id alist)
                 :rev     (%lowassoc _rev alist)
                 :sort    (%lowassoc sort alist)
                 :control (%lowassoc control alist)
                 :text    (%lowassoc text alist)))

(defun data/build-user-from-alist (alist)
  (make-instance 'user
                 :rev             (%lowassoc _rev alist)
                 :full-name       (%lowassoc full-name alist)
                 :username        (%lowassoc _id alist)
                 :password-digest (%lowassoc password-digest alist)
                 :email           (%lowassoc email alist)
                 :time-zone       (%lowassoc time-zone alist)))


(defun data/build-submission-from-alist (alist)
  (make-instance 'submission
                 :id          (%lowassoc _id alist)
                 :rev         (%lowassoc _rev alist)
                 :form        (%lowassoc form alist)
                 :start-date  (%lowassoc start-date alist)
                 :finish-date (%lowassoc finish-date alist)
                 :ip          (%lowassoc ip alist)
                 :user-agent  (%lowassoc user-agent alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOCUMENT STORAGE

(defun data/create-fresh-form (form)
  (flet ((%create-form (form)
           (let* ((secret-id (kmrcl:random-string
                               :set :lower-alphanumeric :length 20))
                  (public-id (kmrcl:random-string
                               :set :lower-alphanumeric :length 10))
                  (doc (clouchdb:create-document
                         `((:|type| .        "form")
                           (:|user| .        ,(form-user form))
                           (:|date| .        ,(slot-value form 'date))
                           (:|update-date| . ,(slot-value form 'update-date))
                           (:|start-date| .  ,(slot-value form 'start-date))
                           (:|valid-date| .  ,(slot-value form 'valid-date))
                           (:|public-id| .   ,public-id)
                           (:|title| .       ,(form-title form))
                           (:|notes| .       ,(form-notes form))
                           (:|time-zone| .   ,(form-time-zone form))
                           (:|status| .      "fresh")
                           (:|time-limit| .  ,(form-time-limit form))
                           (:|tries-limit| . ,(form-tries-limit form))
                           (:|score-p| .     ,(form-score-p form))
                           (:|comments-p| .  ,(form-comments-p form))
                           (:|email-dest| .  ,(form-email-dest form)))
                         :id secret-id)))
             ;; TODO: Should we check (eql (%lowassoc ok doc) T)?
             (when doc
               (setf (form-id form) secret-id
                     (form-public-id form) public-id
                     (form-rev form) (%lowassoc rev doc)
                     (form-status form) "fresh")
               form))))
    (handler-case
      (%create-form form)
      (clouchdb:id-or-revision-conflict (c)
        (declare (ignore c))
        ;; Try once more.. this should be rare, the random number should
        ;; be sufficiently big so that collisions are rare.
        ;; TODO: this is stupid, find an available ID first.
        (ignore-errors
          (%create-form form))))))

(defun data/save-form (form)
  (when (string= (form-status form) "fresh")
    (setf (form-status form) "inactive"))
  (let* ((now (make-date (get-universal-time) (or (form-time-zone form) 6)))
         (saved? (clouchdb:put-document
                   `((:|_id| .         ,(form-id form))
                     (:|_rev| .        ,(form-rev form))
                     (:|type| .        "form")
                     (:|user| .        ,(form-user form))
                     (:|date| .        ,(slot-value form 'date))
                     (:|update-date| . ,(format-iso8601-date now))
                     (:|start-date| .  ,(slot-value form 'start-date))
                     (:|valid-date| .  ,(slot-value form 'valid-date))
                     (:|public-id| .   ,(form-public-id form))
                     (:|title| .       ,(form-title form))
                     (:|notes| .       ,(form-notes form))
                     (:|time-zone| .   ,(form-time-zone form))
                     (:|status| .      ,(form-status form))
                     (:|time-limit| .  ,(form-time-limit form))
                     (:|tries-limit| . ,(form-tries-limit form))
                     (:|score-p| .     ,(form-score-p form))
                     (:|comments-p| .  ,(form-comments-p form))
                     (:|email-dest| .  ,(form-email-dest form))))))
    (when (%lowassoc ok saved?)
      ;; Update the _rev info, just in case.
      (setf (form-rev form) (%lowassoc rev saved?)
            (form-update-date form) now)
      form)))

(defun data/save-form-questions (form-obj questions)
  ;; The questions link back to the form using the form's id, and similarly
  ;; the answers link back to each question using the question's id.
  ;;
  ;; When a question or answer have a non-null _id fields, it means that
  ;; it's an existing document and we should just update it.
  ;; This is taken care of by data%save-question and data%save-answer.
  (labels ((save-question (alist form-id)
             (let ((saved (data%save-question alist form-id)))
               ;; TODO: error handling?
               (when (%lowassoc id saved)
                 (mapc #'data%save-answer
                       (%lowassoc answers alist)
                       (repeatedly (%lowassoc id saved)))))))
    (mapc #'save-question
          questions
          (repeatedly (form-id form-obj)))))

(defun data%save-question (alist form-id)
  (let* ((qid (%lowassoc _id alist))
         (rev (when qid
                (%lowassoc _rev (clouchdb:get-document qid
                                                       :if-missing :ignore))))
         (question `(,@(when qid `((:|_id| . ,qid)))
                     ,@(when rev `((:|_rev| . ,rev)))
                     (:|type| .    "question")
                     (:|sort| .    ,(%lowassoc question-number alist))
                     (:|control| . ,(%lowassoc control alist))
                     (:|text| .    ,(%lowassoc text alist))
                     (:|form| .    ,form-id))))
    (if (and qid rev)
      (clouchdb:put-document question)
      (clouchdb:post-document question))))

(defun data%save-answer (alist question-id)
  (let* ((aid (%lowassoc _id alist))
         (rev (when aid
                (%lowassoc _rev (clouchdb:get-document aid
                                                       :if-missing :ignore))))
         (answer `(,@(when aid `((:|_id| . ,aid)))
                   ,@(when rev `((:|_rev| . ,rev)))
                   (:|type| .     "answer")
                   (:|sort| .     ,(%lowassoc answer-number alist))
                   (:|control| .  ,(%lowassoc control alist))
                   (:|selected| . ,(%lowassoc selected alist))
                   (:|text| .     ,(%lowassoc text alist))
                   (:|question| . ,question-id))))
    (if (and aid rev)
      (clouchdb:put-document answer)
      (clouchdb:post-document answer))))

(defun data/create-user (user-obj)
  ;; TODO: Aren't this and data/save-user almost the same?
  (let ((saved? (clouchdb:create-document
               `((:|type| . "user")
                 (:|full-name| .       ,(user-full-name user-obj))
                 (:|email| .           ,(user-email user-obj))
                 (:|time-zone| .       ,(user-time-zone user-obj))
                 (:|password-digest| . ,(user-password-digest user-obj)))
               :id (user-username user-obj))))
    (when saved?
      (setf (user-rev user-obj) (%lowassoc rev saved?))
      user-obj)))

(defun data/save-user (user-obj)
  (let ((saved? (clouchdb:put-document
                  `((:|_id| .             ,(user-username user-obj))
                    (:|_rev| .            ,(user-rev user-obj))
                    (:|type| .            "user")
                    (:|full-name| .       ,(user-full-name user-obj))
                    (:|email| .           ,(user-email user-obj))
                    (:|time-zone| .       ,(user-time-zone user-obj))
                    (:|password-digest| . ,(user-password-digest user-obj))))))
    (when (%lowassoc ok saved?)
      ;; Update the _rev info, just in case.
      (setf (user-rev user-obj) (%lowassoc rev saved?))
      user-obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOCUMENT DELETION

(defun data/delete-form (form)
  (let* ((fid (if (eq (type-of form) 'form) (form-id form) form))
         (doc (clouchdb:get-document fid :if-missing :ignore)))
    (when doc
      (data/delete-form-questions fid)
      (clouchdb:delete-document fid :if-missing :ignore))))

(defun data/delete-form-questions (form)
  (let* ((fid (if (eq (type-of form) 'form) (form-id form) form))
         (questions (data/get-questions-by-form fid :raw-alist t)))
    (dolist (question questions)
      (data/delete-question-answers (%lowassoc _id question)))
    (clouchdb:bulk-document-update
      (mapcar #'clouchdb:as-deleted-document questions))))

(defun data/delete-form-question (form question)
  (let* ((fid (if (eq (type-of form) 'form) (form-id form) form))
         (qid (if (eq (type-of question) 'question)
                (question-id question) question))
         (doc (clouchdb:get-document qid :if-missing :ignore)))
    (when (and doc (string= fid (%lowassoc form doc)))
      (data/delete-question-answers qid)
      (clouchdb:delete-document qid :if-missing :ignore))))

(defun data/delete-question-answers (question)
  (let* ((qid (if (eq (type-of question) 'question)
                (question-id question) question)))
    (clouchdb:bulk-document-update
      (mapcar #'clouchdb:as-deleted-document
              (data/get-answers-by-question qid :raw-alist t)))))

(defun data/delete-question-answer (question answer)
  (let* ((qid (if (eq (type-of question) 'question)
                (question-id question) question))
         (aid (if (eq (type-of answer) 'answer) (answer-id answer) answer))
         (doc (clouchdb:get-document aid :if-missing :ignore)))
    (when (and doc (string= qid (%lowassoc question doc)))
      (clouchdb:delete-document aid :if-missing :ignore))))

(defun data/delete-form-parts (ids)
  (dolist (id ids)
    (awhen (clouchdb:get-document id :if-missing :ignore)
      (let ((type (%lowassoc type it)))
        (cond ((string= type "question")
               (data/delete-form-question (%lowassoc form it) id))
              ((string= type "answer")
               (data/delete-question-answer (%lowassoc question it) id))))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUBMITTED ANSWERS

(defun data/create-submission (sub)
  (let ((doc (clouchdb:create-document
               `((:|type| . "submission")
                 (:|form| . ,(submission-form sub))
                 (:|start-date| . ,(slot-value sub 'start-date))
                 (:|finish-date| . ,(slot-value sub 'finish-date))
                 (:|ip| . ,(submission-ip sub))
                 (:|user-agent| . ,(submission-user-agent sub))))))
    ;; TODO: Should we check (eql (%lowassoc ok doc) T)?
    (when doc
      (setf (submission-id sub) (%lowassoc id doc)
            (submission-rev sub) (%lowassoc rev doc))
      sub)))

(defun data/add-submitted-question (submission-id question-id answers-list)
  (clouchdb:create-document
   `((:|type| . "submitted-question")
     (:|question| . ,question-id)
     (:|submission| . ,submission-id)
     (:|answers| . ,answers-list))))

(defun data/add-submitted-comments (form-id sub-id comments now)
  (clouchdb:create-document
   `((:|type| . "submitted-comments")
     (:|form| . ,form-id)
     (:|submission| . ,sub-id)
     (:|date| . ,(format-iso8601-date now))
     (:|comments| . ,comments))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WAIT REGISTRY

(defun data/add-wait-registry (email date user-agent remote-addr)
  (clouchdb:create-document
    `((:|type| . "wait-registry")
      (:|email| . ,email)
      (:|user-agent| . ,user-agent)
      (:|remote-addr| . ,remote-addr)
      (:|date| . ,(format-iso8601-date date)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES TO SETUP THE COUCHDB DATABASE, MAINLY THE
;;; DESIGN VIEWS.

(defun %%create-design-documents ()
  (clouchdb:create-ps-view "forms"
    (clouchdb:ps-view ("active-forms")
      (defun map (doc)
        (with-slots (type user status) doc
          (if (and type user status (= type "form") (= status "active"))
            (emit user nil)))))
    (clouchdb:ps-view ("inactive-forms")
      (defun map (doc)
        (with-slots (type user status) doc
          (if (and type user status (= type "form") (= status "inactive"))
            (emit user nil)))))
    (clouchdb:ps-view ("fresh-forms")
      (defun map (doc)
        (with-slots (type user status) doc
          (if (and type user status (= type "form") (= status "fresh"))
            (emit user nil)))))
    (clouchdb:ps-view ("forms-by-pub-id")
      (defun map (doc)
        (with-slots (type :public-id status) doc
          (if (and type (= type "form") (= status "active"))
            (emit :public-id nil)))))
    #>END
    "forms-by-date": {
      "map": "function (doc) {
                var parts;
                if (doc.type && doc.date && doc.type === 'form') {
                  parts = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})/.exec(doc.date);
                  if (parts !== null) {
                    emit([doc.user,                // The User
                          parseInt(parts[1], 10),  // The Year
                          parseInt(parts[2], 10),  // The Month
                          parseInt(parts[3], 10)], // The Day
                         null);
                  }
                }
              }"
    }
    END)
  (clouchdb:create-ps-view "questions"
    (clouchdb:ps-view ("all-questions")
      (defun map (doc)
        (with-slots (type) doc
          (if (and type (= type "question"))
            (emit nil nil)))))
    (clouchdb:ps-view ("questions-by-form")
      (defun map (doc)
        (with-slots (type form sort _id) doc
          (if (and type (= type "question"))
            (emit (array form sort _id) nil))))))
  (clouchdb:create-ps-view "answers"
    (clouchdb:ps-view ("all-answers")
      (defun map (doc)
        (with-slots (type) doc
          (if (and type (or (= type "answer") (= type "sub-answer")))
            (emit nil nil)))))
    (clouchdb:ps-view ("answers-by-question")
      (defun map (doc)
        (with-slots (type question sort _id) doc
          (if (and type (= type "answer"))
            (emit (array question sort _id) nil))))))
  (clouchdb:create-ps-view "submissions"
    (clouchdb:ps-view ("submissions-by-form")
      (defun map (doc)
        (with-slots (type form :finish-date) doc
          (if (and type (= type "submission"))
            (emit (array form :finish-date) 1))))
      (defun reduce (keys values)
        (sum values)))
    ;#>END
    ;"submissions-by-form": {
      ;"map": "function (doc) {
                ;if (doc.type && doc.type === 'submission') {
                  ;return emit([doc.form, doc['finish-date']], 1);
                ;}
              ;}",

      ;"reduce": "function (keys, values) {
                  ;return sum(values);
                ;}"
    ;}
    ;END
    (clouchdb:ps-view ("submitted-questions")
      (defun map (doc)
        (with-slots (type question submission) doc
          (if (and type (= type "submitted-question"))
              (emit question 1)))))
    (clouchdb:ps-view ("submitted-answers")
      (defun map (doc)
        (with-slots (type answer) doc
          (if (and type (= type "submitted-answer"))
            (emit answer 1))))
      (defun reduce (keys values)
        (sum values)))
    (clouchdb:ps-view ("submitted-answers-by-question")
      (defun map (doc)
        (with-slots (type question) doc
          (if (and type (= type "submitted-answer"))
              (emit question 1))))
      (defun reduce (keys values)
        (sum values)))
    (clouchdb:ps-view ("submitted-answers-by-submission-question")
      (defun map (doc)
        (with-slots (type submission question) doc
          (if (and type (= type "submitted-answer"))
            (emit (array submission question) nil)))))
    (clouchdb:ps-view ("submitted-comments-by-form")
      (defun map (doc)
        (with-slots (type form) doc
          (if (and type (= type "submitted-comments"))
            (emit form 1))))
      (defun reduce (keys values)
        (sum values)))))

(defun %%delete-design-documents ()
  (clouchdb:delete-view "forms")
  (clouchdb:delete-view "questions")
  (clouchdb:delete-view "answers")
  (clouchdb:delete-view "submissions"))

(defun %%recreate-design-documents ()
  (%%delete-design-documents)
  (%%create-design-documents))

(defun %%delete-all-documents ()
  (flet ((delete-document (doc)
                          (clouchdb:delete-document doc :if-missing :ignore)))
    (mapc #'delete-document
          (clouchdb:query-document
            '(:|rows| :|id|)
            (clouchdb:invoke-view "answers" "all-answers")))
    (mapc #'delete-document
          (clouchdb:query-document
            '(:|rows| :|id|)
            (clouchdb:invoke-view "questions" "all-questions")))
    (mapc #'delete-document
          (clouchdb:query-document
            '(:|rows| :|id|)
            (clouchdb:invoke-view "forms" "active-forms")))
    (mapc #'delete-document
          (clouchdb:query-document
            '(:|rows| :|id|)
            (clouchdb:invoke-view "forms" "inactive-forms")))
    (mapc #'delete-document
          (clouchdb:query-document
            '(:|rows| :|id|)
            (clouchdb:invoke-view "forms" "fresh-forms")))
    (mapc #'delete-document
          (clouchdb:query-document
            '(:|rows| :|id|)
            (clouchdb:invoke-view "submissions" "submissions-by-form"
                                  :reduce nil)))
    (mapc #'delete-document
          (clouchdb:query-document
           '(:|rows| :|id|)
           (clouchdb:invoke-view "submissions" "submitted-questions")))
    (mapc #'delete-document
          (clouchdb:query-document
            '(:|rows| :|id|)
            (clouchdb:invoke-view "submissions" "submitted-answers"
                                  :reduce nil))))
  (values))