(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA RETRIEVAL

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


(defun data/get-form (id)
  (handler-case
    (let ((alist (clouchdb:get-document id)))
      (data/build-form-from-alist alist))
    (error () nil)))

(defun data/get-form-by-public-id (public-id)
  (let ((data
          (clouchdb:query-document
            `(:|rows| :|id| ,#'clouchdb:get-document)
            (clouchdb:invoke-view "public-forms" "all-forms" :key public-id))))
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


(defun get-questions-by-form (form-id)
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
  (mapcar #'data/build-question-from-alist
          (nreverse
            (clouchdb:query-document
              `(:|rows| :|id| ,#'clouchdb:get-document)
              (clouchdb:invoke-view "questions" "questions-by-form"
                                    :start-key (list form-id)
                                    :end-key (list form-id
                                                   (make-hash-table)))))))

(defun get-answers-by-question (question-id)
  (mapcar (lambda (alist) (data/build-answer-from-alist alist))
          (nreverse
            (clouchdb:query-document
              `(:|rows| :|id| ,#'clouchdb:get-document)
              (clouchdb:invoke-view "answers" "answers-by-question"
                                    :start-key (list question-id)
                                    :end-key (list question-id
                                                   (make-hash-table)))))))


(defun data/get-user (username)
  (handler-case (data/build-user-from-alist (clouchdb:get-document username))
    (error () nil)))

(defun data/validate-credentials (username password)
  (handler-case
    (let ((alist (clouchdb:get-document username))
          (digest (hunchentoot::md5-hex password)))
      (string= digest (%lowassoc password alist)))
    (error () nil)))


(defun data/build-form-from-alist (alist)
  (make-instance 'form
                 :id          (%lowassoc _id  alist)
                 :rev         (%lowassoc _rev alist)
                 :user        (%lowassoc user alist)
                 :date        (%lowassoc date alist)
                 :update-date (%lowassoc update-date alist)
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
                 :id   (%lowassoc _id alist)
                 :rev  (%lowassoc _rev alist)
                 :text (%lowassoc text alist)
                 :sort (%lowassoc sort alist)))

(defun data/build-answer-from-alist (alist)
  (make-instance 'answer
                 :id      (%lowassoc _id alist)
                 :rev     (%lowassoc _rev alist)
                 :sort    (%lowassoc sort alist)
                 :control (%lowassoc control alist)
                 :text    (%lowassoc text alist)))

(defun data/build-user-from-alist (alist)
  (make-instance 'user
                 :rev             (%lowassoc '_rev alist)
                 :full-name       (%lowassoc 'full-name alist)
                 :username        (%lowassoc '_id alist)
                 :password-digest (%lowassoc 'password alist)
                 :email           (%lowassoc 'email alist)
                 :time-zone       (%lowassoc 'time-zone alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA STORAGE

(defun data/create-fresh-form (form-obj)
  (flet ((%create-form (form-obj)
           (let* ((secret-id (kmrcl:random-string
                               :set :lower-alphanumeric :length 20))
                  (public-id (kmrcl:random-string
                               :set :lower-alphanumeric :length 10))
                  (doc (clouchdb:create-document
                         `((:|type| .        "form")
                           (:|user| .        ,(form-user form-obj))
                           (:|date| .        ,(form-date form-obj))
                           (:|update-date| . ,(form-update-date form-obj))
                           (:|valid-date| .  ,(form-valid-date form-obj))
                           (:|public-id| .   ,public-id)
                           (:|title| .       ,(form-title form-obj))
                           (:|notes| .       ,(form-notes form-obj))
                           (:|time-zone| .   ,(form-time-zone form-obj))
                           (:|status| .      "fresh")
                           (:|time-limit| .  ,(form-time-limit form-obj))
                           (:|tries-limit| . ,(form-tries-limit form-obj))
                           (:|score-p| .     ,(form-score-p form-obj))
                           (:|comments-p| .  ,(form-comments-p form-obj))
                           (:|email-dest| .  ,(form-email-dest form-obj)))
                         :id secret-id)))
             ;; TODO: Should we check (eql (%lowassoc ok doc) T)?
             (when doc
               (setf (form-id form-obj) secret-id
                     (form-public-id form-obj) public-id
                     (form-rev form-obj) (%lowassoc rev doc)
                     (form-status form-obj) "fresh")
               form-obj))))
    (handler-case
      (%create-form form-obj)
      (clouchdb:id-or-revision-conflict (c)
        (declare (ignore c))
        ;; Try once more.. this should be rare, the random number should
        ;; be sufficiently big so that collisions are rare.
        (ignore-errors
          (%create-form form-obj))))))

(defun data/save-form (form-obj)
  (let* ((now (format-iso8601-date (make-date (get-universal-time)
                                              (or (form-time-zone form-obj) 6))))
         (saved? (clouchdb:put-document
                   `((:|_id| .         ,(form-id form-obj))
                     (:|_rev| .        ,(form-rev form-obj))
                     (:|type| .        "form")
                     (:|user| .        ,(form-user form-obj))
                     (:|date| .        ,(form-date form-obj))
                     (:|update-date| . ,now)
                     (:|valid-date| .  ,(form-valid-date form-obj))
                     (:|public-id| .   ,(form-public-id form-obj))
                     (:|title| .       ,(form-title form-obj))
                     (:|notes| .       ,(form-notes form-obj))
                     (:|time-zone| .   ,(form-time-zone form-obj))
                     (:|status| .      ,(form-status form-obj))
                     (:|time-limit| .  ,(form-time-limit form-obj))
                     (:|tries-limit| . ,(form-tries-limit form-obj))
                     (:|score-p| .     ,(form-score-p form-obj))
                     (:|comments-p| .  ,(form-comments-p form-obj))
                     (:|email-dest| .  ,(form-email-dest form-obj))))))
    (when (%lowassoc ok saved?)
      ;; Update the _rev info, just in case.
      (setf (form-rev form-obj) (%lowassoc rev saved?)
            (form-update-date form-obj) now)
      form-obj)))

(defun data/save-form-questions (form-obj questions)
  ;; The questions link back to the form using the form's id, and similarly
  ;; the answers link back to each question using the question's id.
  (labels ((save-question (alist form-id)
             (let ((q (clouchdb:create-document
                        `((:|type| . "question")
                          (:|sort| . ,(%lowassoc question-number alist))
                          (:|text| . ,(%lowassoc text alist))
                          (:|form| . ,form-id)))))
               ;; TODO: error handling?
               (when (%lowassoc id q)
                 (mapc #'save-answer
                       (%lowassoc answers alist)
                       (repeatedly (%lowassoc id q))))))
           (save-answer (alist question-id)
             (clouchdb:create-document
               `((:|type| . "answer")
                 (:|sort| . ,(%lowassoc answer-number alist))
                 (:|control| . ,(%lowassoc control alist))
                 ;; TODO: add this field only when needed?
                 (:|selected| . ,(%lowassoc selected alist))
                 (:|text| . ,(%lowassoc text alist))
                 (:|question| . ,question-id)))))
    (mapc #'save-question
          questions
          (repeatedly (form-id form-obj)))))

(defun add-submitted-answer (question-id answer-id value now)
  ;; The answer-id is usually a valid _id referencing a document of type
  ;; 'answer'; however, it could be a free text if the question contained a
  ;; textarea box. TODO: I don't like this design choice.
  (clouchdb:create-document
    `((:|type| . "submitted-answer")
      (:|answer| . ,answer-id)
      ,@(when value `((:|value| . ,value)))
      (:|date| . ,(format-iso8601-date now))
      (:|question| . ,question-id))))


(defun data/add-wait-registry (email date user-agent remote-addr)
  (clouchdb:create-document
    `((:|type| . "wait-registry")
      (:|email| . ,email)
      (:|user-agent| . ,user-agent)
      (:|remote-addr| . ,remote-addr)
      (:|date| . ,(format-iso8601-date date)))))


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
;;; UTILITIES TO SETUP THE COUCHDB DATABASE, MAINLY THE
;;; DESIGN VIEWS.

(defun %%create-design-documents ()
  (clouchdb:create-ps-view "public-forms"
    #>END
    "all-forms": {
      "map": "function (doc) {
                if (doc.type && doc.type === 'form') {
                  emit(doc['public-id'], null);
                }
              }"
    }
    END)
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
    #>END
    "all-questions": {
      "map": "function (doc) {
                if (doc.type && doc.type === 'question') {
                  emit(null, null);
                }
              }"
    },
    "questions-by-form": {
      "map": "function (doc) {
                if (doc.type && doc.type === 'question') {
                  emit([doc.form, doc.sort, doc._id], null);
                }
              }"
    }
    END)
  (clouchdb:create-ps-view "answers"
    #>END
    "all-answers": {
      "map": "function (doc) {
                if (doc.type && (doc.type === 'answer' || doc.type === 'sub-answer')) {
                  emit(null, null);
                }
              }"
    },
    "answers-by-question": {
      "map": "function (doc) {
                if (doc.type && doc.type === 'answer') {
                  emit([doc.question, doc.sort, doc._id], null);
                }
              }"
    },
    "submitted-answers": {
      "map": "function (doc) {
                if (doc.type && doc.type === 'submitted-answer') {
                  emit(doc.question, null);
                }
              }"
    }
    END))

(defun %%delete-design-documents ()
  (clouchdb:delete-view "public-forms")
  (clouchdb:delete-view "forms")
  (clouchdb:delete-view "questions")
  (clouchdb:delete-view "answers"))

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
            (clouchdb:invoke-view "answers" "submitted-answers")))
    (mapc #'delete-document
          (clouchdb:query-document
            '(:|rows| :|id|)
            (clouchdb:invoke-view "questions" "all-questions")))
    (mapc #'delete-document
          (clouchdb:query-document
            '(:|rows| :|id|)
            (clouchdb:invoke-view "public-forms" "all-forms"))))
  (values))
