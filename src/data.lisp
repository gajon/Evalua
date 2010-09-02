(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASSES

(defclass form ()
  ((_id        :initarg :id         :reader   form-id)
   (_rev       :initarg :rev        :reader   form-rev)
   (user       :initarg :user       :accessor form-user)
   (date       :initarg :date       :accessor form-date)
   (valid-date :initarg :valid-date :accessor form-valid-date)
   (public-id  :initarg :public-id  :accessor form-public-id)
   (title      :initarg :title      :accessor form-title)
   (notes      :initarg :notes      :accessor form-notes)
   (time-zone  :initarg :time-zone  :accessor form-time-zone)
   (cached-question-objs :initform nil))
  (:documentation ""))

(defclass question ()
  ((_id     :initarg :id       :reader question-id)
   (_rev    :initarg :rev      :reader question-rev)
   (sort    :initarg :sort     :reader question-sort)
   (text    :initarg :text     :reader question-text)
   (valid-p :initform t :accessor question-valid-p)
   (cached-answer-objs :initform nil))
  (:documentation ""))

(defclass answer ()
  ((_id     :initarg :id      :reader answer-id)
   (_rev    :initarg :rev     :reader answer-rev)
   (sort    :initarg :sort    :reader answer-sort)
   (control :initarg :control :reader answer-control)
   (text    :initarg :text    :reader answer-text)))


(defgeneric form-questions (form)
  (:documentation "")
  (:method ((form form))
    (with-slots ((cached cached-question-objs) _id) form
      (if cached
        cached
        (setf cached
              (get-questions-by-form _id))))))

(defgeneric question-answers (question)
  (:documentation "")
  (:method ((question question))
    (with-slots ((cached cached-answer-objs) _id) question
      (if cached
        cached
        (setf cached
              (get-answers-by-question _id))))))

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


(defun get-form (id)
  (handler-case
    (let ((alist (clouchdb:get-document id)))
      (build-form-from-alist alist))
    (error () nil)))

(defun get-form-by-public-id (public-id)
  (let ((data 
          (clouchdb:query-document
            `(:|rows| :|id| ,#'clouchdb:get-document)
            (clouchdb:invoke-view "public-forms" "all-forms" :key public-id))))
    (when data
      (build-form-from-alist (car data)))))


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
  (mapcar #'build-question-from-alist
          (nreverse
            (clouchdb:query-document
              `(:|rows| :|id| ,#'clouchdb:get-document)
              (clouchdb:invoke-view "questions" "questions-by-form"
                                    :start-key (list form-id)
                                    :end-key (list form-id
                                                   (make-hash-table)))))))

(defun get-answers-by-question (question-id)
  (mapcar (lambda (alist) (build-answer-from-alist alist))
          (nreverse
            (clouchdb:query-document
              `(:|rows| :|id| ,#'clouchdb:get-document)
              (clouchdb:invoke-view "answers" "answers-by-question"
                                    :start-key (list question-id)
                                    :end-key (list question-id
                                                   (make-hash-table)))))))


(defun build-form-from-alist (alist)
  (make-instance 'form
                 :id         (%lowassoc _id  alist)
                 :rev        (%lowassoc _rev alist)
                 :user       (%lowassoc user alist)
                 :date       (%lowassoc date alist)
                 :valid-date (%lowassoc valid-date alist)
                 :public-id  (%lowassoc public-id  alist)
                 :title      (%lowassoc title alist)
                 :notes      (%lowassoc notes alist)
                 :time-zone  (%lowassoc time-zone alist)))

(defun build-question-from-alist (alist)
  (make-instance 'question
                 :id   (%lowassoc _id alist)
                 :rev  (%lowassoc _rev alist)
                 :text (%lowassoc text alist)
                 :sort (%lowassoc sort alist)))

(defun build-answer-from-alist (alist)
  (make-instance 'answer
                 :id      (%lowassoc _id alist)
                 :rev     (%lowassoc _rev alist)
                 :sort    (%lowassoc sort alist)
                 :control (%lowassoc control alist)
                 :text    (%lowassoc text alist)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA STORAGE

(defun add-form (form-obj questions)
  ;; TODO make sure secret-key and public-url doesn't already exist
  (let* ((secret-id (kmrcl:random-string :set :lower-alphanumeric :length 20))
         (public-id (kmrcl:random-string :set :lower-alphanumeric :length 10))
         ;; We'll first create the 'form' document which only contains the
         ;; basic info of the form.
         (form (clouchdb:create-document
                 `((:|type| .       "form")
                   (:|user| .       ,(form-user form-obj))
                   (:|date| .       ,(form-date form-obj))
                   (:|valid-date| . ,(form-valid-date form-obj))
                   (:|public-id| .  ,public-id)
                   (:|title| .      ,(form-title form-obj))
                   (:|notes| .      ,(form-notes form-obj))
                   (:|time-zone| .  ,(form-time-zone form-obj)))
                 :id secret-id)))
    ;; TODO: :|id| must be equal to secret-id, should we assert this?
    (when (assoc :|id| form)
      ;; Now we'll create the 'question' and 'answer' documents, the
      ;; questions link back to the form using the form's id, and
      ;; similarly the answers link back to each question using the
      ;; question's id.
      (labels ((save-question (alist form-id)
                 (let ((q (clouchdb:create-document
                            `((:|type| . "question")
                              (:|sort| . ,(%lowassoc question-number alist))
                              (:|text| . ,(%lowassoc text alist))
                              (:|form| . ,form-id)))))
                   ;; TODO: error handling?
                   (when (assoc :|id| q)
                     (mapc #'save-answer
                           (%lowassoc answers alist)
                           (repeatedly (cdr (assoc :|id| q)))))))
               (save-answer (alist question-id)
                 (clouchdb:create-document
                   `((:|type| . "answer")
                     (:|sort| . ,(%lowassoc answer-number alist))
                     (:|control| . ,(%lowassoc control alist))
                     ;; TODO: add this field only when needed?
                     (:|selected| . ,(%lowassoc selected alist))
                     (:|text| . ,(%lowassoc text alist))
                     (:|question| . ,question-id)))))
               ;(save-answer-old (alist question-id)
                 ;(if (member (%lowassoc control alist)
                             ;'("textarea")
                             ;:test #'string=)
                   ;;; When the answer is of a "text type", it is created as
                   ;;; a 'sub-answer' document and it links back to a new
                   ;;; 'answer' document (holder) that will help us identify
                   ;;; it as such. TODO: This doesn't explain things well.
                   ;(let ((holder
                           ;(save-answer-helper (%lowassoc answer-number alist)
                                               ;(format nil "~a-holder"
                                                       ;(%lowassoc control alist))
                                               ;(%lowassoc text alist)
                                               ;question-id)))
                     ;(when (assoc :|id| holder)
                       ;(clouchdb:create-document
                         ;`((:|type| . "sub-answer")
                           ;(:|control| . ,(%lowassoc control alist))
                           ;(:|answer| . ,(cdr (assoc :|id| holder)))))))
                   ;;; Otherwise
                   ;(save-answer-helper (%lowassoc answer-number alist)
                                       ;(%lowassoc control alist)
                                       ;(%lowassoc text alist)
                                       ;question-id)))
               ;(save-answer-helper (answer-number control text question-id)
                 ;(clouchdb:create-document
                   ;`((:|type| . "answer")
                     ;(:|sort| . ,answer-number)
                     ;(:|control| . ,control)
                     ;(:|text| . ,text)
                     ;(:|question| . ,question-id)))))
        (mapc #'save-question
              questions
              (repeatedly secret-id)))
      ;;
      ;; Set the id and rev to the form object and return it.
      ;;
      (setf (slot-value form-obj '_id) secret-id
            (slot-value form-obj '_rev) secret-id)
      form-obj)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES TO SETUP THE COUCHDB DATABASE, MAINLY THE
;;; DESIGN VIEWS.

(defun %%create-design-documents ()
  (clouchdb:create-ps-view "public-forms"
    #>END
    "all-forms": {
      "map": "function (doc) {
                if (doc.type === 'form') {
                  emit(doc['public-id'], null);
                }
              }"
    }
    END)
  (clouchdb:create-ps-view "questions"
    #>END
    "all-questions": {
      "map": "function (doc) {
                if (doc.type === 'question') {
                  emit(null, null);
                }
              }"
    },
    "questions-by-form": {
      "map": "function (doc) {
                if (doc.type === 'question') {
                  emit([doc.form, doc.sort, doc._id], null);
                }
              }"
    }
    END)
  (clouchdb:create-ps-view "answers"
    #>END
    "all-answers": {
      "map": "function (doc) {
                if (doc.type === 'answer' || doc.type === 'sub-answer') {
                  emit(null, null);
                }
              }"
    },
    "answers-by-question": {
      "map": "function (doc) {
                if (doc.type === 'answer') {
                  emit([doc.question, doc.sort, doc._id], null);
                }
              }"
    },
    "submitted-answers": {
      "map": "function (doc) {
                if (doc.type === 'submitted-answer') {
                  emit(doc.question, null);
                }
              }"
    }
    END))

(defun %%delete-design-documents ()
  (clouchdb:delete-view "public-forms")
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
