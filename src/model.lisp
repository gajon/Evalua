(in-package #:evalua)

(defclass form ()
  ((_id         :initarg :id          :accessor form-id)
   (_rev        :initarg :rev         :accessor form-rev)
   (user        :initarg :user        :accessor form-user)
   (date        :initarg :date        :accessor form-date)
   (update-date :initarg :update-date :accessor form-update-date)
   (start-date  :initarg :start-date  :accessor form-start-date :initform nil)
   (valid-date  :initarg :valid-date  :accessor form-valid-date :initform nil)
   (public-id   :initarg :public-id   :accessor form-public-id)
   (title       :initarg :title       :accessor form-title)
   (notes       :initarg :notes       :accessor form-notes)
   (time-zone   :initarg :time-zone   :accessor form-time-zone)
   (status      :initarg :status      :accessor form-status)
   (time-limit  :initarg :time-limit  :accessor form-time-limit  :initform nil)
   (tries-limit :initarg :tries-limit :accessor form-tries-limit :initform nil)
   (score-p     :initarg :score-p     :accessor form-score-p     :initform nil)
   (comments-p  :initarg :comments-p  :accessor form-comments-p  :initform nil)
   (email-dest  :initarg :email-dest  :accessor form-email-dest  :initform nil)
   (cached-question-objs :initform nil))
  (:documentation ""))

(defclass question ()
  ((_id     :initarg :id       :reader question-id)
   (_rev    :initarg :rev      :reader question-rev)
   (sort    :initarg :sort     :reader question-sort)
   (control :initarg :control  :reader question-control)
   (text    :initarg :text     :reader question-text)
   (valid-p :initform t        :accessor question-valid-p)
   (form                       :accessor question-form)
   (cached-answer-objs :initform nil))
  (:documentation ""))

(defclass answer ()
  ((_id     :initarg :id      :reader answer-id)
   (_rev    :initarg :rev     :reader answer-rev)
   (sort    :initarg :sort    :reader answer-sort)
   (control :initarg :control :reader answer-control)
   (text    :initarg :text    :reader answer-text)))

(defmethod print-object ((form form) stream)
  (print-unreadable-object (form stream :identity t :type t)
    (format stream "~a ~a Updated:~a"
            (form-id form)
            (form-status form)
            (format-date (form-update-date form)))))


;;; When we ask a date from a FORM object we want to get a DATE object
;;; (see utils.lisp), but when we want to persist a FORM object (using
;;; CouchDB in this case), we need to save the dates as strings using
;;; ISO 8601 format, e.g. "1997-07-16T19:20:30-05:00"
;;;
;;; What we do is we keep a string representation of the dates at all
;;; times and convert them to DATE objects only when they are
;;; accessed.
;;;
;;; We specialize the accessors to date slots to achieve an almost
;;; transparent conversion from strings in iso 8601 to DATE
;;; objects. We also specialize the :AFTER method of
;;; INITIALIZE-INSTANCE so that when we manually create a FORM object
;;; with DATE objects in its slots, they get converted to strings.

(defmethod initialize-instance :after ((form form) &key)
  (let ((date (and (slot-boundp form 'date)
                   (slot-value form 'date)))
        (update-date (and (slot-boundp form 'update-date)
                          (slot-value form 'update-date)))
        (start-date (and (slot-boundp form 'start-date)
                         (slot-value form 'start-date)))
        (valid-date (and (slot-boundp form 'valid-date)
                         (slot-value form 'valid-date))))
    (when (and date (eq (type-of date) 'date))
      (setf (form-date form) date))
    (when (and update-date (eq (type-of update-date) 'date))
      (setf (form-update-date form) update-date))
    (when (and start-date (eq (type-of start-date) 'date))
      (setf (form-start-date form) start-date))
    (when (and valid-date (eq (type-of valid-date) 'date))
      (setf (form-valid-date form) valid-date))))

(defmethod form-date ((form form))
  (let-when (date (and (slot-boundp form 'date) (slot-value form 'date)))
    (parse-iso8601-date date)))

(defmethod form-update-date ((form form))
  (let-when (date (and (slot-boundp form 'update-date)
                       (slot-value form 'update-date)))
    (parse-iso8601-date date)))

(defmethod form-start-date ((form form))
  (let-when (date (and (slot-boundp form 'start-date)
                       (slot-value form 'start-date)))
    (parse-iso8601-date date)))

(defmethod form-valid-date ((form form))
  (let-when (date (and (slot-boundp form 'valid-date)
                       (slot-value form 'valid-date)))
    (parse-iso8601-date date)))

(defmethod (setf form-date) :after ((date date) (form form))
  (when date
    (setf (slot-value form 'date) (format-iso8601-date date))))

(defmethod (setf form-update-date) :after ((date date) (form form))
  (when date
    (setf (slot-value form 'update-date) (format-iso8601-date date))))

(defmethod (setf form-start-date) :after ((date date) (form form))
  (when date
    (setf (slot-value form 'start-date) (format-iso8601-date date))))

(defmethod (setf form-valid-date) :after ((date date) (form form))
  (when date
    (setf (slot-value form 'valid-date) (format-iso8601-date date))))


;;; For the moment, we are instantiating new FORM objects on each HTTP
;;; request, and we can safely assume that for most operations the set
;;; of questions in a FORM does not change. To avoid talking to the
;;; database too much we cache the list of QUESTIONs of a
;;; FORM. Likewise with the ANSWERs of a QUESTION.

(defgeneric form-questions (form)
  (:documentation "")
  (:method ((form form))
    (with-slots ((cached cached-question-objs)) form
      (if cached
        cached
        (setf cached
              (data/get-questions-by-form form))))))

(defgeneric question-answers (question)
  (:documentation "")
  (:method ((question question))
    (with-slots ((cached cached-answer-objs) _id) question
      (if cached
        cached
        (setf cached
              (data/get-answers-by-question _id))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; USER
;;; The `username` is the id of the document.
(defclass user ()
  ((_rev            :initarg :rev             :accessor user-rev)
   (full-name       :initarg :full-name       :accessor user-full-name)
   (username        :initarg :username        :accessor user-username)
   (password-digest :initarg :password-digest :accessor user-password-digest)
   (email           :initarg :email           :accessor user-email)
   (time-zone       :initarg :time-zone       :accessor user-time-zone))
  (:documentation ""))

(defmethod print-object ((u user) stream)
  (print-unreadable-object (u stream :identity t :type t)
    (format stream "~a (~a)" (user-username u) (user-full-name u))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUBMISSIONS

(defclass submission ()
  ((_id         :initarg :id          :accessor submission-id)
   (_rev        :initarg :rev         :accessor submission-rev)
   (form        :initarg :form        :accessor submission-form)
   (start-date  :initarg :start-date  :accessor submission-start-date)
   (finish-date :initarg :finish-date :accessor submission-finish-date)
   (ip          :initarg :ip          :accessor submission-ip)
   (user-agent  :initarg :user-agent  :accessor submission-user-agent))
  (:documentation ""))

(defmethod print-object ((submission submission) stream)
  (print-unreadable-object (submission stream :identity t :type t)
    (format stream "~a ~a"
            (submission-id submission)
            (format-date (submission-finish-date submission)))))

;;; Just like with FORM objects, a SUBMISSION object contains dates in
;;; its slots, and we want to keep a string representation of the date
;;; in ISO 8601 format at all times, converting them to a DATE object
;;; only when we want to access its slot.

(defmethod initialize-instance :after ((sub submission) &key)
  (let ((start-date (and (slot-boundp sub 'start-date)
                         (slot-value sub 'start-date)))
        (finish-date (and (slot-boundp sub 'finish-date)
                          (slot-value sub 'finish-date))))
    (when (and start-date (eq (type-of start-date) 'date))
      (setf (submission-start-date sub) start-date))
    (when (and finish-date (eq (type-of finish-date) 'date))
      (setf (submission-finish-date sub) finish-date))))

(defmethod submission-start-date ((submission submission))
  (let-when (start-date (and (slot-boundp submission 'start-date)
                             (slot-value submission 'start-date)))
    (parse-iso8601-date start-date)))

(defmethod submission-finish-date ((submission submission))
  (let-when (finish-date (and (slot-boundp submission 'finish-date)
                              (slot-value submission 'finish-date)))
    (parse-iso8601-date finish-date)))

(defmethod (setf submission-start-date) :after ((date date) (sub submission))
  (when date
    (setf (slot-value sub 'start-date) (format-iso8601-date date))))

(defmethod (setf submission-finish-date) :after ((date date) (sub submission))
  (when date
    (setf (slot-value sub 'finish-date) (format-iso8601-date date))))
