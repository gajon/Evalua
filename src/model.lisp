(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASSES

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
   (valid-p :initform t :accessor question-valid-p)
   (cached-answer-objs :initform nil))
  (:documentation ""))

(defclass answer ()
  ((_id     :initarg :id      :reader answer-id)
   (_rev    :initarg :rev     :reader answer-rev)
   (sort    :initarg :sort    :reader answer-sort)
   (control :initarg :control :reader answer-control)
   (text    :initarg :text    :reader answer-text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

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

;;; FORM-DATE
(defmethod form-date ((form form))
  (let ((date (and (slot-boundp form 'date) (slot-value form 'date))))
    (when date
      (parse-iso8601-date date))))

(defmethod (setf form-date) :after ((date date) (form form))
  (when date
    (setf (slot-value form 'date) (format-iso8601-date date))))

;;; FORM-UPDATE-DATE
(defmethod form-update-date ((form form))
  (let ((date (and (slot-boundp form 'update-date)
                   (slot-value form 'update-date))))
    (when date
      (parse-iso8601-date date))))

(defmethod (setf form-update-date) :after ((date date) (form form))
  (when date
    (setf (slot-value form 'update-date) (format-iso8601-date date))))

;;; FORM-START-DATE
(defmethod form-start-date ((form form))
  (let ((date (and (slot-boundp form 'start-date)
                   (slot-value form 'start-date))))
    (when date
      (parse-iso8601-date date))))

(defmethod (setf form-start-date) :after ((date date) (form form))
  (when date
    (setf (slot-value form 'start-date) (format-iso8601-date date))))

;;; FORM-VALID-DATE
(defmethod form-valid-date ((form form))
  (let ((date (and (slot-boundp form 'valid-date)
                   (slot-value form 'valid-date))))
    (when date
      (parse-iso8601-date date))))

(defmethod (setf form-valid-date) :after ((date date) (form form))
  (when date
    (setf (slot-value form 'valid-date) (format-iso8601-date date))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defgeneric form-questions (form)
  (:documentation "")
  (:method ((form form))
    (with-slots ((cached cached-question-objs) _id) form
      (if cached
        cached
        (setf cached
              (data/get-questions-by-form _id))))))

(defgeneric question-answers (question)
  (:documentation "")
  (:method ((question question))
    (with-slots ((cached cached-answer-objs) _id) question
      (if cached
        cached
        (setf cached
              (data/get-answers-by-question _id))))))


;;; For the USER, the `username` is the id of the document.
(defclass user ()
  ((_rev            :initarg :rev             :accessor user-rev)
   (full-name       :initarg :full-name       :accessor user-full-name)
   (username        :initarg :username        :accessor user-username)
   (password-digest :initarg :password-digest :accessor user-password-digest)
   (email           :initarg :email           :accessor user-email)
   (time-zone       :initarg :time-zone       :accessor user-time-zone))
  (:documentation ""))


(defclass submission ()
  ((_id         :initarg :id          :accessor submission-id)
   (_rev        :initarg :rev         :accessor submission-rev)
   (form        :initarg :form        :accessor submission-form)
   (start-date  :initarg :start-date  :accessor submission-start-date)
   (finish-date :initarg :finish-date :accessor submission-finish-date)
   (ip          :initarg :ip          :accessor submission-ip)
   (user-agent  :initarg :user-agent  :accessor submission-user-agent))
  (:documentation ""))

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
  (let ((start-date (and (slot-boundp submission 'start-date)
                         (slot-value submission 'start-date))))
    (when start-date
      (parse-iso8601-date start-date))))

(defmethod (setf submission-start-date) :after ((date date) (sub submission))
  (when date
    (setf (slot-value sub 'start-date) (format-iso8601-date date))))

(defmethod submission-finish-date ((submission submission))
  (let ((finish-date (and (slot-boundp submission 'finish-date)
                          (slot-value submission 'finish-date))))
    (when finish-date
      (parse-iso8601-date finish-date))))

(defmethod (setf submission-finish-date) :after ((date date) (sub submission))
  (when date
    (setf (slot-value sub 'finish-date) (format-iso8601-date date))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods to get a nice REPL display.

(defmethod print-object ((u user) stream)
  (print-unreadable-object (u stream :identity t :type t)
    (format stream "~a (~a)" (user-username u) (user-full-name u))))

