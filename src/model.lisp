(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASSES

(defclass form ()
  ((_id         :initarg :id          :accessor form-id)
   (_rev        :initarg :rev         :accessor form-rev)
   (user        :initarg :user        :accessor form-user)
   (date        :initarg :date        :accessor form-date)
   (update-date :initarg :update-date :accessor form-update-date)
   (valid-date  :initarg :valid-date  :accessor form-valid-date)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods to get a nice REPL display.

(defmethod print-object ((u user) stream)
  (print-unreadable-object (u stream :identity t :type t)
    (format stream "~a (~a)" (user-username u) (user-full-name u))))

