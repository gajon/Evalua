(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RENDER A PUBLIC FORM.

(define-url-fn (public/a :prefix "a" :auth nil)
  (let* ((form (or (data/get-form-by-public-id (parameter "id"))
                   (redirect "/")))
         (start-time (or (session-value 'start-time)
                         (setf (session-value 'start-time)
                               (get-universal-time))))
         (questions (form-questions form)))
    (awhen (and (eql :post (request-method*))
                (public%process-submitted-answers form questions start-time))
      (redirect (format nil "/thankyou?id=~a&sub=~a"
                        (form-public-id form)
                        (submission-id it))))
    (standard-page (:title (form-title form) :show-banner nil :show-footer nil
                           :css-files ("public-styles.css?v=20101209"))
      (:form :method "post" :action "/a"
             (hidden-input "id" :default-value (form-public-id form))
             (:section :id "form-title"
                       (:header (:h1 (esc (form-title form))))
                       (:div :id "form-notes" (esc (form-notes form))))
             (:section :id "questions"
                       (show-all-messages)
                       (dolist (question questions)
                         (public%display-question question)))
             (:section :id "submit" (submit-button "Enviar respuestas"))))))

(defun public%display-question (question)
  (with-html-output (*standard-output*)
    (:div :class "question"
          (:h2 (esc (format nil "~d. ~a"
                            (question-sort question)
                            (question-text question))))
          (unless (question-valid-p question)
            (htm (:span :class "invalid" "Por favor contesta la pregunta.")))
          (:div :class "answers"
                (dolist (answer (question-answers question))
                  (public%display-answer answer question))))))

(defun public%display-answer (answer question)
  (with-html-output (*standard-output*)
    (:div :class "answer"
      (cond ((or (string= (answer-control answer) "radio-choice")
                 (string= (answer-control answer) "true-false"))
             (radio-choice (answer-text answer) (question-id question)
                           (answer-id answer)))
            ((string= (answer-control answer) "checkbox")
             (checkbox-choice (answer-text answer) (question-id question)
                              (answer-id answer)))
            ((string= (answer-control answer) "textarea")
             (hidden-input
               (escape-string (question-id question))
               :default-value (format nil "wrap-~a"
                                      (escape-string (answer-id answer))))
             (text-area nil (escape-string (answer-id answer))))))))

(defun public%process-submitted-answers (form questions start-universal-time)
  "Traverses the list of questions, validating that each one of them has a
valid answer sent in the POST data. If all of them are valid, they are saved
in the database; otherwise each invalid question is marked and this function
returns NIL."
  (let ((ht (make-hash-table :test 'equal)))
    ;; We collect all the POST values sent into a hash table. The keys are
    ;; the _id's of the questions and the values are the _id's of the
    ;; answers. When the answer is a text input type, the value is the _id
    ;; of the answer but with the letters 'wrap-' prepended to it; with this
    ;; we expect to find another POST key-value pair whose key is the _id of
    ;; the answer and the value is the text the user entered.
    ;;
    ;; Usually there's only one answer per question, but in the case of
    ;; questions with checkboxes we get more than one answer per question.
    ;; In other words, the POST data might come as:
    ;;
    ;; &idQuestion1=idAnswer1
    ;; &idQuestion2=wrap-idAnswer2
    ;; &idQuestion3=idAnswer4
    ;; &idQuestion3=idAnswer5
    ;; &idAnswer2=Text entered by the user.
    ;;
    ;; In our hash table we'll have:
    ;; idQuestion1 => (idAnswer1)
    ;; idQuestion2 => (wrap-idAnswer2)
    ;; idQuestion3 => (idAnswer5 idAnswer4)
    ;; idAnswer2   => (Text entered by the user)
    (loop for (key . value) in (post-parameters*)
          do (push value (gethash key ht)))
    ;; If we REQUIRE SCORING, we'll save the information only when every
    ;; question has a valid answer. Otherwise we return NIL and let the caller
    ;; deal with it.
    ;;
    ;; If the form does not require scoring we just save what we have, we don't
    ;; care if the user did not answer all the questions.
    ;; TODO: What if the user did not send any single answer?
    (if (or (not (form-score-p form))
            (loop with success = t
                  for question in questions
                  for valid = (public%validate-question question ht)
                  unless valid do (setf (question-valid-p question) nil
                                        success nil)
                    finally (return success)))
        ;; All questions valid, or nor scoring required.
        (let* ((time-zone (form-time-zone form))
               ;; TODO (I love it when I do this, write just TODO as if I'm gonna remember what)
               (start-date (make-date start-universal-time time-zone))
               (finish-date (make-date (get-universal-time) time-zone))
               (sub (data/create-submission
                     (make-instance 'submission
                                    :form (form-id form)
                                    :start-date start-date
                                    :finish-date finish-date
                                    :ip (remote-addr*)
                                    :user-agent (user-agent)))))
          (public%save-submitted-questions sub questions ht))
        ;; Not all questions answered.
        (push-error-msg "Por favor contesta todas las preguntas."))))

(defun public%validate-question (question ht-submitted-answers)
  (let ((answers (gethash (question-id question) ht-submitted-answers)))
    (unless answers
      (return-from public%validate-question nil))
    (dolist (answer answers)
      (let ((answer (trim-or-nil answer)))
        (cond ((or (null answer) (< (length answer) 6))
               (return-from public%validate-question nil))
              ((string= "wrap-" answer :end1 5 :end2 5)
               (let ((wrapped (gethash (subseq answer 5) ht-submitted-answers)))
                 (or (and wrapped (trim-or-nil (car wrapped)))
                     (return-from public%validate-question nil))))))))
  t)

(defun public%save-submitted-questions (submission questions answers)
  (loop with submission-id = (submission-id submission)
        for question in questions
        for question-id = (question-id question)
        for submitted-answers = (gethash question-id answers)
        when submitted-answers do
          (data/add-submitted-question
           submission-id question-id
           (loop for answer in submitted-answers
                 for wrap? = (string= "wrap-" answer :end2 5)
                 for answer-id = (or (and wrap? (subseq answer 5)) answer)
                 collect
              (list
                (cons "answer" answer-id)
                (cons "value"
                      (and wrap? (car (gethash answer-id answers))))))))
  submission)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(define-url-fn (public/thankyou :prefix "thankyou" :auth nil)
  (let ((form (or (data/get-form-by-public-id (parameter "id"))
                  (redirect "/")))
        (sub (or (data/get-submission (parameter "sub"))
                 (redirect "/"))))
    ;; TODO: Do we really want to redirect to "/" on errors?
    (unless (string= (form-id form) (submission-form sub))
      (redirect "/"))
    (awhen (and (eq :post (request-method*))
                (form-comments-p form)
                (trim-or-nil (post-parameter "comments")))
      (data/add-submitted-comment (submission-id sub) it)
      (push-success-msg "Tus comentarios se han enviado, gracias")
      (redirect (format nil "/thankyou?id=~a&sub=~a"
                        (form-public-id form)
                        (submission-id sub))))
    ;;
    (standard-page (:title "Gracias"
                    :show-banner nil
                    :show-footer nil
                    :css-files ("public-styles.css?v=20101209"))
      (:section :id "form-title"
        (:header (:h1 (esc (form-title form)))))
      (:section :id "thankyou"
        (:h2 "Gracias por contestar la evaluación.")
        (show-all-messages))
      (when (form-comments-p form)
        (htm
         (:form :method "post" :action "/thankyou"
          (hidden-input "id" :default-value (form-public-id form))
          (hidden-input "sub" :default-value (submission-id sub))
          (:section :id "comments"
           (text-area
            "Puedes dejar comentarios adicionales (opcional):"
            "comments")
           (:div :class "button"
            (submit-button "Enviar")))))))))
