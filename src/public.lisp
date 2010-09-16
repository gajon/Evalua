(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RENDER A PUBLIC FORM.

(define-url-fn (a :prefix "a")
  (let ((form (or (data/get-form-by-public-id (parameter "id"))
                  (redirect "/"))))
    ;;
    ;;
    (when (and (eql :post (request-method*))
               (process-submitted-answers form))
      (redirect (format nil "/thankyou?id=~a" (form-public-id form))))
    ;;
    ;;
    (standard-page (:title (form-title form)
                    :show-banner nil
                    :css-files ("public-styles.css"))
      (:form :method "post" :action "/a"
        (hidden-input "id" :default-value (form-public-id form))
        ;;
        ;; Title and notes.
        ;;
        (:section :id "form-title"
          (:header (:h1 (esc (form-title form))))
          (:div :id "form-notes" (esc (form-notes form))))
        ;;
        ;; Questions.
        ;;
        (:section :id "questions"
          (show-all-messages)
          (dolist (question (form-questions form))
            (htm (:div :class "question"
                   (:h2 (esc (format nil "~d. ~a"
                                     (question-sort question)
                                     (question-text question))))
                   (unless (question-valid-p question)
                     (htm (:span :class "invalid"
                                 "Por favor contesta la pregunta.")))
                   (:div :class "answers"
                     (dolist (answer (question-answers question))
                       (display-answer answer question)))))))
        ;;
        ;; Submit button
        ;;
        (:section :id "submit"
          (submit-button "Enviar respuestas"))))))

(defun display-answer (answer question)
  (with-html-output (*standard-output*)
    (:div :class "answer"
      (cond ((or (string= (answer-control answer) "radio-choice")
                 (string= (answer-control answer) "true-false"))
             (htm (:input :type "radio"
                          :name (escape-string (question-id question))
                          :id (escape-string
                                (format nil "id_~a" (answer-sort answer)))
                          :value (escape-string (answer-id answer)))
                  (:label :for (escape-string
                                 (format nil "id_~a"
                                         (answer-sort answer)))
                          (esc (answer-text answer)))))
            ;;
            ;;
            ((string= (answer-control answer) "checkbox")
             (htm (:input :type "checkbox"
                          :name (escape-string (question-id question))
                          :id (escape-string
                                (format nil "id_~a" (answer-sort answer)))
                          :value (escape-string (answer-id answer)))
                  (:label :for (escape-string
                                 (format nil "id_~a" (answer-sort answer)))
                          (esc (answer-text answer)))))
            ;;
            ;;
            ((string= (answer-control answer) "textarea")
             (hidden-input
               (escape-string (question-id question))
               :default-value (format nil "wrap-~a"
                                      (escape-string (answer-id answer))))
             (text-area nil (escape-string (answer-id answer))))))))

(defun process-submitted-answers (form)
  (let ((questions (form-questions form))
        (ht (make-hash-table :test 'equal)))
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
    ;; Now we iterate over the questions defined by our form and consult the
    ;; submitted answers from the hash table. We'll save the information
    ;; only when every question has a valid answer. Otherwise we return NIL
    ;; and let the caller deal with it.
    (if (loop with success = t
              for question in questions
              for valid = (validate-question question ht)
              unless valid do (setf (question-valid-p question) nil
                                    success nil)
              finally (return success))
      (save-submitted-answers questions ht (form-time-zone form))
      (push-error-msg "Por favor contesta todas las preguntas."))))

(defun validate-question (question ht-submitted-answers)
  (let ((answers (gethash (question-id question) ht-submitted-answers)))
    (unless answers
      (return-from validate-question nil))
    (dolist (answer answers)
      (let ((answer (trim-or-nil answer)))
        (cond ((or (null answer) (< (length answer) 6))
               (return-from validate-question nil))
              ((string= "wrap-" answer :end1 5 :end2 5)
               (let ((wrapped (gethash (subseq answer 5) ht-submitted-answers)))
                 (or (and wrapped (trim-or-nil (car wrapped)))
                     (return-from validate-question nil))))))))
  t)

(defun save-submitted-answers (questions ht time-zone)
  ;; We use the time-zone recorded in the form, which is the time-zone of
  ;; the user who designed and created the form. That user is the one who
  ;; will see any dates, therefore we'd like to show them in his/her
  ;; time-zone.
  (let ((now (make-date (get-universal-time) (or time-zone 6))))
    (loop for question in questions
          for qid = (question-id question)
          for answers = (gethash qid ht)
          do (loop for answer in answers
                   for wrap? = (string= "wrap-" answer :end1 5 :end2 5)
                   for ansid = (or (and wrap? (subseq answer 5)) answer)
                   do (add-submitted-answer
                        (question-id question)
                        ansid
                        (and wrap? (car (gethash ansid ht)))
                        now))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(define-url-fn (thankyou :prefix "thankyou")
  (let ((form (or (data/get-form-by-public-id (parameter "id"))
                  (redirect "/"))))
    (standard-page (:title "Gracias"
                    :show-banner nil
                    :css-files ("public-styles.css"))
      (:section :id "form-title"
        (:header (:h1 (esc (form-title form)))))
      (:section :id "thankyou"
        (:h2 "Gracias por contestar el cuestionario.")))))
