(in-package #:evalua)

(define-url-fn design/create-new-form
  ;; TODO: time-zone
  (let* ((now (make-date (get-universal-time) (or time-zone 6)))
         (fresh-form (data/create-fresh-form
                       (make-instance 'form
                                      :user (user-username the-user)
                                      :date now
                                      :update-date now
                                      :title nil
                                      :notes nil
                                      :time-zone (or time-zone 6)
                                      :email-dest (user-email the-user)))))
    ;; TODO: Error handling?
    (if fresh-form
      (redirect (format nil "/design/edit-form?id=~a" (form-id fresh-form)))
      (redirect "/"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM DESIGN PAGE.

(define-url-fn design/edit-form
  ;; The only utility of this function is to render the initial HTML for the
  ;; JavaScript side to work. And the actual saving of posted information is
  ;; done by the function `backend-save-form` below.
  (let ((form (or (data/get-form (parameter "id")) (redirect "/"))))
    (standard-page (:title "Paso 1. Diseña tu cuestionario."
                    :css-files ("design-styles.css?v=20101019")
                    :js-files ("jquery-1.4.2.min.js"
                               "json2.min.js"
                               "design.js?v=20101007"))
      (hidden-input "id" :default-value (form-id form))
      (:section :id "questions"
        (:h1 "Diseña tu evaluación")
        (:div :id "form-basics"
          (:div :id "form-title"
            (text-input "Asigna un título a la evaluación:" "title"
                        :default-value (form-title form)))
          (:div :id "form-notes"
            (text-area "Indica las instrucciones (opcional):" "notes"
                       :default-value (form-notes form))))
        (:div :id "questions-addarea"))
      (:section :id "questions-selection"
        (:h3 "Haz click en un botón para agregar una pregunta.")
        (:div :class "buttons"
              (:a :id "radio-choice"
                  (:img :src "/static/icons/text_list_bullets.png" :alt "")
                  "Opción múltiple")
              (:a :id "true-false"
                  (:img :src "/static/icons/help.png" :alt "")
                  "Falso-Verdadero")
              (:a :id "checkboxes"
                  (:img :src "/static/icons/tick.png" :alt "")
                  "Selección múltiple")
              (:a :id "textarea"
                  (:img :src "/static/icons/text_dropcaps.png" :alt "")
                  "Texto libre")))
      (:section :id "questions-submit" (submit-button "Guardar cambios")))))

(define-json-fn design/backend-get-form-questions
  (let ((form-obj (or (data/get-form (parameter "id")) (error ""))))
    (htm
      (str
        (clouchdb:document-to-json
          (mapcar
            (lambda (alist)
              (acons :|answers|
                     (data/get-answers-by-question
                       (cdr (assoc :|_id| alist)) :raw-alist t)
                     alist))
            (data/get-questions-by-form (form-id form-obj) :raw-alist t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAVE FORM DESIGN.

(define-json-fn design/backend-save-form
  (unless (eql :post (request-method*)) (redirect "/"))
  ;; The handler-case is to make it easy for us to bail at any point
  ;; while processing this request.
  (handler-case
    (let ((form-obj (or (data/get-form (parameter "id")) (redirect "/")))
          (questions-data (or (clouchdb:json-to-document
                                (trim-or-nil (post-parameter "questions")))
                              (error "empty-questions")))
          (to-delete (clouchdb:json-to-document
                       (trim-or-nil (post-parameter "todelete")))))
      ;; Set the title and notes.
      (setf (form-title form-obj) (or (trim-or-nil (post-parameter "title"))
                                      (error "title"))
            (form-notes form-obj) (trim-or-nil (post-parameter "notes")))
      ;; And save the form along with its questions.
      ;; The questions and answers must be numerated so that we can display
      ;; them in the proper order.
      (if (and (data/save-form form-obj)
               (data/delete-form-parts to-delete)
               (data/save-form-questions
                 form-obj
                 (design%numerate-questions-and-answers questions-data)))
        (htm (str (clouchdb:document-to-json
                    `((:|status| . "ok")
                      (:|id| . ,(form-id form-obj))))))
        (error "Form couldn't be saved.")))
    ;;
    ;; Oops.
    ;;
    (error (c)
           (htm (str (clouchdb:document-to-json
                       `((:|status| . "error")
                         (:|error| . ,(format nil "~a" c)))))))))

(defun design%numerate-questions-and-answers (data)
  "This function will add a new key :|question-number| to each question's
  alist in increasing order, while also adding a new key :|answer-number| to
  each of the answers of each question. This is so that we can record the
  order of the questions and answers as the user intented them to be."
  (let ((question-number 0) (answer-number 0))
    (labels ((numerate-answer (alist-answer)
                (acons :|answer-number| (incf answer-number) alist-answer))
             (numerate-question (alist-q)
                (pairlis
                  (list :|answers| :|question-number| :|control| :|text| :|_id|)
                  (list (mapcar #'numerate-answer
                                (cdr (assoc :|answers| alist-q)))
                        (incf question-number)
                        (cdr (assoc :|control| alist-q))
                        (cdr (assoc :|text| alist-q))
                        (cdr (assoc :|_id| alist-q))))))
      (mapcar #'numerate-question data))))

#| (let ((decoded-data1 (clouchdb:json-to-document
                       #"[{"text":"Pregunta", "control":"radio-choice",
                           "answers":[{"control":"radio-choice","text":"Input a possible answer."},
                                      {"control":"radio-choice","text":"Opcion 2"},
                                      {"control":"radio-choice","text":"Opcion 3"}]}]"#))
      (decoded-data2 (clouchdb:json-to-document
                       #"[{"text":"Opcion multiple", "control":"radio-choice", "_id": "qid123",
                           "answers":[{"control":"radio-choice","_id":"aid1","selected":false,"text":"Possible answer."},
                                      {"control":"radio-choice","_id":"aid2","selected":true,"text":"Opcion 2"},
                                      {"control":"radio-choice","_id":"aid3","selected":false,"text":"Opcion 3"}]},
                          {"text":"Falso - Verdadero", "control":"true-false","_id":"qid456",
                           "answers":[{"control":"true-false","_id":"aid4","selected":true,"text":"Verdadero"},
                                      {"control":"true-false","_id":"aid5","selected":false,"text":"Falso"}]},
                          {"text":"Seleccion multiple", "control":"checkbox","_id":"qid789",
                           "answers":[{"control":"checkbox","_id":"aid6","selected":true,"text":"Seleccion 1"},
                                      {"control":"checkbox","_id":"aid7","selected":true,"text":"Seleccion 2"},
                                      {"control":"checkbox","_id":"aid8","selected":false,"text":"Seleccion 3"}]},
                          {"text":"Texto libre", "control":"textarea","_id":null,
                           "answers":[{"control":"textarea","_id":null,"text":""}]}]"#)))
  (declare (ignorable decoded-data1 decoded-data2))
  (design%numerate-questions-and-answers decoded-data2)) |#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM PREVIEW

;;; TODO: We are repeating code with public/a
(define-url-fn design/preview-form
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (questions (form-questions form)))
    ;;
    ;;
    (standard-page (:title (form-title form)
                    :show-banner nil
                    :show-footer nil
                    :css-files ("public-styles.css?v=20101008"))
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
        (dolist (question questions)
          (htm (:div :class "question"
                 (:h2 (esc (format nil "~d. ~a"
                                   (question-sort question)
                                   (question-text question))))
                 (unless (question-valid-p question)
                   (htm (:span :class "invalid"
                               "Por favor contesta la pregunta.")))
                 (:div :class "answers"
                   (dolist (answer (question-answers question))
                     (public%display-answer answer question)))))))
      ;;
      ;; Submit button
      ;;
      (push-info-msg "Esta es una vista preliminar de la evaluación,
                     no se puede mandar respuestas desde aquí.")
      (:section :id "submit"
        (submit-button "Enviar respuestas" :disabled t)
        (show-all-messages)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACTIVATE/DEACTIVATE FORM

(define-url-fn design/activate-form
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (id (form-id form)))
    ;;
    ;;
    (let ((now (make-date (get-universal-time) (or time-zone 6))))
      (when (and (eql :post (request-method*))
                 (setf (form-status form) "active"
                       (form-start-date form) now)
                 (dashboard%process-form-options form))
        (redirect (format nil "/dashboard/form-info?id=~a" id))))
    ;;
    ;;
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("design-styles.css?v=20101027"
                                "dashboard.css?v=20101027"))
      (:form :method "post" :action "/design/activate-form"
       (hidden-input "id" :default-value id)
       (dashboard%render-form-title-and-links form)
       (with-tabbed-page (id :current :form-info)
         (:div :id "form-activate-button"
               (:p "Al hacer click en el botón de abajo se activara la
                   evaluación; obtendrás una URL, la cual deberás mandar a todas
                   aquellas personas que desees tomen parte en la evaluación.")
               (:div :class "button"
                     (submit-button "Activar evaluación")))
         (dashboard%render-form-options form
                                        :with-hidden-id nil
                                        :with-submit-button nil))))))

(define-url-fn design/deactivate-form
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (id (form-id form)))
    ;;
    ;;
    (when (eql :post (request-method*))
      ;; TODO: save settings
      (setf (form-status form) "inactive")
      (data/save-form  form)
      (redirect (format nil "/dashboard/form-info?id=~a" id)))
    ;;
    ;;
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("design-styles.css?v=20101027"
                                "dashboard.css?v=20101027"))
      (:form :method "post" :action "/design/deactivate-form"
       (hidden-input "id" :default-value id)
       (dashboard%render-form-title-and-links form)
       (with-tabbed-page (id :current :form-info)
         (:div :id "form-deactivate-button"
           (:p "¿Estas seguro(a) que deseas detener las evaluaciones? Al
                detener las evaluaciones nadie podrá enviar mas respuestas.")
           (:div :class "button"
                 (:a :href (escape-string
                            (format nil "/dashboard/form-info?id=~a" id))
                     "Cancelar")
                 (submit-button "Detener evaluaciones")))
         (dashboard%render-form-stats form :with-download-button nil))))))
