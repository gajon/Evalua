(in-package #:evalua)

(define-url-fn create-new-form
  ;; TODO: time-zone
  (let* ((now (make-date (get-universal-time) (or time-zone 6)))
         (fresh-form (data/create-fresh-form
                       (make-instance 'form
                                      :user (user-username the-user)
                                      :date (format-iso8601-date now)
                                      :update-date (format-iso8601-date now)
                                      :valid-date nil
                                      :title nil
                                      :notes nil
                                      :time-zone (or time-zone 6)
                                      :email-dest (user-email the-user)))))
    ;; TODO: Error handling?
    (if fresh-form
      (redirect (format nil "/edit-form?id=~a" (form-id fresh-form)))
      (redirect "/"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM DESIGN PAGE.

(define-url-fn edit-form
  ;; The only utility of this function is to render the initial HTML for the
  ;; JavaScript side to work. And the actual saving of posted information is
  ;; done by the function `backend-save-form` below.
  (let ((form (or (data/get-form (parameter "id")) (redirect "/"))))
    (standard-page (:title "Paso 1. Diseña tu cuestionario."
                    :css-files ("design-styles.css?v=20100910")
                    :js-files ("jquery-1.4.2.min.js"
                               "json2.min.js"
                               "design.js?v=20100910"))
      (hidden-input "id" :default-value (form-id form))
      (:section :id "questions"
        (:h1 "Paso 1. Diseña tu cuestionario")
        (:div :id "form-title"
              (text-input "Asigna un título a la evaluación:" "title"))
        (:div :id "form-notes"
              (text-area "Indica las instrucciones (opcional):" "notes"))
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
      (:section :id "questions-submit" (submit-button "Enviar")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAVE FORM DESIGN.

;(define-condition design-error (error) ((element :initarg :element :reader design-error-element) (message :initarg :msg :reader design-error-message)) (:report (lambda (condition stream) (format stream "~a" (design-error-message condition)))))

(define-json-fn backend-save-form
  (unless (eql :post (request-method*)) (redirect "/"))
  ;; The handler-case is to make it easy for us to bail at any point
  ;; while processing this request.
  (handler-case
    (let ((form-obj (or (data/get-form (parameter "id")) (redirect "/")))
          (decoded-data (or (clouchdb:json-to-document
                              (trim-or-nil (post-parameter "questions")))
                            (error "empty-questions"))))
      ;; Set the title and notes.
      (setf (form-title form-obj) (or (trim-or-nil (post-parameter "title"))
                                      (error "title"))
            (form-notes form-obj) (trim-or-nil (post-parameter "notes")))
      ;; And save the form along with its questions.
      ;; The questions and answers must be numerated so that we can display
      ;; them in the proper order.
      (if (and (data/save-form form-obj)
               (data/save-form-questions
                 form-obj
                 (numerate-questions-and-answers decoded-data)))
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

(defun numerate-questions-and-answers (data)
  "This function will add a new key :|question-number| to each question's
  alist in increasing order, while also adding a new key :|answer-number| to
  each of the answers of each question. This is so that we can record the
  order of the questions and answers as the user intented them to be."
  (let ((question-number 0) (answer-number 0))
    (labels ((numerate-answer (alist-answer)
                (acons :|answer-number| (incf answer-number) alist-answer))
             (numerate-question (alist-q)
                (pairlis (list :|answers| :|question-number| :|text|)
                         (list (mapcar #'numerate-answer
                                       (cdr (assoc :|answers| alist-q)))
                               (incf question-number)
                               (cdr (assoc :|text| alist-q))))))
      (mapcar #'numerate-question data))))

;(let ((decoded-data1 (clouchdb:json-to-document #"[{"text":"Pregunta","answers":[{"control":"radio-choice","text":"Input a possible answer."},{"control":"radio-choice","text":"Opcion 2"},{"control":"radio-choice","text":"Opcion 3"}]}]"#))
      ;(decoded-data2 (clouchdb:json-to-document #"[{"text":"Opcion multiple","answers":[{"control":"radio-choice","text":"Input a possible answer."},{"control":"radio-choice","text":"Opcion 2"},{"control":"radio-choice","text":"Opcion 3"}]},{"text":"Falso - Verdadero","answers":[{"control":"true-false","text":"Verdadero"},{"control":"true-false","text":"Falso"}]},{"text":"Seleccion multiple","answers":[{"control":"checkbox","text":"Seleccion 1"},{"control":"checkbox","text":"Seleccion 2"},{"control":"checkbox","text":"Seleccion 3"}]},{"text":"Texto libre","answers":[{"control":"textarea","text":""}]}]"#)))
  ;(numerate-questions-and-answers decoded-data2))


(define-url-fn edit-form-options
  (let ((form (or (data/get-form (parameter "id")) (redirect "/"))))
    ;;
    (when (and (eql :post (request-method*))
               (process-form-options form))
      (redirect (format nil "/form-info?id=~a" (form-id form))))
    ;;
    (standard-page (:title "Paso 2. Configura las opciones."
                    :css-files ("design-styles.css"))
      (:form :method "post" :action "."
        (hidden-input "id" :default-value (form-id form))
        (:section :id "options"
          (:h1 "Paso 2. Configura las opciones")
          (:div :id "time" (text-input "Tiempo límite:" "timelimit"))
          (:div :id "tries" (text-input "Intentos permitidos:" "tries"))
          (:div :id "score"
            (:label "¿Asignar calificación?:")
            (radio-choice "Si" "score" "yes" :labelclass "radio")
            (radio-choice "No" "score" "no" :labelclass "radio"))
          (:div :id "comments"
            (:label "¿Habilitar comentarios?:")
            (radio-choice "Si" "comments" "yes" :labelclass "radio")
            (radio-choice "No" "comments" "no" :labelclass "radio"))
          (:div :id "email"
            (text-input "Cuenta de correo a donde se enviarán los resultados:"
                        "email")))
        (:section :id "options-submit" :class "buttons"
          (:a :class "negative" :href "#"
              (:img :src "/static/icons/cancel.png")
              "Cancelar")
          ;; TODO: abstract this out.
          (submit-button "<img src=\"/static/icons/accept.png\"/> Continuar"
                         :inputclass "positive"
                         :escape-label nil))))))

(defun process-form-options (form-obj)
  (let ((time-limit (trim-or-nil (post-parameter "timelimit")))
        (tries (trim-or-nil (post-parameter "tries")))
        (score-p (string= (trim-or-nil (post-parameter "score")) "yes"))
        (comments-p (string= (trim-or-nil (post-parameter "comments")) "yes"))
        (email (trim-or-nil (post-parameter "email"))))
    (when (require-fields email)
      (setf (form-time-limit form-obj) time-limit
            (form-tries-limit form-obj) tries
            (form-score-p form-obj) score-p
            (form-comments-p form-obj) comments-p
            (form-email-dest form-obj) email)
      ;; Save it!
      (setf form-obj (data/save-form form-obj)))))

(define-url-fn form-info
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (public-url (format nil "http://~a:8081/a?id=~a"
                             (host)
                             (form-public-id form))))
    (standard-page (:title "Paso 3. Envía tu cuestionario."
                    :css-files ("design-styles.css"))
      (:section :id "form-info"
        (:h1 "Paso 3. Envía tu cuestionario")
        (:div (:p "La liga para accesar a tu cuestionario es la siguiente:"))
        (:div :id "public-url" :class "text-center"
          (:a :href (escape-string public-url)
              :target "_blank"
              (esc public-url)))
        (:div (:p "Si deseas compartirla en con tus contactos y amigos, haz click en un botón de abajo:"))
        (:div :id "share-buttons" :class "text-center"
          (:img :src "/static/social-buttons.jpg")))
      (:section :id "form-info-submit" :class "buttons"
        (submit-button "<img src=\"/static/icons/accept.png\"/> Finalizar"
                       :inputclass "positive"
                       :escape-label nil)))))


