(in-package #:evalua)

(define-url-fn design/create-new-form
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
                    :css-files ("design-styles.css?v=20101004")
                    :js-files ("jquery-1.4.2.min.js"
                               "json2.min.js"
                               "design.js?v=20100923"))
      (hidden-input "id" :default-value (form-id form))
      (:section :id "questions"
        (:h1 "Paso 1. Diseña tu cuestionario")
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
      (:section :id "questions-submit" (submit-button "Enviar")))))

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

;(define-json-fn design/backend-create-fresh-question
  ;(when (eql :post (request-method*))
    ;(handler-case
      ;(let* ((form-obj (or (data/get-form (parameter "id")) (error "")))
             ;(q-type (or (parameter "control") (error "")))
             ;(fresh-q (data/create-fresh-question (form-id form-obj) q-type)))
        ;(htm (str (clouchdb:document-to-json fresh-q))))
      ;;;
      ;;; TODO: make up your mind about error handling.
      ;(error (c)
             ;(htm (str (clouchdb:document-to-json
                         ;`((:|status| . "error")
                           ;(:|error| . ,(format nil "~a" c))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAVE FORM DESIGN.

;(define-condition design-error (error) ((element :initarg :element :reader design-error-element) (message :initarg :msg :reader design-error-message)) (:report (lambda (condition stream) (format stream "~a" (design-error-message condition)))))

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
                 (design/numerate-questions-and-answers questions-data)))
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

(defun design/numerate-questions-and-answers (data)
  "This function will add a new key :|question-number| to each question's
  alist in increasing order, while also adding a new key :|answer-number| to
  each of the answers of each question. This is so that we can record the
  order of the questions and answers as the user intented them to be."
  (let ((question-number 0) (answer-number 0))
    (labels ((numerate-answer (alist-answer)
                (acons :|answer-number| (incf answer-number) alist-answer))
             (numerate-question (alist-q)
                (pairlis (list :|answers| :|question-number| :|control| :|text| :|_id|)
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
  (design/numerate-questions-and-answers decoded-data2)) |#


(define-url-fn design/edit-form-options
  (let ((form (or (data/get-form (parameter "id")) (redirect "/"))))
    ;;
    (when (and (eql :post (request-method*))
               (design/process-form-options form))
      (redirect (format nil "/design/form-info?id=~a" (form-id form))))
    ;;
    (standard-page (:title "Paso 2. Configura las opciones."
                    :css-files ("design-styles.css?v=20101004"))
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

(defun design/process-form-options (form-obj)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM INFORMATION

(define-url-fn design/form-info
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (id (form-id form))
         (title (escape-string (form-title form)))
         (public-url (format nil "http://~a:8081/a?id=~a"
                             (host)
                             (form-public-id form))))
    (standard-page (:title (format nil "Evaluacion: ~a" title)
                    :css-files ("design-styles.css?v=20101007"))
      ;;
      ;; Form title and links to modify/preview.
      ;;
      (:section :id "form-info-title"
        (:div :class "title"
          (:h1 "Evaluación: " (:span :class "title" (str title)))
          (:p :class "dates"
              (:em "Fecha de creación: ")
              (:span :class "date"
                (esc (format-date (parse-iso8601-date (form-date form)))))
              (:em "Última modificación: ")
              (:span :class "date"
                (esc (format-date (parse-iso8601-date
                                    (form-update-date form)))))
              (:em :class "state-paused" "Pausada")))
        (:div :class "links"
          (:ul
            (:li :class "edit"
              (:a :href (escape-string (format nil "/design/edit-form?id=~a" id))
                  "Modificar evaluación"))
            (:li :class "preview"
              (:a :href (escape-string
                          (format nil "/design/preview-form?id=~a" id))
                  "Vista preliminar")))))
      ;;
      ;; Pause/Run button & description, incl. link to form.
      ;;
      (:section :id "form-info-run-button"
        (:p "La evaluación se encuentra en pausa, nadie podra contestar la
            evaluación mientras se encuentre pausada.")
        (:p "Puedes continuar editando la evaluación, agregando y modificando
            preguntas y respuestas.")
        (:p "Cuando hayas terminado de diseñar tu evaluación y desees comenzar
            a recibir respuestas haz click en el siguiente botón:")
        (:p :class "button"
            (button "Comenzar evaluaciones" "start")))
      ;;
      ;; Form options box and statistics/download box.
      ;;
      (:section :id "form-info-options-and-stats"
        (:div :id "form-info-options"
          (:h2 "Opciones")
          (:div :class "option" :id "form-option-time"
            (:p (text-input "Tiempo límite:" "timelimit" :size 6)
                (:small "hh:mm"))
            (:p :class "help"
                "Limita el tiempo disponible para completar la evaluación.
                Si el evaluado no termina la evaluación en el tiempo
                indicado las respuestas que haya dato hasta ese
                momento se guardarán y ya no podrá continuar con el resto de
                la evaluación."))
          (:div :class "option" :id "form-option-tries"
            (:p (text-input "Intentos permitidos:" "tries" :size 3))
            (:p :class "help"
                "El número de veces que un evaluado podrá participar en la
                evaluación. En caso de que se le permita contestar la
                evaluación mas de una vez los resultados enviados se
                acumularán."))
          (:div :class "option" :id "form-option-score"
            (:p (:label "¿Asignar calificación?:")
                (radio-choice "Si" "score" "yes" :labelclass "radio")
                (radio-choice "No" "score" "no" :labelclass "radio"))
            (:p :class "help"
                "Si al diseñar la evaluación se indicaron cuáles eran las
                respuestas correctas el sistema podrá evaluar
                automáticamente las respuestas enviadas por los evaluados.
                Si la evaluacion contiene preguntas de texto libre éstas
                deberán ser revisadas manualmente para obtener la
                calificación final."))
          (:div :class "option" :id "form-option-comments"
            (:p (:label "¿Habilitar comentarios?:")
                (radio-choice "Si" "comments" "yes" :labelclass "radio")
                (radio-choice "No" "comments" "no" :labelclass "radio"))
            (:p :class "help"
                "Al habilitar esta opción el evaluado podrá, después de
                haber completado la evaluación, dejar comentarios para el
                evaluador."))
          (:div :class "button"
            (button "Guardar opciones" "options")))
        (:div :id "form-info-stats"
          (:h2 "Estadísticas")
          (:div :class "stats"
            (:label "Evaluaciones completadas: ") "3")
          (:div :class "stats"
            (:label "Fecha inicio: ") "N/A")
          (:div :class "stats"
            (:label "Días corriendo la evaluación: ") "0")
          (:p "Haz click en el siguiente botón para descargar la información
          de las evaluaciones completadas. Puedes abrir este archivo en Excel:")
          (:div :class "button"
            (button "Descargar estadísticas" "download")))))))

      ;(:section :id "form-info-submit" :class "buttons"
        ;(submit-button "<img src=\"/static/icons/accept.png\"/> Finalizar"
                       ;:inputclass "positive"
                       ;:escape-label nil)))))
