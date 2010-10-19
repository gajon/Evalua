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
  (design/numerate-questions-and-answers decoded-data2)) |#



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
    ;;
    (when (and (eql :post (request-method*))
               ;; TODO: Distinguish action.
               (design/process-form-options form))
      (push-success-msg "Las opciones se han guardado.")
      (redirect (format nil "/design/form-info?id=~a" id)))
    ;;
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
                (esc (format-date (form-date form))))
              (:em "Última modificación: ")
              (:span :class "date"
                (esc (format-date (form-update-date form))))
              (if (string= (form-status form) "active")
                (htm (:em :class "state-running" "Corriendo"))
                (htm (:em :class "state-paused" "Pausada")))))
        (:div :class "links"
          (:ul
            (:li :class "edit"
              (:a :href (escape-string
                          (format nil "/design/edit-form?id=~a" id))
                  "Modificar evaluación"))
            (:li :class "preview"
              (:a :href (escape-string
                          (format nil "/design/preview-form?id=~a" id))
                  :target "_blank"
                  "Vista preliminar")))))
      ;;
      ;; Pause/Run button & description, incl. link to form.
      ;;
      (:section :id "form-info-run-button"
       (show-all-messages)
       (if (string= (form-status form) "active")
         (htm (:p "La evaluacion se encuentra corriendo: "
               (:a :target "_blank" :class "link"
                :href public-url (esc public-url)))
              (:p "Deberas enviar la siguiente liga mostrada arriba a todas las
                   personas que desees tomen parte en la evaluacion.")
              (:p "Para detener el proceso de evaluacion deberas hacer click en
                   el siguiente boton:")
              (:form :method "get" :action "/design/deactivate-form"
               (:p :class "stop-button"
                (hidden-input "id" :default-value id)
                (submit-button "Detener evaluaciones"))))
         (htm
          (:p "La evaluación se encuentra en pausa, nadie podra contestar la
               evaluación mientras se encuentre pausada.")
          (:p "Puedes continuar editando la evaluación, agregando y modificando
               preguntas y respuestas.")
          (:p "Cuando hayas terminado de diseñar tu evaluación y desees comenzar
               a recibir respuestas haz click en el siguiente botón:")
          (:form :method "get" :action "/design/activate-form"
           (:p :class "button"
            (hidden-input "id" :default-value id)
            (submit-button "Comenzar evaluaciones"))))))
      ;;
      ;; Form options box and statistics/download box.
      ;;
      (:section :id "form-info-options-and-stats"
        ;; Options
        (:form :method "post" :action "/design/form-info"
         (design%render-form-options form))
        ;; Stats
        (design%render-form-stats form)))))

(defun design%render-form-options (form &key (div.id "form-info-options")
                                        (with-hidden-id t)
                                        (with-submit-button t))
  (let ((tries (form-tries-limit form))
        (id (form-id form)))
    (with-html-output (*standard-output*)
      (:div :id (escape-string div.id)
       (when with-hidden-id
         (htm (hidden-input "id" :default-value id)))
       (:h2 "Opciones")
       (:div :class "option" :id "form-option-time"
        (:p (text-input "Tiempo límite:" "timelimit" :size 6
                        :default-value (form-time-limit form))
         (:small "hh:mm"))
        (:p :class "help"
         "Limita el tiempo disponible para completar la evaluación.
          Si el evaluado no termina la evaluación en el tiempo
          indicado las respuestas que haya dato hasta ese
          momento se guardarán y ya no podrá continuar con el resto de
          la evaluación."))
       (:div :class "option" :id "form-option-tries"
        (:p (text-input "Intentos permitidos:" "tries" :size 3
                        :default-value (when (and tries (> tries 0))
                                         (princ-to-string tries))))
        (:p :class "help"
         "El número de veces que un evaluado podrá participar en la
          evaluación. En caso de que se le permita contestar la
          evaluación mas de una vez los resultados enviados se
          acumularán."))
       (:div :class "option" :id "form-option-score"
        (:p (:label "¿Asignar calificación?:")
         (radio-choice "Si" "score" "yes" :labelclass "radio"
                       :current-value (if (form-score-p form)
                                        "yes" "no"))
         (radio-choice "No" "score" "no" :labelclass "radio"
                       :current-value (if (form-score-p form)
                                        "yes" "no")))
        (:p :class "help"
         "Si al diseñar la evaluación se indicaron cuáles eran las
          respuestas correctas el sistema podrá evaluar
          automáticamente las respuestas enviadas por los evaluados.
          Si la evaluacion contiene preguntas de texto libre éstas
          deberán ser revisadas manualmente para obtener la
          calificación final."))
       (:div :class "option" :id "form-option-comments"
        (:p (:label "¿Habilitar comentarios?:")
         (radio-choice "Si" "comments" "yes" :labelclass "radio"
                       :current-value (if (form-comments-p form)
                                        "yes" "no"))
         (radio-choice "No" "comments" "no" :labelclass "radio"
                       :current-value (if (form-comments-p form)
                                        "yes" "no")))
        (:p :class "help"
         "Al habilitar esta opción el evaluado podrá, después de
          haber completado la evaluación, dejar comentarios para el
          evaluador."))
       (when with-submit-button
         (htm (:div :class "button"
               (submit-button "Guardar opciones" :name "options"))))))))


(defun design/process-form-options (form-obj)
  (flet ((validate-time-limit (time)
           (multiple-value-bind (start end start-positions end-positions)
               (#~m/^(\d{1,2}):(\d{1,2})$/ time)
             (declare (ignore end))
             (when start
               (let ((hours (parse-integer time
                                           :start (svref start-positions 0)
                                           :end (svref end-positions 0)))
                     (minutes (parse-integer time
                                             :start (svref start-positions 1)
                                             :end (svref end-positions 1))))
                 (and (> 60 hours) (> 60 minutes)))))))
    ;; Get data from the post, and validate.
    (let ((time-limit (trim-or-nil (post-parameter "timelimit")))
          (tries (parse-int-force-pos-or-zero (post-parameter "tries")))
          (score-p (string= (trim-or-nil (post-parameter "score")) "yes"))
          (comments-p (string= (trim-or-nil (post-parameter "comments"))
                               "yes")))
      ;;
      (when (or (null time-limit)
                (validate-time-limit time-limit)
                (push-error-msg "El formato de tiempo limite debe ser hh:mm,
                                por ejemplo 03:00"))
        (setf (form-time-limit form-obj) time-limit
              (form-tries-limit form-obj) tries
              (form-score-p form-obj) score-p
              (form-comments-p form-obj) comments-p)
        ;; Save it!
        (data/save-form form-obj)))))


(defun design%render-form-stats (form &key (div.id "form-info-stats")
                                      (with-download-button t))
  (with-html-output (*standard-output*)
    (:div :id (escape-string div.id)
      (:h2 "Estadísticas")
      (:div :class "stats"
        (:label "Evaluaciones completadas: ")
        (str (aif (data/get-submissions-by-form-count form) it "N/A")))
      (if (string= (form-status form) "active")
        (htm
          (:div :class "stats"
            (:label "Fecha inicio: ")
            (str (format-date (form-start-date form))))
          (:div :class "stats"
            (:label "Días corriendo la evaluación: ")
            (kmrcl:let-when (start (form-start-date form))
              (let* ((now (make-date (get-universal-time)
                                     (form-time-zone form)))
                     (diff (- (date-universal-time now)
                              (date-universal-time start))))
                ;; It shouldn't happen that DIFF is negative.
                (if (minusp diff)
                  (htm "N/A")
                  (htm (str (format nil "~a día~:p"
                                    (floor (/ diff %secs-in-one-day))))))))))
        ;;
        ;; The form is NOT active.
        ;;
        (htm
          (:div :class "stats inactive" (:label "Fecha inicio: ") "N/A")
          (:div :class "stats inactive"
            (:label "Días corriendo la evaluación: ") "N/A")))
      (when with-download-button
        (htm (:p "Haz click en el siguiente botón para descargar la información
                 de las evaluaciones completadas. Puedes abrir este archivo
                 en Excel:")
             (:div :class "button"
               (button "Descargar estadísticas" "download")))))))


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
         (id (form-id form))
         (title (escape-string (form-title form))))
    ;;
    ;;
    (let ((now (make-date (get-universal-time) (or time-zone 6))))
      (when (and (eql :post (request-method*))
                 (setf (form-status form) "active"
                       (form-start-date form) now)
                 (design/process-form-options form))
        (redirect (format nil "/design/form-info?id=~a" id))))
    ;;
    ;;
    (standard-page (:title (format nil "Evaluacion: ~a" title)
                    :css-files ("design-styles.css?v=20101007"))
      (:form :method "post" :action "/design/activate-form"
       (hidden-input "id" :default-value id)
       ;;
       ;; Form title and links to modify/preview.
       ;;
       (:section :id "form-info-title-activate"
        (:div :class "title"
         (:h1 "Evaluación: " (:span :class "title" (str title)))
         (:p :class "dates"
          (:em "Fecha de creación: ")
          (:span :class "date"
           (esc (format-date (form-date form))))
          (:em "Última modificación: ")
          (:span :class "date"
           (esc (format-date (form-update-date form))))
          ;; TODO: Verify this
          (:em :class "state-paused" "Pausada"))))
       ;;
       ;; Form options box.
       ;;
       (:section :id "form-info-options-and-stats"
        (design%render-form-options form
                                    :with-hidden-id nil
                                    :with-submit-button nil)
        (:div :id "form-activate-button"
         (:p "Indica las opciones que deseas para esta evaluacion.")
         (:p "Al hacer click en el boton de abajo se activara la evaluacion;
              obtendras una URL, la cual deberas de mandar a todas aquellas
              personas que desees tomen parte en la evaluacion.")
         (:div :class "button"
          (submit-button "Activar evaluación"))))))))

(define-url-fn design/deactivate-form
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (id (form-id form))
         (title (escape-string (form-title form))))
    ;;
    ;;
    (when (eql :post (request-method*))
      ;; TODO: save settings
      (setf (form-status form) "inactive")
      (data/save-form  form)
      (redirect (format nil "/design/form-info?id=~a" id)))
    ;;
    ;;
    (standard-page (:title (format nil "Evaluacion: ~a" title)
                    :css-files ("design-styles.css?v=20101007"))
      (:form :method "post" :action "/design/deactivate-form"
       (hidden-input "id" :default-value id)
       ;;
       ;; Form title and links to modify/preview.
       ;;
       (:section :id "form-info-title-activate"
        (:div :class "title"
         (:h1 "Evaluación: " (:span :class "title" (str title)))
         (:p :class "dates"
          (:em "Fecha de creación: ")
          (:span :class "date"
           (esc (format-date (form-date form))))
          (:em "Última modificación: ")
          (:span :class "date"
           (esc (format-date (form-update-date form))))
          ;; TODO: Verify this.
          (:em :class "state-running" "Corriendo"))))
       ;;
       ;; Form statistics box.
       ;;
       (:section :id "form-info-options-and-stats"
        (:div :id "form-deactivate-button"
         (:p "Estas seguro(a) que deseas detener las evaluaciones? Al detener las
              evaluaciones nadie podra enviar mas respuestas.")
         (:div :class "button"
          (:a :href (escape-string (format nil "/design/form-info?id=~a" id))
           "Cancelar")
          (submit-button "Detener evaluaciones")))
        (design%render-form-stats form :with-download-button nil))))))
