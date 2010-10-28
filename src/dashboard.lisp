(in-package #:evalua)

(define-url-fn dashboard
  (let ((active-forms (data/get-active-forms the-user))
        (inactive-forms (data/get-inactive-forms the-user)))
    (standard-page (:title "Dashboard"
                    :css-files ("dashboard.css?v=20101004"
                                "tablesorter/blue/style.css")
                    :js-files ("jquery-1.4.2.min.js"
                               "jquery.tablesorter.min.js"
                               "dashboard.js?v=20100914"))
      (:section :id "dashboard"
        (:header (:h1 "Dashboard"))
        (:div :id "big-button"
         (:a :href "/design/create-new-form"
              (:span "Crea una evaluación")))
        (:div :class "listing"
          (:h2 "Mis evaluaciones activas")
          ;(:h3 "(haz click en una evaluación para ver los reportes)")
          (:table :id "id-table-active"
                  :class "tablesorter"
                  :cellspacing 1 :cellpadding 0
            (table-columns "Evaluación" "Fecha Inicio" "Enviadas"
                           "Promedio Puntos" "Configuración")
            ;(:tfoot
            ;  (:tr (:th "Totals:")
            ;       (:th "")
            ;       (:th "")
            ;       (:th "")
            ;       (:th "")
            ;       (:th "")))
            (:tbody
              (if active-forms
                (loop for form-obj in active-forms
                      do (dashboard%render-form-as-row form-obj))
                (htm (:tr (:td :colspan 6
                               "No tienes evaluaciones activas.")))))))
        (:div :class "listing"
          (:h2 "Mis evaluaciones inactivas")
          (:table :id "id-table-inactive"
                  :class "tablesorter"
                  :cellspacing 1 :cellpadding 0
            (table-columns "Evaluación" "Enviadas" "Promedio Puntos"
                           "Configuración")
            ;(:tfoot
            ;  (:tr (:th "Totals:")
            ;       (:th "")
            ;       (:th "")
            ;       (:th "")
            ;       (:th "")
            ;       (:th "")))
            (:tbody
              (if inactive-forms
                (loop for form-obj in inactive-forms
                      do (dashboard%render-form-as-row form-obj
                                                       :start-date nil))
                (htm (:tr (:td :colspan 6
                               "No tienes evaluaciones inactivas.")))))))))))

(defun dashboard%render-form-as-row (form-obj &key (start-date t))
  (let ((id (form-id form-obj))
        (title (or (form-title form-obj) "N/A")))
    ;; We want to limit the title to 50 chars.
    (when (> (length title) 50)
      (setf title (escape-string
                    (format nil "~a..." (subseq title 0 46)))))
    (with-html-output (*standard-output*)
      (:tr
        (:td (:a :href (escape-string ;;TODO: Is escape-string necessary here?
                         (format nil "/dashboard/form-info?id=~a" id))
                 (str title)))
        (when start-date
          (htm (:td (esc (format-date (form-start-date form-obj))))))
        (:td (str (aif (data/get-submissions-by-form-count form-obj) it "N/A")))
        (:td "N/A")
        (:td (:a :href (escape-string ;; TODO: is escape necessary?
                         (format nil "/dashboard/form-info?id=~a" id))
                 "Configuración"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM INFORMATION

(define-url-fn dashboard/form-info
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (id (form-id form))
         (public-url (format nil "http://~a/a?id=~a"
                             (host)
                             (form-public-id form))))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20101027"))
      ;;
      ;; Form title and links to modify/preview.
      ;;
      (dashboard%render-form-title-and-links form)
      ;;
      ;; Pause/Run button & description, incl. link to form.
      ;;
      (with-tabbed-page (id :current :form-info)
        (:section :id "form-info-run-button"
          (show-all-messages)
          (if (string= (form-status form) "active")
              (htm (:p "La evaluación se encuentra corriendo: "
                       (:a :target "_blank" :class "link"
                           :href public-url (esc public-url)))
                   (:p "Deberás enviar la siguiente liga mostrada arriba a todas
                        las personas que desees tomen parte en la evaluación.")
                   (:p "Para detener el proceso de evaluación deberás hacer
                        click en el siguiente botón:")
                   (:form :method "get" :action "/design/deactivate-form"
                          (:p :class "stop-button"
                              (hidden-input "id" :default-value id)
                              (submit-button "Detener evaluaciones"))))
              (htm
               (:p "La evaluación se encuentra en pausa, nadie podrá contestar
                    la evaluación mientras se encuentre pausada.")
               (:p "Puedes continuar editando la evaluación, agregando y
                    modificando preguntas y respuestas.")
               (:p "Cuando hayas terminado de diseñar tu evaluación y desees
                    comenzar a recibir respuestas haz click en el siguiente
                    botón:")
               (:form :method "get" :action "/design/activate-form"
                      (:p :class "button"
                          (hidden-input "id" :default-value id)
                          (submit-button "Comenzar evaluaciones"))))))))))

(defun dashboard%render-form-title-and-links (form)
  (with-html-output (*standard-output*)
    (:section :id "form-info-title"
      (:div :class "title"
            (:h1 "Evaluación: " (:span :class "title" (esc (form-title form))))
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
                                     (format nil "/design/edit-form?id=~a"
                                             (form-id form)))
                              "Modificar evaluación"))
                     (:li :class "preview"
                          (:a :href (escape-string
                                     (format nil "/design/preview-form?id=~a"
                                             (form-id form)))
                              :target "_blank"
                              "Vista preliminar")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM OPTIONS

(define-url-fn dashboard/form-options
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (id (form-id form)))
    ;;
    (when (and (eql :post (request-method*))
               (dashboard%process-form-options form))
      (push-success-msg "Las opciones se han guardado.")
      (redirect (format nil "/dashboard/form-options?id=~a" id)))
    ;;
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20101027"))
      ;;
      ;; Form title and links to modify/preview.
      ;;
      (dashboard%render-form-title-and-links form)
      ;;
      ;; Form options box.
      ;;
      (with-tabbed-page (id :current :form-options)
        (:form :method "post" :action "/dashboard/form-options"
               (dashboard%render-form-options form))))))

(defun dashboard%render-form-options (form &key (div.id "form-info-options")
                                      (with-hidden-id t)
                                      (with-submit-button t))
  (let ((tries (form-tries-limit form))
        (id (form-id form)))
    (with-html-output (*standard-output*)
      (:div :id (escape-string div.id)
       (when with-hidden-id
         (htm (hidden-input "id" :default-value id)))
       (show-all-messages)
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
          Si la evaluación contiene preguntas de texto libre éstas
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

(defun dashboard%process-form-options (form-obj)
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
                (push-error-msg "El formato de tiempo límite debe ser hh:mm,
                                por ejemplo 03:00"))
        (setf (form-time-limit form-obj) time-limit
              (form-tries-limit form-obj) tries
              (form-score-p form-obj) score-p
              (form-comments-p form-obj) comments-p)
        ;; Save it!
        (data/save-form form-obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM STATISTICS.

(define-url-fn dashboard/form-stats
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (submissions-count (data/get-submissions-by-form-count form)))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20101027"))
      ;;
      ;; Form title and links to modify/preview.
      ;;
      (dashboard%render-form-title-and-links form)
      ;;
      ;; Form statistics/download box.
      ;;
      (with-tabbed-page ((form-id form) :current :form-stats)
        (dashboard%render-form-stats form)))))

(defun dashboard%render-form-stats (form &key (div.id "form-info-stats")
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
        (htm (:form :method "get" :action "/dashboard/download"
               (hidden-input "id" :default-value (form-id form))
               (:p "Haz click en el siguiente botón para descargar la
                   información de las evaluaciones completadas. Puedes abrir
                   este archivo en Excel:")
               (:div :class "button"
                 (submit-button "Descargar estadísticas"
                                :name "download"))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM DOWNLOAD OPTIONS

(define-url-fn dashboard/download
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (submissions (data/get-submissions-by-form form))
         (questions (form-questions form)))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20101020"
                                "tablesorter/blue/style.css")
                    :js-files ("jquery-1.4.2.min.js"
                               "jquery.tablesorter.min.js"
                               "dashboard-stats.js?v=20101020"))
      (:section :id "form-download-stats"
        (:h1 "Espérame tantito!")
        (:form :method "post" :action "/dashboard/download-excel"
               (hidden-input "id" :default-value (form-id form))
               (submit-button "Descargar"))
        (:table :id "id-table-stats" :class "tablesorter"
                :cellspacing 1 :cellpadding 0
          (:thead
            (:tr (:th "Hora")
                 (loop for qq in questions
                       do (htm (:th (esc (question-text qq)))))))
          (:tbody
            (dashboard%render-submitted-answers submissions questions)))))))

(defun dashboard%render-submitted-answers (subs questions)
  (labels ((%limit-width (x)
             (if (> (length x) 50)
               (format nil "~a..." (subseq x 0 46))
               x))
           (%collect-answers (answers cached)
             (mapcar
               (lambda (ans)
                 (aif (%lowassoc value ans)
                   ;; If there's a value, we use that.
                   (%limit-width it)
                   ;; Other wise we need to retrieve the 'answer' document
                   ;; from the DB to lookup it's 'text' field; unless we
                   ;; already have it in our hash table.
                   (let ((id (%lowassoc answer ans)))
                     (aif (gethash id cached)
                       it
                       ;; cache miss... update it,
                       (setf (gethash id cached)
                             (%limit-width ; limit the widht,
                               (%lowassoc text ; of the text field,
                                          ;; of the 'answer' document.
                                          (clouchdb:get-document id))))))))
               answers))
           (%render-answers (answers cached)
             (with-html-output (*standard-output*)
               (:td
                 (esc
                   (format nil "~{~a~^, ~}"
                           (%collect-answers answers cached)))))))
    ;;
    ;;
    (let ((cached-ansers (make-hash-table)))
      (dolist (ss subs)
        (with-html-output (*standard-output*)
          (:tr
            (:td (esc (format-date (submission-finish-date ss))))
            (loop for question in questions
                  for answers = (data/get-submitted-answers-by-question
                                  (submission-id ss)
                                  (question-id question))
                  do (%render-answers answers cached-ansers))))))))

(define-url-fn dashboard/download-excel
  (let ((replacer #~s/"/""/)) ;"))
    (labels ((%collect-answers (answers)
               (mapcar (lambda (ans)
                         (aif (%lowassoc value ans)
                              it
                              (%lowassoc text (clouchdb:get-document
                                               (%lowassoc answer ans)))))
                       answers)))
      (when (eql :post (request-method*))
        (with-html-output-to-string (*standard-output*)
          (let* ((form (or (data/get-form (parameter "id"))
                           (redirect "/")))
                 (submissions (data/get-submissions-by-form form))
                 (questions (form-questions form)))
            (setf (hunchentoot:header-out :content-disposition)
                  (format nil "attachment; filename=\"export.csv\""))
            (format *standard-output* "~a~%" (form-title form))
            (format *standard-output* "\"Hora envíada\",~{\"~a\"~^,~}~%"
                    (mapcar (lambda (q)
                              (funcall replacer (question-text q)))
                            questions))
            (dolist (ss submissions)
              (format *standard-output* "\"~a\",~{\"~a\"~^,~}~%"
                      (format-iso8601-date (submission-finish-date ss))
                      (loop for question in questions
                         for answers = (data/get-submitted-answers-by-question
                                        (submission-id ss)
                                        (question-id question))
                         collect (funcall replacer
                                  (format nil "~{~a~^ | ~}"
                                          (%collect-answers answers))))))))))))