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
                      do (dashboard/render-form-as-row form-obj))
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
                      do (dashboard/render-form-as-row form-obj
                                                       :start-date nil))
                (htm (:tr (:td :colspan 6
                               "No tienes evaluaciones inactivas.")))))))))))

(defun dashboard/render-form-as-row (form-obj &key (start-date t))
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
         (title (escape-string (form-title form)))
         (public-url (format nil "http://~a/a?id=~a"
                             (host)
                             (form-public-id form))))
    ;;
    (when (and (eql :post (request-method*))
               ;; TODO: Distinguish action.
               (design/process-form-options form))
      (push-success-msg "Las opciones se han guardado.")
      (redirect (format nil "/dashboard/form-info?id=~a" id)))
    ;;
    (standard-page (:title (format nil "Evaluación: ~a" title)
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
      (with-tabbed-page (:current :form-info)
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
                          (submit-button "Comenzar evaluaciones")))))))
      ;;
      ;; Form options box and statistics/download box.
      ;;
      (:section :id "form-info-options-and-stats"
        ;; Options
        (:form :method "post" :action "/dashboard/form-info"
         (design%render-form-options form))
        ;; Stats
        (design%render-form-stats form)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STATS AND DOWNLOAD OPTIONS

(define-url-fn dashboard/download
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (title (escape-string (form-title form)))
         (submissions (data/get-submissions-by-form form))
         (questions (form-questions form)))
    (standard-page (:title (format nil "Evaluación: ~a" title)
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