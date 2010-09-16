(in-package #:evalua)

(define-url-fn dashboard
  (let ((active-forms (data/get-active-forms the-user))
        (inactive-forms (data/get-inactive-forms the-user)))
    (standard-page (:title "Dashboard"
                    :css-files ("dashboard.css?v=20100914"
                                "tablesorter/blue/style.css")
                    :js-files ("jquery-1.4.2.min.js"
                               "jquery.tablesorter.min.js"
                               "dashboard.js?v=20100914"))
      (:section :id "dashboard"
        (:header (:h1 "Dashboard")
          (:a :href "/create-new-form"
              (:span "Crea una evaluación")))
        (:div :class "listing"
          (:h2 "Mis evaluaciones activas")
          (:h3 "(haz click en una evaluación para ver los reportes)")
          (:table :id "id-table-active"
                  :class "tablesorter"
                  :cellspacing 1 :cellpadding 0
            (table-columns "Evaluación" "Fecha Inicio" "Fecha Fin"
                           "Enviadas" "Promedio Puntos" "Configuración")
            (:tfoot
              (:tr (:th "Totals:")
                   (:th "")
                   (:th "")
                   (:th "")
                   (:th "")
                   (:th "")))
            (:tbody
              (if active-forms
                (loop for form-obj in active-forms
                      do (dashboard/render-form-as-row form-obj))
                (htm (:tr (:td :colspan 6
                               "No tienes evaluaciones activas.")))))))
        (:div :class "listing"
          (:h2 "Mis evaluaciones inactivas")
          (:h3 "(haz click en una evaluación para ver los reportes)")
          (:table :id "id-table-inactive"
                  :class "tablesorter"
                  :cellspacing 1 :cellpadding 0
            (table-columns "Evaluación" "Fecha Inicio" "Fecha Fin"
                           "Enviadas" "Promedio Puntos" "Configuración")
            (:tfoot
              (:tr (:th "Totals:")
                   (:th "")
                   (:th "")
                   (:th "")
                   (:th "")
                   (:th "")))
            (:tbody
              (if inactive-forms
                (loop for form-obj in inactive-forms
                      do (dashboard/render-form-as-row form-obj))
                (htm (:tr (:td :colspan 6
                               "No tienes evaluaciones inactivas.")))))))))))

(defun dashboard/render-form-as-row (form-obj)
  (let ((id (form-id form-obj))
        (date (form-date form-obj))
        (title (or (form-title form-obj) "N/A"))
        (valid (form-valid-date form-obj)))
    ;; We want to limit the title to 50 chars.
    (when (> (length title) 50)
      (setf title (escape-string
                    (format nil "~a..." (subseq title 0 46)))))
    (with-html-output (*standard-output*)
      (:tr
        (:td (:a :href (escape-string ;;TODO: Is escape-string necessary here?
                         (format nil "/form-reports?id=~a" id))
                 (str title)))
        (:td (esc (format-date (parse-iso8601-date date))))
        (:td (if valid
               (htm (esc (format-date (parse-iso8601-date valid))))
               (htm "N/A")))
        (:td "20")
        (:td "90.3")
        (:td (:a :href (escape-string ;; TODO: is escape necessary?
                         (format nil "/edit-form-options?id=~a" id))
                 "Configuración"))))))
