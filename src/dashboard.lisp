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
                         (format nil "/design/form-info?id=~a" id))
                 (str title)))
        (when start-date
          (htm (:td (esc (format-date (form-start-date form-obj))))))
        (:td (str (aif (data/get-submissions-by-form-count form-obj) it "N/A")))
        (:td "N/A")
        (:td (:a :href (escape-string ;; TODO: is escape necessary?
                         (format nil "/design/form-info?id=~a" id))
                 "Configuración"))))))


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
