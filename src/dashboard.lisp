(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILS

(defgeneric main-url (object &key id &allow-other-keys)
  (:documentation "")
  (:method ((form form) &key id)
           (format nil "/dashboard/form-info?id=~a" (or id (form-id form)))))

(defgeneric public-url (object &key host &allow-other-keys)
  (:documentation "")
  (:method ((form form) &key host)
           (format nil "http://~a/a?id=~a" (or host (host))
                   (form-public-id form))))

(defgeneric options-url (object &key id &allow-other-keys)
  (:documentation "")
  (:method ((form form) &key id)
           (format nil "/dashboard/form-options?id=~a"
                   (or id (form-id form)))))

(defgeneric stats-url (object &key id &allow-other-keys)
  (:documentation "")
  (:method ((form form) &key id)
           (format nil "/dashboard/form-stats?id=~a"
                   (or id (form-id form))))
  (:method ((question question) &key id)
           (declare (ignore id))
           (format nil "/dashboard/form-question-stats?id=~a&qid=~a"
                   (form-id (question-form question))
                   (question-id question))))

(defgeneric edit-url (object &key id &allow-other-keys)
  (:documentation "")
  (:method ((form form) &key id)
           (format nil "/design/edit-form?id=~a"
                   (or id (form-id form)))))

(defgeneric preview-url (object &key id &allow-other-keys)
  (:documentation "")
  (:method ((form form) &key id)
           (format nil "/design/preview-form?id=~a"
                   (or id (form-id form)))))


(defmacro with-form-tabs ((form &optional current) &body body)
  `(with-html-output (*standard-output*)
     (:div :id "tabbed-navigation"
           (:div :id "tabbed-navigation-tabs"
                 (:ul
                  (:li ,@(when (eq current :form-info) `(:class "current"))
                       (:a :href (main-url ,form) "Información"))
                  (:li ,@(when (eq current :form-options) `(:class "current"))
                       (:a :href (options-url ,form) "Opciones"))
                  (:li ,@(when (eq current :form-stats) `(:class "current"))
                       (:a :href (stats-url ,form) "Estadísticas"))
                  (:li ,@(when (eq current :form-download) `(:class "current"))
                       (:a :href "#" "Exportar"))))
           (:div :id "tabbed-navigation-content"
                 ,@body))))

(defgeneric age-in-days (object &optional now)
  (:documentation
   "Returns a number of how many days old this OBJECT is.
For FORM objects its age is 0 if it is not active.")
  (:method ((form form) &optional now)
           (let ((start-date (form-start-date form)))
             (when (and (string= (form-status form) "active")
                        start-date)
               (ceiling (/ (- (or now (get-universal-time))
                              (date-universal-time start-date))
                           %secs-in-one-day))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DASHBOARD

(define-url-fn dashboard
  (let ((active-forms (data/get-active-forms the-user))
        (inactive-forms (data/get-inactive-forms the-user)))
    (standard-page (:title "Dashboard"
                    :css-files ("dashboard.css?v=20101209"
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
          (:table :id "id-table-active"
                  :class "tablesorter"
                  :cellspacing 1 :cellpadding 0
            (table-columns "Evaluación" "Fecha Inicio" "Enviadas"
                           "Promedio Puntos" "Configuración")
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
            (:tbody
              (if inactive-forms
                (loop for form-obj in inactive-forms
                      do (dashboard%render-form-as-row form-obj
                                                       :start-date nil))
                (htm (:tr (:td :colspan 6
                               "No tienes evaluaciones inactivas.")))))))))))

(defun dashboard%render-form-as-row (form-obj &key (start-date t))
  (let ((title (or (form-title form-obj) "N/A")))
    (with-html-output (*standard-output*)
      (:tr
       (:td (:a :href (main-url form-obj)
                :title (escape-string title)
                (esc (truncate-words title 10))))
       (when start-date
         (htm (:td (esc (format-date (form-start-date form-obj))))))
       (:td (str (aif (data/get-submissions-by-form-count form-obj) it "N/A")))
       (:td "N/A")
       (:td (:a :href (main-url form-obj) "Configuración"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM INFORMATION

(define-url-fn dashboard/form-info
  (let ((form (or (data/get-form (parameter "id"))
                  (redirect "/"))))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20101209"))
      (dashboard%render-form-title-and-links form)
      (with-form-tabs (form :form-info)
        (:section :id "form-info-run-button"
          (show-all-messages)
          (if (string= (form-status form) "active")
              (htm (:p "La evaluación se encuentra corriendo: ")
                   (:p :class "link"
                       (:a :target "_blank" :href (public-url form)
                           (esc (public-url form))))
                   (:p "Deberás enviar la liga mostrada arriba a todas
                        las personas que desees tomen parte en la evaluación.")
                   (:p "Para detener el proceso de evaluación deberás hacer
                        click en el siguiente botón:")
                   (:form :method "get" :action "/dashboard/deactivate-form"
                          (:p :class "button"
                              (hidden-input "id" :default-value (form-id form))
                              (submit-button "Detener evaluaciones"))))
              (htm
               (:p "La evaluación se encuentra en pausa, nadie podrá contestar
                    la evaluación mientras se encuentre pausada.")
               (:p "Puedes continuar editando la evaluación, agregando y
                    modificando preguntas y respuestas.")
               (:p "Cuando hayas terminado de diseñar tu evaluación y desees
                    comenzar a recibir respuestas haz click en el siguiente
                    botón:")
               (:form :method "get" :action "/dashboard/activate-form"
                      (:p :class "button"
                          (hidden-input "id" :default-value (form-id form))
                          (submit-button "Comenzar evaluaciones"))))))))))

(defun dashboard%render-form-title-and-links (form)
  (with-html-output (*standard-output*)
    (:section :id "form-info-title"
      (:div :class "title"
            (:h1 "Evaluación: "
                 (:span :class "title"
                        :title (escape-string (form-title form))
                        (esc (truncate-words (form-title form) 12))))
            (:p :class "dates"
                (:em "Fecha de creación: ")
                (:span :class "date"
                       (esc (format-date (form-date form))))
                (:em "Última modificación: ")
                (:span :class "date"
                       (esc (format-date (form-update-date form))))
                (if (string= (form-status form) "active")
                    (htm (:span :class "state-running" "Corriendo"))
                    (htm (:span :class "state-paused" "Pausada")))))
              (:div :class "links"
                    (:ul
                     (:li :class "edit"
                          (:a :href (edit-url form) "Modificar evaluación"))
                     (:li :class "preview"
                          (:a :href (preview-url form)
                              :target "_blank"
                              "Vista preliminar")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACTIVATE/DEACTIVATE FORM

(define-url-fn dashboard/activate-form
  (let ((form (or (data/get-form (parameter "id"))
                  (redirect "/"))))
    (let ((now (make-date (get-universal-time) (or time-zone 6))))
      (when (and (eql :post (request-method*))
                 (setf (form-status form) "active"
                       (form-start-date form) now)
                 (dashboard%process-form-options form))
        (redirect (main-url form))))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20101209"))
      (:form :method "post" :action "/dashboard/activate-form"
       (hidden-input "id" :default-value (form-id form))
       (dashboard%render-form-title-and-links form)
       (with-form-tabs (form :form-info)
         (:div :id "form-info-run-button"
               (:p "Al hacer click en el botón de abajo se activara la
                   evaluación; obtendrás una URL, la cual deberás mandar a todas
                   aquellas personas que desees tomen parte en la evaluación.")
               (:p :class "button" (submit-button "Activar evaluación")))
         (dashboard%render-form-options form
                                        :with-hidden-id nil
                                        :with-submit-button nil))))))

(define-url-fn dashboard/deactivate-form
  (let ((form (or (data/get-form (parameter "id"))
                  (redirect "/"))))
    (when (eql :post (request-method*))
      ;; TODO: save settings
      (setf (form-status form) "inactive")
      (data/save-form  form)
      (redirect (main-url form)))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20101209"))
      (:form :method "post" :action "/dashboard/deactivate-form"
       (hidden-input "id" :default-value (form-id form))
       (dashboard%render-form-title-and-links form)
       (with-form-tabs (form :form-info)
         (:div :id "form-info-run-button"
           (:p "¿Estas seguro(a) que deseas detener las evaluaciones? Al
                detener las evaluaciones nadie podrá enviar mas respuestas.")
           (:p :class "button"
                 (:a :href (main-url form) "Cancelar")
                 (submit-button "Detener evaluaciones"))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM OPTIONS

(define-url-fn dashboard/form-options
  (let ((form (or (data/get-form (parameter "id"))
                  (redirect "/"))))
    (when (and (eql :post (request-method*))
               (dashboard%process-form-options form))
      (push-success-msg "Las opciones se han guardado.")
      (redirect (options-url form)))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20101209"))
      (dashboard%render-form-title-and-links form)
      (with-form-tabs (form :form-options)
        (:form :method "post" :action "/dashboard/form-options"
               (dashboard%render-form-options form))))))

(defun dashboard%render-form-options (form &key (div.id "form-info-options")
                                      (with-hidden-id t)
                                      (with-submit-button t))
  (let ((tries (form-tries-limit form)))
    (with-html-output (*standard-output*)
      (:div :id (escape-string div.id)
       (when with-hidden-id
         (htm (hidden-input "id" :default-value (form-id form))))
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
  (let ((form (or (data/get-form (parameter "id"))
                  (redirect "/"))))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20110112"))
      (dashboard%render-form-title-and-links form)
      (with-form-tabs (form :form-stats)
        (:div :id "form-info-stats"
              (:h2 "Resumen de evaluaciones")
              (:div :class "stats"
                    (:p (str
                         (aif (data/get-submissions-by-form-count form)
                              (if (= 1 it)
                                  "1 evaluación enviada"
                                  (format nil "~:d evaluaciones enviadas" it))
                              "N/A"))
                        (let-when (age (age-in-days form))
                          (htm
                           (fmt " en el transcurso de ~:d día~:p." age)))))
              (dashboard%render-questions-stats form))))))

(defun dashboard%render-questions-stats (form)
  (dolist (question (form-questions form))
    (let ((count (data/get-submissions-by-question-count question))
          (question-text (question-text question)))
      (with-html-output (*standard-output*)
        (:div :class "question"
              (:div :class "question-title"
                    (:span :class "title"
                           :title (escape-string question-text)
                           (esc (format nil "~:d. ~a"
                                        (question-sort question)
                                        (truncate-words question-text 25))))
                    (:ul :class "question-options"
                         (:li (:a :href (stats-url question) "respuestas"))
                         (:li (:a :href "" "gráfica"))
                         (:li (:a :href "" "exportar")))
                    (:span :class "count" (fmt "~d respuesta~:p" count)))
              (:div :class "answers"
                    (dashboard%render-answers-stats (question-control question)
                                                    (question-answers question)
                                                    count)))))))

(defun dashboard%render-answers-stats (control answers submissions-count)
  (dolist (answer answers)
    (let* ((answer-count (data/get-submissions-by-answer-count answer))
           (percent (if (> submissions-count 0)
                        (floor (* 100 (/ answer-count submissions-count)))
                        0))
           (answer-text (answer-text answer)))
      (with-html-output (*standard-output*)
        (:div :class "answer"
              (:span :class "title"
                     :title (escape-string answer-text)
                     (esc (truncate-words answer-text 25)))
              (if (string= control "textarea")
                  (htm (:span :class "bar"
                              (:small "Haz click en " (:em "respuestas"))))
                  (htm
                   (:span :class "bar"
                          ;; A 100% bar is 300 pixels wide
                          (:span :style (format nil "width:~dpx;"
                                                (* 3 percent))))
                   (:span :class "stat-count" (fmt "~:d" answer-count))
                   (:span :class "stat-percent" (fmt "~d%" percent))))
              (:div :class "clear"))))))


(define-url-fn dashboard/form-question-stats
  (let* ((form (or (data/get-form (parameter "id")) (redirect "/")))
         (question-id (or (trim-or-nil (parameter "qid")) (redirect "/")))
         (question (or (find question-id (form-questions form) ; wasteful?
                             :key #'question-id :test #'string=)
                       (redirect "/")))
         (global-count (data/get-submissions-by-form-count form))
         (this-count (data/get-submissions-by-question-count question)))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                           :css-files ("dashboard.css?v=20110112"
                                       "tablesorter/blue/style.css")
                           :js-files ("jquery-1.4.2.min.js"
                                      "jquery.tablesorter.min.js"
                                      "dashboard-stats.js?v=20110113"))
      (dashboard%render-form-title-and-links form)
      (with-form-tabs (form :form-stats)
        (:div :id "form-info-stats"
              (:h2 "Respuestas enviadas")
              (:div :class "stats"
                    (:p
                     (fmt "~:d respuesta~:p enviada~:p de un total de"
                          this-count)
                     (if (= 1 global-count)
                         (str " 1 evaluación")
                         (fmt " ~:d evaluaciones" global-count))
                     (fmt " (~d%)" (floor (* 100 (/ this-count global-count))))
                     (let-when (age (age-in-days form))
                       (htm
                        (:br (fmt "En el transcurso de  ~:d día~:p." age))))))
              (:p (:a :class "return"
                      :href (stats-url form)
                      "Regresar a resumen general"))
              (dashboard%render-a-question-stats form question)
              (:p (:a :class "return"
                      :href (stats-url form)
                      "Regresar a resumen general")))))))

(defun dashboard%render-a-question-stats (form question)
  (let* ((question-text (question-text question))
         (submissions (data/get-submissions-by-form form))
         (answer-texts (loop for a in (question-answers question)
                             collect (cons (answer-id a) (answer-text a))))
         (submitted-answers
          (loop with table = (make-hash-table :test 'equal)
                for (id keys values) in (data/get-submitted-answers-by-question
                                             question)
                for key = (third keys)
                do (setf (gethash key table) (cdr values))
                finally (return table)))
         (submitted-count (data/get-submissions-by-question-count question)))
    (with-html-output (*standard-output*)
      (:div :class "question"
            (:div :class "question-title"
                  (:span :class "title"
                         :title (escape-string question-text)
                         (esc (format nil "~:d. ~a"
                                      (question-sort question)
                                      (truncate-words question-text 25))))
                  (:ul :class "question-options"
                       (:li (:a :href (stats-url question) "respuestas"))
                       (:li (:a :href "" "gráfica"))
                       (:li (:a :href "" "exportar")))
                  (:span :class "count" (fmt "~d respuesta~:p" submitted-count)))
            (:table :id "id-table-stats"
                    :class "tablesorter"
                    :cellspacing 1 :cellpadding 0
                    (table-columns "Fecha" "Respuesta(s)" "ip")
                    (:tbody
                     (loop for submission in submissions
                           for answers = (gethash (submission-id submission)
                                                  submitted-answers)
                           when answers
                             do (dashboard%render-submission
                                          submission
                                          answers
                                          answer-texts
                                          (question-control question)))))))))

(defun dashboard%render-submission (submission answers answer-texts type)
  (with-html-output (*standard-output*)
    (:tr
     (:td (esc (format-iso8601-date (submission-finish-date submission))))
     (:td
      (:ul :class (format nil "~a" type)
           (loop for ans in answers
                 for ansid = (cdr (assoc :|answer| ans))
                 do
              (aif (cdr (assoc :|value| ans))
                   (htm (:li :title (escape-string it)
                             (esc (truncate-words it 25))))
                   (let ((text (cdr (assoc ansid answer-texts
                                           :test #'string=))))
                     (htm (:li :title (escape-string text)
                               (esc (truncate-words text 25)))))))))
     (:td (esc (submission-ip submission))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM DOWNLOAD OPTIONS

(define-url-fn dashboard/download
  (let* ((form (or (data/get-form (parameter "id"))
                   (redirect "/")))
         (submissions (data/get-submissions-by-form form))
         (questions (form-questions form)))
    (standard-page (:title (format nil "Evaluación: ~a" (form-title form))
                    :css-files ("dashboard.css?v=20101209"
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
                  for answers = (data/get-submitted-answers-by-submission-question
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
                         for answers = (data/get-submitted-answers-by-submission-question
                                        (submission-id ss)
                                        (question-id question))
                         collect (funcall replacer
                                  (format nil "~{~a~^ | ~}"
                                          (%collect-answers answers))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACCOUNT PAGE

(define-url-fn dashboard/account
  (when (eql :post (request-method*))
    (let ((full-name (trim-or-nil (parameter "full-name")))
          (email (trim-or-nil (parameter "email")))
          (current-password (trim-or-nil (parameter "current-password")))
          (new-password (trim-or-nil (parameter "new-password")))
          (confirm-password (trim-or-nil (parameter "confirm-password"))))
      (when (or full-name email)
        (setf (user-full-name the-user) full-name
              (user-email the-user) email)
        (data/save-user the-user)
        (push-success-msg "Los cambios se han guardado." :group :basic)
        (redirect "/dashboard/account"))
      (when (and (or current-password new-password confirm-password)
                 (dashboard%process-account-change-password current-password
                                                            new-password
                                                            confirm-password
                                                            the-user))
        (redirect "/dashboard/account"))))
  (standard-page (:title "Mi cuenta"
                  :css-files ("dashboard.css?v=20101209"))
    (:section :id "account"
      ;; TODO: place messages here or in the password change section
      ;; below, depending on action taken.
      (show-all-messages :group :basic)
      (:form :method "post" :action "/dashboard/account"
             (:header (:h1 "Mi cuenta"))
             (:div (text-input "Nombre:" "full-name"
                               :default-value (user-full-name the-user)))
             (:div (text-input "E-Mail:" "email"
                               :default-value (user-email the-user)))
             (:div (submit-button "Guardar"))))
    (:section :id "account-password"
      (:form :method "post" :action "/dashboard/account"
             (show-all-messages :group :password)
             (:header (:h1 "Contraseña"))
             (:p "Para cambiar tu contraseña introduce la contraseña actual y
                 luego introduce la contraseña nueva dos veces:")
             (:div (password-input "Actual:" "current-password"))
             (:div (password-input "Nueva:" "new-password"))
             (:div (password-input "Confirmación:" "confirm-password"))
             (:div (submit-button "Cambiar"))))))

(defun dashboard%process-account-change-password (current-password
                                                  new-password
                                                  confirm-password
                                                  the-user)
  ;;
  ;; Check some basics
  (unless current-password
    (push-error-msg "La contraseña actual es requerida." :group :password)
    (return-from dashboard%process-account-change-password nil))
  (unless new-password
    (push-error-msg "La nueva contraseña no puede estar vacía" :group :password)
    (return-from dashboard%process-account-change-password nil))
  (unless (string= new-password confirm-password)
    (push-error-msg "La confirmación de la contraseña no es correcta"
                    :group :password)
    (return-from dashboard%process-account-change-password nil))
  ;;
  ;; Basics ok, calculate digest and confirm current password.
  (let ((curr-digest (hunchentoot::md5-hex current-password))
        (new-digest (hunchentoot::md5-hex new-password)))
    (unless (string= curr-digest (user-password-digest the-user))
      (push-error-msg "La contraseña actual no es correcta" :group :password)
      (return-from dashboard%process-account-change-password nil))
    ;;
    ;; It seems everything is ok... GO!
    (setf (user-password-digest the-user) new-digest)
    (data/save-user the-user)
    (push-success-msg "La contraseña se ha cambiado." :group :password)
    the-user))