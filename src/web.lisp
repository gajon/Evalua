(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start and Stop the Server

;;; These start/stop functions were initially lifted from:
;;; http://github.com/smanek/trivial-lisp-webapp (Shaneal Manek)

(defvar *evalua-acceptor-instance* nil)

(defun start ()
  (kmrcl:seed-random-generator)
  (unless *evalua-acceptor-instance*
    (setf *evalua-acceptor-instance*
          (make-instance 'hunchentoot:acceptor :port *server-port*))
    (hunchentoot:start *evalua-acceptor-instance*)
    (format t "Webserver started on port ~A.~%" *server-port*)))

(defun stop ()
  (when *evalua-acceptor-instance*
    (format t "Shutting down")
    (hunchentoot:stop *evalua-acceptor-instance*)
    (setf *evalua-acceptor-instance* nil)
    #+ImDebugging (portable-quit)))

(defun portable-quit ()
  #+allegro(excl:exit)
  #+sbcl(sb-ext:quit)
  #+clisp(ext:quit)
  #-(or allegro sbcl clisp)(error "don't know how to quit."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INDEX PAGE

(define-index-fn
  ;;
  ;;
  (standard-page (:title "Evalua.mx - La manera más fácil y rápida de crear evaluaciones en línea."
                  :css-files ("index.css"))
    ;;
    ;; Middle Section, main benefits.
    ;;
    (:section :id "middle-section"
      (:div :class "left"
        (:h1 "La manera más fácil y rápida de crear evaluaciones en línea.")
        (:hr)
        (:ul
          (:li "Evalúa y refuerza los procesos de ingreso y capacitación laboral.")
          (:li "Aplica encuestas de satisfacción y mejora a empleados y clientes de tu empresa.")
          (:li "Aplica exámenes y ejercicios de práctica a tus alumnos.")))
      (:div :class "right"
        (:img :src "/static/img/screenshot.png" :alt "screenshot"))
      (:div :class "clear"))
    ;;
    ;; Big F Button, call to action.
    ;;
    (:section :id "big-f-button"
      (:form :method "post" :action "/wait-registry"
        (hidden-input "timezone")
        (text-input "Envíame un email cuando el producto esté listo:" "email")
        (submit-button "Enviar"))
      (:div :class "clear")) ;(:a :href "/design" (:span "Crea una evaluación")))
    ;;
    ;; More benefits and list of features.
    ;;
    (:section :id "more-features"
      (:div :class "left"
        (:h2 "¿Cómo funciona?")
        (:p "Evalua.mx es muy sencillo de utilizar:")
        (:ul
          (:li "Diseña tu evaluación, creando preguntas y sus respuestas.")
          (:li "Envíalo por correo electrónico o publícalo en tu página web.")
          (:li "Monitorea las calificaciones y estadísticas.")))
        ;(:p (:a :href "#" "Ver mas..")))
      (:div :class "right"
        (:h2 "Características")
        (:ul
          (:li "Diseña y crea tu evaluación o cuestionario en línea sin
               necesidad de saber programar.")
          (:li "Se adapta a tus necesidades: número y tipo de preguntas,
               intentos permitidos, tiempo límite, etc.")
          (:li "Recibe de manera inmediata la retroalimentación y
               calificación de los evaluados.")
          (:li "Almacena la evaluación y los resultados para consultas
               futuras, o para crear nuevas evaluaciones.")
          (:li "Envía la evaluación a través de correo electrónico o
               publícala en tu página de Internet, Blog, Facebook, Twitter,
               etc.")
          (:li "Obtén reportes con calificaciones promedio, más alta, más
               baja, número de aprobados y reprobados, tiempo promedio de
               respuesta, número de aciertos y errores por pregunta, número
               de evaluados por rangos de calificación, distribución de
               resultados por pregunta, etc.")))
        ;(:p (:a :href "#" "Ver mas..")))
      (:div :class "clear"))
    (:script
      #>SCRIPT
      var timezoneInput = document.getElementById("id_timezone");
      timezoneInput.value = (new Date()).getTimezoneOffset() / 60;
      SCRIPT)))

(define-url-fn wait-registry
  ;; TODO: We are not storing the timezone in the session.
  (let* ((email (trim-or-nil (post-parameter "email")))
        (timezone (parse-int-force-pos-or-zero (post-parameter "timezone")))
        (now (make-date (get-universal-time) (or timezone 6))))
    ;; If there was POST data and an email, GOOD! Otherwise I don't care,
    ;; we'll continue as if everything was fine.
    ;; Specially because we want to clear the POST data with a redirect.
    (when (and email (eql :post (request-method*)))
      (data/add-wait-registry email now (user-agent) (real-remote-addr))
      (redirect "/wait-registry")))
  (standard-page (:title "Evalua.mx - La manera más fácil y rápida de crear evaluaciones en línea."
                  :css-files ("index.css"))
    (:section :id "wait-registry"
      (:h1 "Gracias por tu interés.")
      (:p "Nosotros te avisaremos por correo electrónico tan pronto el
          producto se encuentre listo.")
      (:p "Te queremos recordar que tu dirección de correo electrónico
          es completamente confidencial y 
          no será compartida con nadie, ni será utilizada para mandarte correo
          no deseado."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORM DESIGN PAGE.

(define-url-fn design
  ;; We expect a POST parameter "timezone" sent from this form.
  ;; This value will be used in the function 'backend-save-form' to
  ;; calculate the date and time of the new form.
  (when (eql :post (request-method*))
    (let ((ptimezone (parse-int-force-pos-or-zero
                       (trim-or-nil (post-parameter "timezone")))))
      (setf (session-value 'timezone) ptimezone
            time-zone ptimezone)))
  ;;
  (standard-page (:title "Paso 1. Diseña tu cuestionario."
                  :css-files (;;;;"jquery-ui-1.8.2.custom.css"
                              "design-styles.css")
                  :js-files ("jquery-1.4.2.min.js"
                             ;;;;;"jquery-ui-1.8.2.custom.min.js"
                             "json2.min.js"
                             "design.js"))
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
    (:section :id "questions-submit" (submit-button "Enviar"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAVE FORM DESIGN.

(define-json-fn backend-save-form
  ;; The handler-case is to make it easy for us to bail at any point
  ;; while processing this request.
  (handler-case
    (progn
      (unless (eql :post (request-method*))
        (error "Post method is required."))
      (let ((form (process-save-form
                    (trim-or-nil (post-parameter "title"))
                    (trim-or-nil (post-parameter "notes"))
                    (trim-or-nil (post-parameter "questions"))
                    time-zone)))
        (if form
          (htm (str (clouchdb:document-to-json
                      `((:|status| . "ok") (:|id| . ,(form-id form))))))
          (error "Form couldn't be created."))))
    ;;
    ;; Oops.
    ;;
    (error (c)
           (htm (str (clouchdb:document-to-json
                       `((:|status| . "error")
                         (:|condition| . ,(format nil "~a" c)))))))))

(defun process-save-form (title notes json-questions time-zone)
  (let* ((title (or title (error "Form title is required.")))
         (decoded-data (clouchdb:json-to-document json-questions))
         (now (make-date (get-universal-time) (or time-zone 6)))
         (new-form
           (add-form
             (make-instance 'form
                            :user "gajon@gajon.org"
                            :date (format-iso8601-date now)
                            :valid-date (format-iso8601-date now)
                            :title title
                            :notes notes
                            :time-zone (or time-zone 6))
             (numerate-questions-and-answers decoded-data))))
    new-form))

(defun numerate-questions-and-answers (data)
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


(define-url-fn create-new-form-step-2
  (standard-page (:title "Paso 2. Configura las opciones."
                  :css-files ("design-styles.css"))
    (:form :method "post" :action "/form-info/"
      (hidden-input "id" :default-value (parameter "id"))
      (:section :id "options"
        (:h1 "Paso 2. Configura las opciones")
        (:div :id "time" (text-input "Tiempo límite:" "tiempo"))
        (:div :id "tries" (text-input "Intentos permitidos:" "intentos"))
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
                       :escape-label nil)))))

(define-url-fn form-info
  (let* ((form (or (get-form (parameter "id"))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RENDER A PUBLIC FORM.

(define-url-fn (a :prefix "a")
  (let ((form (or (get-form-by-public-id (parameter "id"))
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
  (let ((form (or (get-form-by-public-id (parameter "id"))
                  (redirect "/"))))
    (standard-page (:title "Gracias"
                    :show-banner nil
                    :css-files ("public-styles.css"))
      (:section :id "form-title"
        (:header (:h1 (esc (form-title form)))))
      (:section :id "thankyou"
        (:h2 "Gracias por contestar el cuestionario.")))))
