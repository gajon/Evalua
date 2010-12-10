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
  (standard-page (:title "Evalua.mx - La manera más fácil y rápida de crear
                         evaluaciones en línea."
                  :css-files ("index.css?v=20101209")
                  :show-options nil
                  :include-analytics-p t)
    ;;
    ;; Middle Section, main benefits.
    ;;
    (:section :id "middle-section"
      (:div :class "left"
        (:h1 "La manera más fácil y rápida de crear evaluaciones en línea.")
        (:hr)
        (:ul
          (:li "Evalúa y refuerza los procesos de ingreso y
               capacitación laboral.")
          (:li "Aplica encuestas de satisfacción y mejora a empleados y
               clientes de tu empresa.")
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
      (:div :class "clear"))
    ;(:a :href "/design/create-new-form" (:span "Crea una evaluación"))
    ;;
    ;; More benefits and list of features.
    ;;
    (:section :id "more-features"
      (:div :class "diagram"
            (:img :src "/static/img/comofunciona.png"))
      (:div :class "firstcolumn"
        (:ul
          (:li "Diseña tu evaluación, creando preguntas y sus respuestas.")
          (:li "Es muy sencillo de utilizar, no necesitas conocimientos
               técnicos.")
          (:li "No es necesario instalar ningún software.")
          (:li "Se adapta a tus necesidades: número y tipo de preguntas,
               intentos permitidos, tiempo límite, etc.")))
      (:div :class "secondcolumn"
        (:ul
          (:li "Tus empleados, clientes o alumnos contestan la evaluación a
               través de una página web.")
          (:li "Sin publicidad, sin distracciones, sólo el logotipo de tu
               empresa.")
          (:li "Envíalo por correo electrónico o publícalo en tu sitio web.")))
      (:div :class "thirdcolumn"
        (:ul
          (:li "Recibe de manera inmediata la retroalimentación y
               calificación de los evaluados.")
          (:li "Almacena la evaluación y los resultados para consultas
               futuras, o para crear nuevas evaluaciones.")
          (:li "Obtén reportes con calificaciones promedio, más alta, más
               baja, número de aprobados y reprobados, tiempo promedio de
               respuesta, número de aciertos y errores por pregunta, número
               de evaluados por rangos de calificación, distribución de
               resultados por pregunta, etc.")))
      (:div :class "clear"))
    (:script
      #>SCRIPT
      var timezoneInput = document.getElementById("id_timezone");
      timezoneInput.value = (new Date()).getTimezoneOffset() / 60;
      SCRIPT)))

(define-url-fn (wait-registry :auth nil)
  ;; TODO: We are not storing the timezone in the session.
  (let* ((email (trim-or-nil (post-parameter "email")))
        (timezone (parse-int-or-force-value (post-parameter "timezone") 6))
        (now (make-date (get-universal-time) (or timezone 6))))
    ;; If there was POST data and an email, GOOD! Otherwise I don't care,
    ;; we'll continue as if everything was fine.
    ;; Specially because we want to clear the POST data with a redirect.
    (when (and email (eql :post (request-method*)))
      (data/add-wait-registry email now (user-agent) (real-remote-addr))
      (redirect "/wait-registry")))
  (standard-page (:title "Evalua.mx - La manera más fácil y rápida de crear
                         evaluaciones en línea."
                  :css-files ("index.css?v=20101209")
                  :show-options nil
                  :include-analytics-p t)
    (:section :id "wait-registry"
      (:h1 "Gracias por tu interés.")
      (:p "Nosotros te avisaremos por correo electrónico tan pronto el
          producto se encuentre listo.")
      (:p "Te queremos recordar que tu dirección de correo electrónico es
          completamente confidencial y no será compartida con nadie, ni será
          utilizada para mandarte correo no deseado."))))

(define-url-fn (login :auth nil)
  (when (eq :post (request-method*))
    (let ((username (trim-or-nil (post-parameter "username")))
          (password (trim-or-nil (post-parameter "password")))
          (timezone (parse-int-or-force-value (post-parameter "timezone") 6)))
      (if (and (require-fields username password)
               (or (data/validate-credentials username password)
                   (push-error-msg "El usuario o contraseña son incorrectos.")))
        (setf (session-value 'authenticated) "yes"
              (session-value 'username) username
              (session-value 'timezone) timezone)
        ;; Invalid credentials
        (setf (session-value 'authenticated) nil))))
  (when (string= (session-value 'authenticated) "yes")
    (redirect "/dashboard"))
  (standard-page (:title "Evalua.mx - La manera más fácil y rápida de crear
                         evaluaciones en línea."
                  :css-files ("index.css?v=20101209")
                  :show-options nil)
    (:section :id "login"
     (show-all-messages)
     (:form :method "post" :action "/login"
      (hidden-input "timezone")
      (:div (text-input "Usuario:" "username"))
      (:div (password-input "Contraseña:" "password"))
      (:div (submit-button "Entrar"))))
    (:script
      #>SCRIPT
      var usernameInput = document.getElementById("id_username");
      usernameInput.focus();
      usernameInput.select();

      var timezoneInput = document.getElementById("id_timezone");
      timezoneInput.value = (new Date()).getTimezoneOffset() / 60;
      SCRIPT)))

(define-url-fn (logout :auth nil)
  (setf (session-value 'authenticated) nil
        (session-value 'username) "")
  (redirect "/"))
