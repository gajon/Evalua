(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACCOUNT PAGE


(define-url-fn account
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
        (redirect "/account"))
      (when (and (or current-password new-password confirm-password)
                 (process-account-change-password current-password
                                                  new-password
                                                  confirm-password
                                                  the-user))
        (redirect "/account"))))
  (standard-page (:title "Mi cuenta"
                  :css-files ("dashboard.css?v=20101209"))
    (:section :id "account"
      (show-all-messages :group :basic)
      (:form :method "post" :action "/account"
             (:header (:h1 "Mi cuenta"))
             (:div (text-input "Nombre:" "full-name"
                               :default-value (user-full-name the-user)))
             (:div (text-input "E-Mail:" "email"
                               :default-value (user-email the-user)))
             (:div (submit-button "Guardar"))))
    (:section :id "account-password"
      (:form :method "post" :action "/account"
             (show-all-messages :group :password)
             (:header (:h1 "Contraseña"))
             (:p "Para cambiar tu contraseña introduce la contraseña actual y
                 luego introduce la contraseña nueva dos veces:")
             (:div (password-input "Actual:" "current-password"))
             (:div (password-input "Nueva:" "new-password"))
             (:div (password-input "Confirmación:" "confirm-password"))
             (:div (submit-button "Cambiar"))))))


(defun process-account-change-password (current-password
                                        new-password
                                        confirm-password
                                        the-user)
  ;; Check some basics
  (unless current-password
    (push-error-msg "La contraseña actual es requerida." :group :password)
    (return-from process-account-change-password nil))
  (unless new-password
    (push-error-msg "La nueva contraseña no puede estar vacía" :group :password)
    (return-from process-account-change-password nil))
  (unless (string= new-password confirm-password)
    (push-error-msg "La confirmación de la contraseña no es correcta"
                    :group :password)
    (return-from process-account-change-password nil))
  ;; Basics ok, calculate digest and confirm current password.
  (let ((curr-digest (hunchentoot::md5-hex current-password))
        (new-digest (hunchentoot::md5-hex new-password)))
    (unless (string= curr-digest (user-password-digest the-user))
      (push-error-msg "La contraseña actual no es correcta" :group :password)
      (return-from process-account-change-password nil))
    ;; It seems everything is ok... GO!
    (setf (user-password-digest the-user) new-digest)
    (data/save-user the-user)
    (push-success-msg "La contraseña se ha cambiado." :group :password)
    the-user))