(in-package #:evalua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server Port and Directories

(defvar *server-port* 8081)
(defvar *webapp-home* (asdf:component-relative-pathname
                       (asdf:find-system :evalua)))
(defvar *static-web-files* (merge-pathnames
                            (make-pathname :directory '(:relative "static"))
                            *webapp-home*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other Hunchentoot Settings

(push
  (create-folder-dispatcher-and-handler "/static/" *static-web-files*)
  *dispatch-table*)

(setf *access-log-pathname*
      (merge-pathnames "access.txt" *webapp-home*))

(setf *message-log-pathname*
      (merge-pathnames "messages.txt" *webapp-home*))

(setf *default-content-type* "text/html; charset=UTF-8")

(setf *hunchentoot-default-external-format* hunchentoot::+utf-8+)
;(flexi-streams:make-external-format :utf-8)

;; Sessions time out after 12 hours of no use.
(setf *session-max-time* (* 12 60 60))

(setf *show-lisp-errors-p* t)
(setf *catch-errors-p* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CouchDB Settings

(clouchdb:set-connection :db-name "evalua")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings for CL-WHO

(setf (cl-who:html-mode) :SGML)

;;; We want HTML5 Doctype
(setf cl-who:*prologue* "<!DOCTYPE html>")

