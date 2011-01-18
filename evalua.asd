(defpackage #:evalua-system (:use :common-lisp :asdf))

(in-package #:evalua-system)

(defsystem #:evalua
  :description ""
  :version "0.1"
  :author "Jorge Gajon <gajon@gajon.org>"
  :licence ""
  :serial t
  :components ((:file "packages")
               (:file "patches")
               (:module "src"
                :serial t
                :components ((:file "lol")
                             (:file "globals")
                             (:file "utils")
                             (:file "model")
                             (:file "data")
                             (:file "web")
                             (:file "design")
                             (:file "public")
                             (:file "account")
                             (:file "dashboard"))))
  :depends-on (:hunchentoot
               :cl-who
               :clouchdb
               :kmrcl
               :split-sequence))

;;; vim: set filetype=lisp:
