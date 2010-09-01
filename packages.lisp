(in-package #:cl-user)

(defpackage #:evalua
  (:use :common-lisp :hunchentoot :cl-who)
  (:shadow :start :stop)
  (:export :start :stop))
