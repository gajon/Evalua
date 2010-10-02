(in-package #:cl-user)

(defpackage #:evalua
  (:use :common-lisp :hunchentoot :cl-who)
  (:import-from :kmrcl :awhen :it)
  (:shadow :start :stop)
  (:export :start :stop))
