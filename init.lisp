;;;; This file is used to launch the project from a bash script, you can use
;;;; something like the following:
;;;
;;; setenv SBCL_HOME "/usr/lib/sbcl"
;;; chdir /path/to/Lisp/Evalua
;;; screen -t Evalua sbcl --no-userinit --load /path/to/Lisp/Evalua/init.lisp

(require :sb-aclrepl)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "evalua")
(evalua:start)



(ql:quickload "swank")
;(setf swank-loader::*contribs* '(swank-c-p-c
;                                  swank-arglists
;                                  swank-fuzzy swank-fancy-inspector
;                                  swank-package-fu))
(swank-loader::loadup)
(swank:create-server :port 7781
                     :dont-close t
                     :coding-system "utf-8-unix")
