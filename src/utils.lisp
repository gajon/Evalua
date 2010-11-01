(in-package #:evalua)

;;; See: http://www.sbcl.org/manual/Defining-Constants.html
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                ,@(when doc (list doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MACROS TO DEFINE PAGES

;;; These macros to define url handlers were initially taken, and then
;;; modified from: http://www.adampetersen.se/articles/lispweb.htm
;;; Thank you Adam Petersen.
;(defmacro define-url-fn (name &body body)
;  `(progn
;     (defun ,name ()
;       (let ((the-user (get-user-obj (session-value 'username))))
;         ;; It could happen that the user was not found but the session
;         ;; has the 'authenticated value set if the user was deleted from
;         ;; the database.
;         (unless (and the-user (string= (session-value 'authenticated) "yes"))
;           (setf (session-value 'authenticated) nil)
;           (redirect "/"))
;         ;;
;         ;; Yay, we have a valid user, let's set the time zone.
;         (let ((time-zone (setf (user-time-zone the-user)
;                                (session-value 'timezone))))
;           (declare (ignorable time-zone))
;           (no-cache) ; Prevent caching on most browsers.
;           ,@body)))
;     (push (create-prefix-dispatcher ,(format nil "/~(~a~)/" name)
;                                     ',name)
;           *dispatch-table*)))

;;; TODO: there should be a way to combine these `define-*-fn` macros
;;;       into a single one.
(defmacro define-url-fn (name-and-options &body body)
  "This macro is like `define-url-fn` macro but does not try to see
if there's a valid session. The variables `the-user` and `time-zone` are
still being inserted into the lexical context but their values are nil.
BE CAREFUL."
  (flet ((gen-body (name &key prefix (auth t))
           `(progn
              ;;;
              ;;; The handler function
              ;;;
              (defun ,name ()
                (let ((the-user (data/get-user (session-value 'username)))
                      (time-zone (session-value 'timezone)))
                  (declare (ignorable the-user time-zone))
                  ;; Enforce authentication when :auth t
                  ,(when auth
                     `(unless (and the-user
                                   (string= (session-value 'authenticated)
                                            "yes"))
                        (setf (session-value 'authenticated) nil)
                        (redirect "/")))
                  ;; Yay, we have a valid user, let's set the time zone.
                  (when time-zone
                    (setf (user-time-zone the-user) time-zone))
                  ,@body))
              ;;;
              ;;; Update Hunchentoot's dispatch table.
              ;;;
              (push (create-prefix-dispatcher
                      ,(if (stringp prefix)
                         (if (char= (char prefix 0) #\/)
                           prefix
                           (format nil "/~(~a~)" prefix))
                         (format nil "/~(~a~)" name))
                      ',name)
                    *dispatch-table*))))
    (if (consp name-and-options)
      (apply #'gen-body name-and-options)
      (gen-body name-and-options))))

(defmacro define-json-fn (name-and-options &body body)
  `(define-url-fn ,name-and-options
     (with-html-output-to-string (*standard-output* nil
                                                    :prologue nil
                                                    :indent nil)
       ;; We must return a json object.
       (setf (content-type*) "application/json")
       ,@body)))

(defmacro define-index-fn (&body body)
  (let ((index-fn (gensym)))
    `(progn
       (defun ,index-fn ()
         (let (the-user time-zone)
           (declare (ignorable the-user time-zone))
           ,@body))
       (setf hunchentoot::*default-handler* #',index-fn))))


;;; CAUTION, We expect to capture the free variable `the-user`.
(defmacro standard-page ((&key (title "")
                               (show-banner t)
                               (show-options t)
                               (show-footer t)
                               (include-analytics-p nil)
                               css-files js-files)
                         &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent nil)
     (:html
       (:head
         (:meta :charset "utf-8")
         (:title (esc ,title))
         ;;
         ;; CSS files
         ;;
         (:link :type "text/css"
                :rel "stylesheet"
                :media "screen, projection"
                :href "/static/css/html5reset-1.4.1.css")
         (:link :type "text/css"
                :rel "stylesheet"
                :media "screen, projection"
                :href "/static/css/common.css?v=20101004")
         ,@(mapcar (lambda (file)
                     `(:link :type "text/css" :rel "stylesheet"
                             :media "screen, projection"
                             :href ,(format nil"/static/css/~a" file)))
                   css-files)
         ;;
         ;; JavaScript files
         ;;
         ,@(mapcar (lambda (file)
                     `(:script :type "text/javascript"
                               :src ,(format nil "/static/js/~a" file)))
                   js-files)
         "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
       (:body
         (:div :id "body-container"
           ,(when show-banner
              `(:section :id "banner"
                 (:div :class "logo"
                       (:a :href "/" (:span "Evalua.mx")))
                 (:div :class "options"
                   ,(when show-options
                      `(:ul
                         (:li (:div :class "search"
                                (:input :type "text" :name "search")))
                         (:li (:a :href "/dashboard" "Tablero"))
                         (:li (:a :href "/dashboard/account" "Mi cuenta"))
                         (:li (:a :href "#" "Opciones"))
                         (:li (:a :href "/logout" "Salir")))))))
           (:section :id "content"
             ,@body)
           ,(when show-footer
              `(:section :id "footer"
                 (:p
                     (:a :href "#" "Términos y Condiciones")
                     (:a :href "#" "Precios")
                     (:a :href "#" "Contacto")
                     "&copy; 2010 Evalua.mx")
                 (:div :class "clear"))))
         ,(when include-analytics-p
            #| Include your analytics code below... |#
#>ANALYTICS
<script type="text/javascript">
var _gaq = _gaq || [];
_gaq.push(['_setAccount', 'UA-18398503-1']);
_gaq.push(['_trackPageview']);
(function() {
var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
})();
</script>
ANALYTICS
            #| End of analytics code |#)))))

(defmacro with-tabbed-page ((form-id &key current) &body body)
  `(with-html-output (*standard-output*)
     (:div :id "tabbed-navigation"
           (:div :id "tabbed-navigation-tabs"
                 (:ul
                  (:li ,@(when (eql current :form-info) `(:class "current"))
                       (:a :href (format nil "/dashboard/form-info?id=~a"
                                         ,form-id)
                           "Información"))
                  (:li ,@(when (eql current :form-options) `(:class "current"))
                       (:a :href (format nil "/dashboard/form-options?id=~a"
                                         ,form-id)
                           "Opciones"))
                  (:li ,@(when (eql current :form-stats) `(:class "current"))
                       (:a :href (format nil "/dashboard/form-stats?id=~a"
                                         ,form-id)
                           "Estadísticas"))
                  (:li ,@(when (eql current :form-download) `(:class "current"))
                       (:a :href (format nil "/dashboard/form-download?id=~a"
                                         ,form-id)
                           "Exportar"))))
           (:div :id "tabbed-navigation-content"
                 ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUEUE AND SHOW ERROR/SUCCESS MESSAGES TO THE USER

(defun push-error-msg (msg &key group)
  (push (cons group msg) (session-value 'error-msgs))
  nil)

(defun push-success-msg (msg &key group)
  (push (cons group msg) (session-value 'success-msgs)))

(defun push-info-msg (msg &key group)
  (push (cons group msg) (session-value 'info-msgs)))

(defun %show-messages (messages group css-id)
  (let ((msgs (if group
                  (filter (lambda (x) (when (eql group (car x)) x))
                          messages)
                  messages)))
    (when msgs
      (with-html-output (*standard-output*)
        (:div :id (escape-string css-id)
              (:ul (mapcar
                    (lambda (msg) (htm (:li (esc (cdr msg)))))
                    msgs)))))))

(defun %clear-messages (group session-key)
  (if group
      ;; Filter out those in the group.
      (setf (session-value session-key)
            (filter (lambda (x) (unless (eql group (car x)) x))
                    (session-value session-key)))
      ;; Erase all
      (setf (session-value session-key) nil)))

(defun show-error-messages (&key group)
  (awhen (session-value 'error-msgs)
    (%show-messages (reverse it) group "errors")
    (%clear-messages group 'error-msgs)))

(defun show-success-messages (&key group)
  (awhen (session-value 'success-msgs)
    (%show-messages (reverse it) group "success")
    (%clear-messages group 'success-msgs)))

(defun show-info-messages (&key group)
  (awhen (session-value 'info-msgs)
    (%show-messages (reverse it) group "messages")
    (%clear-messages group 'info-msgs)))

(defun show-all-messages (&key group)
  (show-error-messages :group group)
  (show-info-messages :group group)
  (show-success-messages :group group))

(defmacro! require-fields (&rest args)
  ;; TODO: Add group keyword.
  `(let ((,g!success t))
     (flet ((,g!failed (msg)
               (setf ,g!success nil)
               (push-error-msg msg)))
       ,@(mapcar
           (lambda (arg)
             (if (consp arg)
               `(or ,(car arg) (,g!failed ,(cadr arg)))
               `(or ,arg (,g!failed
                           ,(format nil "The ~(~a~) is required."
                                    (#~s/-/ / (symbol-name arg)))))))
           args)
       ,g!success)))

;(macroexpand-1 '(require-fields username password))
;(macroexpand-1 '(require-fields username password-confirmation))
;(macroexpand-1 '(require-fields (foo "The field foo is lucky.") bar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITY MACROS

(defmacro table-columns (&rest args)
  `(with-html-output (*standard-output*)
     (:thead
       (:tr
         ,@(loop for a in args collect
                 (if (consp a)
                   `(:th ,@(cdr a) (esc ,(car a)))
                   `(:th (esc ,a))))))))

(defun text-input (label name &key default-value (size 20) disabled
                         labelclass inputclass (type "text"))
  (let* ((name (escape-string name))
         (label (escape-string label))
         (id (format nil "id_~a" name))
         (default-value (or (escape-string (post-parameter name))
                            (escape-string default-value))))
    (with-html-output (*standard-output*)
      (when label
        (htm (:label :for id
                     :class labelclass
                     (str label))))
      (:input :type type
              :id id
              :name name
              :value default-value
              :size size
              :class inputclass
              :disabled disabled))))

(defun password-input (label name &key default-value (size 20) disabled
                             labelclass inputclass)
  (text-input label name :default-value default-value :size size
              :disabled disabled :labelclass labelclass
              :inputclass inputclass :type "password"))

(defun hidden-input (name &key default-value)
  (let* ((name (escape-string name))
         (id (format nil "id_~a" name))
         (default-value (or (escape-string (post-parameter name))
                            (escape-string default-value))))
    (with-html-output (*standard-output*)
      (:input :type "hidden"
              :id id
              :name name
              :value default-value))))

(defun text-area (label name &key default-value (cols 30) (rows 7)
                  disabled labelclass inputclass)
  (let* ((name (escape-string name))
         (label (escape-string label))
         (id (format nil "id_~a" name))
         (default-value (or (escape-string (post-parameter name))
                            (escape-string default-value))))
    (with-html-output (*standard-output*)
      (when label
        (htm (:label :for id :class labelclass (str label))))
      (:textarea :id id
                 :name name
                 :cols cols
                 :rows rows
                 :class inputclass
                 :disabled disabled
        (str default-value)))))

(defun button (label name &key disabled submit-p inputclass (escape-label t))
  (let* ((name (escape-string name))
         (label (if escape-label (escape-string label) label))
         (id (format nil "id_~a" name)))
    (with-html-output (*standard-output*)
      (:button :type (if submit-p "submit" "button")
               :id id
               :value label
               :name name
               :disabled disabled
               :class inputclass
               (str label)))))

(defun submit-button (label &key (name "submit") disabled inputclass
                            (escape-label t))
  (button label name :disabled disabled :submit-p t :inputclass inputclass
          :escape-label escape-label))

(defun checkbox-choice (label name value &key disabled current-value
                              labelclass inputclass (type "checkbox"))
  (let* ((name (escape-string name))
         (label (escape-string label))
         (id (format nil "id_~a" (gensym name)))
         (disabled (and disabled "disabled"))
         (value (escape-string value))
         (checked (or (find-if (lambda (x) (and (consp x)
                                                (string= (car x) name)
                                                (string= (cdr x) value)))
                               (post-parameters*))
                      (and current-value (string= current-value value)))))
    (with-html-output (*standard-output*)
      (:input :type type
              :id id
              :value value
              :name name
              :class inputclass
              :checked (when checked "checked")
              :disabled disabled)
      (when label
        (htm (:label :for id :class labelclass (str label)))))))

(defun radio-choice (label name value &key disabled current-value
                           labelclass inputclass)
  (checkbox-choice label name value
                   :disabled disabled
                   :current-value current-value
                   :labelclass labelclass
                   :inputclass inputclass :type "radio"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATE CLASS.
;;; This is used to encapsulate an universal-time with a time-zone,
;;; also defining some utility functions to convert the date to
;;; different representations.

(defclass date ()
  ((universal-time :initarg :universal-time :reader date-universal-time)
   (time-zone      :initarg :time-zone      :reader date-time-zone)))

(defmethod print-object ((date date) stream)
  (print-unreadable-object (date stream :identity t :type t)
    (format stream "~a (TZ:~a)" (format-date date) (date-time-zone date))))

(defgeneric make-date (time &optional time-zone)
  (:documentation
"Creates a DATE object. The time parameter must be either an universal-time
or another DATE object. If the time-zone is not supplied a default value of
6 is used. If time is a DATE object and no time-zone is supplied, the
default value is taken from the DATE object itself."))

(defmethod make-date ((universal-time number) &optional (time-zone 6))
  (make-instance 'date
                 :universal-time universal-time
                 :time-zone time-zone))

(defmethod make-date ((date date) &optional time-zone)
  (make-instance 'date
                 :universal-time (date-universal-time date)
                 :time-zone (or time-zone (date-time-zone date))))

(defgeneric date- (date seconds)
  (:documentation
"Creates a new DATE object with its date set to that of the supplied date
in the first parameter, minus the number of seconds supplied in the second
parameter."))

(defmethod date- ((date date) (seconds number))
  (make-instance 'date
                 :universal-time (- (date-universal-time date) seconds)
                 :time-zone (date-time-zone date)))

(defgeneric date+ (date seconds)
  (:documentation
"Creates a new DATE object with its date set to that of the supplied date
in the first parameter, plus the number of seconds supplied in the second
parameter."))

(defmethod date+ ((date date) (seconds number))
  (make-instance 'date
                 :universal-time (+ (date-universal-time date) seconds)
                 :time-zone (date-time-zone date)))


(define-constant +day-names+
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(define-constant +month-names+
  #("January" "February" "March" "April" "May" "June" "July"
    "August" "September" "October" "November" "December"))

(defun format-date (date &key (longform nil))
    "Returns a string with the date formatted as \"YYYY-MM-DD\" or as
\"DayOfWeek Month DD, YYYY\" (eg. Tuesday May 4, 2010) if key :longform is t."
  (multiple-value-bind (s m h date month year day)
      (decode-universal-time (date-universal-time date) (date-time-zone date))
    (declare (ignore s m h))
    (if longform
      ;; Thursday May 6, 2010
      (format nil "~a ~a ~d, ~d"
              (svref +day-names+ day)
              (svref +month-names+ (1- month))
              date year)
      ;; 2010-05-06
      (format nil "~d-~2,'0d-~2,'0d" year month date))))

(defun parse-date (date time-zone)
  "Parse a date from a string like \"2010-06-25\" and returns a new DATE
object representing that date and the supplied time-zone."
  (when (and (stringp date) (= (length date) 10))
    (let ((year (parse-int-force-pos-or-zero (subseq date 0 4)))
          (month (parse-int-force-pos-or-zero (subseq date 5 7)))
          (day (parse-int-force-pos-or-zero (subseq date 8 10))))
      (when (and (plusp year) (plusp month) (plusp day))
        (make-date
          (encode-universal-time 0 0 12 day month year time-zone)
          time-zone)))))

(defun format-iso8601-date (date)
  "Returns a string with the date formatted to a single restricted format
from the set of formats defined by ISO 8601. Specifically, the format
YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30-05:00 for EST time zone)
See: http://www.w3.org/TR/NOTE-datetime
and  http://en.wikipedia.org/wiki/ISO_8601"
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (date-universal-time date) (date-time-zone date))
    (declare (ignore day daylight-p))
    (format nil
            "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~a~2,'0d:00"
            year month date hour minute second
            (if (plusp zone) "-" "+") (abs zone))))

(defun parse-iso8601-date (date)
  "Parse a date from a string in the following ISO 8601 format
YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30-05:00) into a DATE object."
  (flet ((extract-parts (date)
           (multiple-value-bind (lowbound upperbound vector1 vector2)
               ;; 2010-06-10T19:20:30-06:00
               ;; TODO: Is it a good idea to use a regexp? Why not simply
               ;;       use subseqs of the string?
               (#~m/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})([-|+])(\d{2}):\d{2}$/
                date)
             (declare (ignorable lowbound upperbound))
             (when (and vector1 vector2)
               ;; vector1 contains the starting positions for each matched
               ;; group while vector2 contains the corresponding ending
               ;; positions.
               ;;
               ;; We extract the pairs of positions with the `loop` form
               ;; and pass them to the lambda to extract the strings and
               ;; convert them to fixnums, except for the -/+ sign of the
               ;; zone.
               (mapcar
                 #'(lambda (pos)
                     (let ((str (subseq date (car pos) (cdr pos))))
                       (cond ((string= str "-") +1) ; CL time zones have
                             ((string= str "+") -1) ; their signs inverted.
                             (t (parse-int-force-pos-or-zero str)))))
                 (loop for start across vector1 and end across vector2
                       collect (cons start end)))))))

    ;; Now use the extract-parts return value to reconstruct an
    ;; universal-time.
    (destructuring-bind (year month day hour minute second zone-+ zone)
        (extract-parts date)
      (make-date
        (encode-universal-time
          second minute hour day month year (* zone zone-+))
        (* zone zone-+)))))

;(format-iso8601-date
;  (parse-iso8601-date "2010-06-10T19:20:30-06:00"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc utilities

(defvar %secs-in-one-day (* 60 60 24))

(defun trim-or-nil (string)
  "Returns a string with all spaces, tabs and newlines removed from both
ends. It will return NIL if the resulting string is of length 0, or if
the input string is NIL as well."
  (when string
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) string)))
      (when (> (length trimmed) 0)
        trimmed))))

(defun parse-int-or-force-value (str default &key (start 0) (end nil)
                                                  (radix 10))
  "Parses an integer from the given string. The string could be NIL or
contain garbage, in which case the function simply returns the default value
given as the second parameter. The function accepts the same keyword arguments
as `parse-integer`, except for :junk-allowed which is always T. There is no
`pos` return value like in `parse-integer`."
  (or (and str (parse-integer str
                              :start start
                              :end end
                              :radix radix
                              :junk-allowed t))
      default))

(defun parse-int-force-pos-or-zero (string &key (start 0) (end nil) (radix 10))
  "Parses an integer from the given string. The string could be NIL or contain
garbage, in which case the function will simply return the number zero (0).
Otherwise, it will parse the string and return the ABSOLUTE VALUE of the
number. The function accepts the same arguments as `parse-integer`, except for
:junk-allowed which is always T. There is no `pos` return value like in
`parse-integer`."
  (abs (parse-int-or-force-value string 0 :start start :end end :radix radix)))

;(mapcar #'parse-int-force-pos-or-zero
;        (list "" nil "haosd" "1" "0" "23" "-12" "-1" "-0"))

;;; From Paul Graham's On Lisp, pg. 47.
(defun filter (fn lst)
  "You give FILTER a function and a list, and get back a list of whatever
non-nil values are returned by the function as it is applied to the
elements of the list."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

;;; I think I saw this function in a Naggum's news post.
(defun repeatedly (&rest x) (nconc x x))
