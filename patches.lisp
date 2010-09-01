;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOUCHDB (as of June 2010) wouldn't let me json-encode an empty object
;;; to get "{}", which is needed as part of the argument to :end-key in
;;; views with multiple keys. Therefore I'm overriding the function
;;; `CLOUCHDB::ENCODE` to simply encode an empty hash table as an empty
;;; object "{}".
;;;
;;; This way we can query a CouchDB view with params like these:
;;; ?startkey=["foo"]&endkey=["bar",{}]&group=true
;;;
;;; Using a form like this:
;;; (clouchdb:invoke-view "design-doc" "view" :group t
;;;                       :start-key (list "foo")
;;;                       :end-key (list "bar" (make-hash-table))
;;;
;;; This code is taken directly from CLOUCHDB (file 'encoder.lisp'). I only
;;; added the lines:
;;; ((hash-table-p d)
;;;  (write-string "{}" stream))
;;;
;;; Peter Eddy is the author of CLOUCHDB.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (c) 2007 Peter Eddy. All rights reserved.

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
(in-package #:cl-user)

(setf (symbol-function (find-symbol "ENCODE" (find-package "CLOUCHDB")))
      (compile nil
               (lambda (d stream)
                 (cond ((null d)
                        (write-string "null" stream))
                       ((numberp d)
                        (clouchdb::write-json-number d stream))
                       ((symbolp d)
                        (clouchdb::write-json-symbol d stream))
                       ((stringp d)
                        (clouchdb::write-json-string d stream))
                       ;; TODO: Obviously this is just an ugly patch.
                       ;; It would be good to actually implement encoding
                       ;; a hash-table.
                       ((hash-table-p d)
                        (write-string "{}" stream))
                       ((clouchdb::assoclp d)
                        (clouchdb::write-alist d stream))
                       ((listp d)
                        (clouchdb::write-list d stream))))))

