(cl:defpackage #:trinsic
  (:use #:cl)
  (:export #:*standard-readtable*
           #:call-with-cell-value
           #:client-form
           #:define-cell-value
           #:defun*
           #:defmacro*
           #:features-list
           #:initial-cell-value
           #:intrinsicp
           #:make-define-interface
           #:cell-value
           #:valid-cell-value-p))
