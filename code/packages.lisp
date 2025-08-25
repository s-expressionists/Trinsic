(cl:defpackage #:trinsic
  (:use #:cl)
  (:export #:*standard-readtable*
           #:call-with-state-value
           #:client-form
           #:features-list
           #:intrinsicp
           #:make-define-interface
           #:state-value
           #:valid-state-value-p))
