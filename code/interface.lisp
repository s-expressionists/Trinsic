(cl:in-package #:trinsic)

(defgeneric intrinsicp (client)
  (:method (client)
    (declare (ignore client))
    nil))

(defgeneric client-form (client)
  (:documentation "Return the client form for future reference. Used primarily in macro
expansion"))

(defgeneric features-list (client)
  (:documentation "Return a list of feature keywords.")
  (:method-combination nconc)
  (:method nconc (client)
    (declare (ignore client))
    nil))

(defun define-interface-forms (client-form)
  `((eval-when (:compile-toplevel :load-toplevel :execute)
      (when (intrinsicp ,client-form)
        (setf *features* (nunion (features-list ,client-form) *features*))))))
