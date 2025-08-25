(cl:in-package #:trinsic)

(defgeneric client-form (client)
  (:documentation "Return the client form for future reference. Used primarily in macro
expansion."))

(defgeneric features-list (client)
  (:documentation "Return a list of feature keywords.")
  (:method-combination nconc)
  (:method nconc (client)
    (declare (ignore client))
    nil))

(defgeneric intrinsicp (client)
  (:documentation "Return non-NIL if the client is intrinsic.")
  (:method (client)
    (declare (ignore client))
    nil))

(defgeneric valid-state-value-p (client aspect value))

(defgeneric state-value (client aspect))

(defgeneric (setf state-value) (new-value client aspect))

(defgeneric call-with-state-value (client thunk aspect value))

(defmacro make-define-interface ((client-form client-class intrinsicp) declarations &body body)
  `(defmacro ,(intern (symbol-name '#:define-interface))
       (client-form client-class &optional intrinsicp)
     (let ((body-forms (let ((,client-form client-form)
                             (,client-class client-class)
                             (,intrinsicp intrinsicp)
                             ,@(mapcar (lambda (decl)
                                         (let ((var (first decl))
                                               (sym (second decl)))
                                           (if (symbol-package sym)
                                               `(,var (if intrinsicp
                                                          ',sym
                                                          (intern ,(string sym))))
                                               `(,var (intern ,(string sym))))))
                                       declarations))
                         ,@body))
           (feature-forms (when intrinsicp
                            `((setf *features* (nunion (features-list ,client-form)
                                                       *features*)))))
           (other-forms `((defmethod client-form ((client ,client-class))
                            ,client-form)
                          (defmethod intrinsicp ((client ,client-class))
                            ,intrinsicp))))
       `(progn
          ,.other-forms
          ,.feature-forms
          ,.body-forms))))
