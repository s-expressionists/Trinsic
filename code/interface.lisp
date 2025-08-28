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

(defgeneric valid-cell-value-p (client name type value))

(defgeneric initial-cell-value (client name type))

(defgeneric cell-value (client name type))

(defgeneric (setf cell-value) (new-value client name type))

(defgeneric call-with-cell-value (client name type value thunk))

(defmacro make-define-interface
    ((&key (client-form nil client-form-p) (client-class nil client-class-p)
           ((:intrinsic intrinsicp) nil intrinsicp-p))
     declarations
     &body body)
  `(defmacro ,(intern (symbol-name '#:define-interface))
       (&key client-form client-class ((:intrinsic intrinsicp) nil))
     (let ((body-forms (let (,@(when client-form-p
                                 `((,client-form client-form)))
                             ,@(when client-class-p
                                 `((,client-class client-class)))
                             ,@(when intrinsicp-p
                                 `((,intrinsicp intrinsicp)))
                             ,@(mapcar (lambda (decl)
                                         (destructuring-bind (var sym &key variable)
                                             decl
                                           (declare (ignore variable))
                                           (if (symbol-package sym)
                                               `(,var (if intrinsicp
                                                          ',sym
                                                          (intern ,(string sym))))
                                               `(,var (intern ,(string sym))))))
                                       declarations))
                         (nconc (locally ,@body)
                                ,@(mapcar (lambda (decl)
                                            (destructuring-bind (var sym &key variable)
                                                decl
                                              (when variable
                                                ``((defmethod cell-value
                                                       ((client ,client-class)
                                                        (name (eql ',',sym))
                                                        (type (eql 'cl:variable)))
                                                     ,,var)

                                                   (defmethod (setf cell-value)
                                                       (new-value (client ,client-class)
                                                        (name (eql ',',sym))
                                                        (type (eql 'cl:variable)))
                                                     (setf ,,var new-value))

                                                   (defmethod call-with-cell-value
                                                       ((client ,client-class)
                                                        (name (eql ',',sym))
                                                        (type (eql 'cl:variable)) thunk value)
                                                     (let ((,,var value))
                                                       (funcall thunk)))))))
                                          declarations))))
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

(defmacro make-define-interface2
    ((&key (client-form nil client-form-p) (client-class nil client-class-p)
           ((:intrinsic intrinsicp) nil intrinsicp-p))
     &body body)
  `(defmacro ,(intern (symbol-name '#:define-interface))
       (&key client-form client-class ((:intrinsic intrinsicp) nil))
     (let ((body-forms (macrolet ((defun* (name &rest rest)
                                    (let ((actual-name ,(if intrinsicp
                                                            '(if (symbol-package name)
                                                              name
                                                              (intern (string name)))
                                                            '(intern (string name)))))
                                      `(defun ,actual-name ,@rest)))
                                  (defmacro* (name &body rest)
                                    (let ((actual-name ,(if intrinsicp
                                                            '(if (symbol-package name)
                                                              name
                                                              (intern (string name)))
                                                            '(intern (string name)))))
                                      `(defmacro ,actual-name ,@rest))))
                         (let (,@(when client-form-p
                                   `((,client-form client-form)))
                               ,@(when client-class-p
                                   `((,client-class client-class)))
                               ,@(when intrinsicp-p
                                   `((,intrinsicp intrinsicp))))
                           ,@body))))
       `(progn
          (defmethod client-form ((client ,client-class))
            ,client-form)
          (defmethod intrinsicp ((client ,client-class))
            ,intrinsicp)
          ,.body-forms
          ,.(when intrinsicp
              `((setf *features* (nunion (features-list ,client-form)
                                         *features*))))))))
