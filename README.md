# Trinsic

Trinisic is a collection of useful functions that aid in
extrinsic/intrinsic system construction.

## client-form

```common-lisp
(defgeneric client-form (client))
```

CLIENT-FORM returns a form that will return the current client when
evaluated in the future. It is used during during macro-expansion so
that the expanded code refers to correct client at execution time.

## features-list

```common-lisp
(defgeneric features-list (client)
```

Return a list of feature keywords to be merged with *FEATURES* for
intrinsic clients. This generic function has NCONC method combination.

## cell-value

```common-lisp
(defgeneric cell-value (client name type))
```

Returns the current value of a cell in the client. For example
`(cell-value client 'cl:*standard-output* 'cl:variable)` could be used
to retrieve the value of *STANDARD-OUTPUT*.

## setf cell-value

```common-lisp
(defgeneric (setf cell-value) (new-value client name type))
```

Set the value of the cell.

## valid-cell-value-p

```common-lisp
(defgeneric valid-cell-value-p (client name type value))
```

Checks for valid values of a cell.

## initial-cell-value

```common-lisp
(defgeneric initial-cell-value (client name type))
```

Returns the proper initial value for a cell.

## call-with-cell-value

```common-lisp
(defgeneric call-with-cell-value (client name type value thunk))
```

Invokes THUNK with a cell value. For example:

```common-lisp
(defmethod call-with-cell-value
    (client (name (eql 'cl:*standard-output*)) (type (eql 'cl:variable))
     value thunk)
  (let ((cl:*standard-output* value))
    (funcall thunk)))
```

## make-define-interface

```common-lisp
(defmacro make-define-interface
    ((&key client-form client-class intrinsic)
     declarations &body body))
```

MAKE-DEFINE-INTERFACE creates a macro in the current package called
DEFINE-INTERFACE that used to create an interface in an intrinsic or
extrinsic system. The macro DEFINE-INTERFACE will define an
appropriate CLIENT-FORM method.

DECLARATIONS are a list of variable declarations each with the form
`(var symbol &key variable)`. A LET binding is established around BODY
with these declarations. The value of VAR in this binding is as
follows:

1. For intrinsic systems the initial value of VAR will be SYMBOL if
   SYMBOL is interned. If SYMBOL is uninterned the VAR will be set to
   an interned symbol in *PACKAGE* with same SYMBOL-NAME as SYMBOL.
2. For extrinsic systems the initial value of VAR will be an an
   interned symbol in *PACKAGE* with same SYMBOL-NAME as SYMBOL.
3. If the keyword argument :VARIABLE is supplied and is non-NIL then
   methods for CELL-VALUE, (SETF CELL-VALUE), and CALL-WITH-CELL-VALUE
   will be defined for a TYPE of CL:VARIABLE.

For intrinsic systems the DEFINE-INTERFACE macro created by
MAKE-DEFINE-INTERFACE will also merge the features keywords from
FEATURE-LIST into CL:*FEATURES* and disable package locks for the BODY
via TRIVIAL-PACKAGE-LOCKS:WITH-UNLOCKED-SYSTEM-PACKAGES.