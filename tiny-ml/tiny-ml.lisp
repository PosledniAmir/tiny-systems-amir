(defclass integer-value ()
  ((value
    :reader get-value
    :initform 0
    :initarg :value
    :type integer
    :documentation "Integer value")))

(defun make-integer-value (value)
  (make-instance 'integer-value :value value))

(defclass constant-expression ()
  ((value
    :reader get-value
    :initform 0
    :initarg :value
    :type integer
    :documentation "Integer value")))

(defun make-constant-expression (value)
  (make-instance 'constant-expression :value value))

(defclass binary-expression ()
  ((function
    :reader get-function
    :initform ""
    :initarg :function
    :type string
    :documentation "Function name")
   (left
    :reader get-left
    :initform nil
    :initarg :left
    :documentation "Left expression")
   (right
    :reader get-right
    :initform nil
    :initarg :right
    :documentation "Right expression")))

(defun make-binary-expression (function left right)
  (make-instance 'binary-expression :function function
                                    :left left
                                    :right right))

(defclass variable-expression ()
  ((name
    :reader get-name
    :initform ""
    :initarg :name
    :documentation "Variable name")))

(defun make-variable-expression (name)
  (make-instance 'variable-expression :name name))

(defclass unary-expression ()
  ((function
    :reader get-function
    :initform ""
    :initarg :function
    :type string
    :documentation "Function name")
   (right
    :reader get-right
    :initform nil
    :initarg :right
    :documentation "Right expression")))

(defun make-unary-expression (function right)
  (make-instance 'unary-expression :function function
                 :right right))

(defclass if-expression ()
  ((condition
    :reader get-condition
    :initform nil
    :initarg :condition
    :documentation "Condition expression")
   (then
    :reader get-then
    :initform nil
    :initarg :then
    :documentation "Then expression")
   (else
    :reader get-else
    :initform nil
    :initarg :else
    :documentation "Else expression")))

(defclass variable-context ()
  ((context
    :reader get-context
    :initform (make-hash-table)
    :initarg :context
    :documentation "Context hash table")))

(defun empty-context ()
  (make-instance 'variable-context))

(defun make-variable-context (&rest pairs)
  (reduce (lambda (collection pair)
            (put (first pair) (second pair) collection))
          pairs
          :initial-value (empty-context)))

(defgeneric put (key value collection)
  (:documentation "Puts value under key in collection."))

(defmethod put (key value (collection variable-context))
  (setf (gethash key (get-context collection)) value)
  collection)

(defgeneric contains? (key collection)
  (:documentation "Checks whether key is in collection."))

(defmethod contains? (key (collection variable-context))
  (multiple-value-bind (value found?) (gethash key (get-context collection))
    (declare (ignore value))
    (cond
      (found? t)
      (t nil))))

(defgeneric obtain (key collection)
  (:documentation "Obtains value according to key from collection."))

(defmethod obtain (key (collection variable-context))
  (gethash key (get-context collection)))

(defgeneric plus (left right)
  (:documentation "Plus function."))

(defmethod plus ((left integer-value) (right integer-value))
  (make-integer-value (+ (get-value left) (get-value right))))

(defgeneric minus (left right)
  (:documentation "Minus function."))

(defmethod minus ((left integer-value) (right integer-value))
  (make-integer-value (- (get-value left) (get-value right))))

(defgeneric times (left right)
  (:documentation "Times function."))

(defmethod times ((left integer-value) (right integer-value))
  (make-integer-value (* (get-value left) (get-value right))))

(defgeneric true? (value)
  (:documentation "Checks whether the value is truthful."))

(defmethod true? ((value integer-value))
  (cond
    ((/= 0 (get-value value)) t)
    (t nil)))

(defgeneric evaluate (context expression)
  (:documentation "Evaluates expression with given context."))

(defmethod evaluate (context (exp constant-expression))
  (make-integer-value (get-value exp)))

(defmethod evaluate (context (exp binary-expression))
  (let ((fn (get-function exp))
        (left  (evaluate context (get-left exp)))
        (right (evaluate context (get-right exp))))
    (cond
      ((string= "+" fn) (plus left right))
      ((string= "*" fn) (times left right))
      (t (error "Unsupported binary operator.")))))

(defmethod evaluate (context (exp variable-expression))
  (let ((name (get-name exp)))
    (cond
      ((contains? name context) (obtain name context))
      (t (error (format nil "Unbound variable: ~a." name))))))

(defmethod evaluate (context (exp unary-expression))
  (let ((fn (get-function exp))
        (right (evaluate context (get-right exp))))
    (cond
      ((string= "-" fn) (minus (make-integer-value 0) right))
      (t (error "Unsupported unary operator.")))))

(defmethod evaluate (context (exp if-expression))
  (let ((condition (get-condition exp)) (then (get-then exp))
      (else (get-else exp)))
  (cond
    ((true? (evaluate context  condition)) (evaluate context then))
    (t (evaluate context else)))))

(format t "test00: ~a~%"
        (get-value
         (evaluate (empty-context)
                   (make-binary-expression
                    "+"
                    (make-binary-expression "*"
                                            (make-constant-expression 1)
                                            (make-constant-expression 2))
                    (make-binary-expression "*"
                                            (make-constant-expression 20)
                                            (make-constant-expression 2))))))

(defun test01 (context)
  (format t "test01: ~a~%"
          (get-value
           (evaluate context
                     (make-binary-expression
                      "+"
                      (make-variable-expression "x")
                      (make-binary-expression "*"
                                              (make-variable-expression "x")
                                              (make-constant-expression 20)))))))

(test01 (make-variable-context (list "x" (make-integer-value 4))))
(test01 (make-variable-context (list "x" (make-integer-value 2))))

(format t "test02: ~a~%"
        (get-value
         (evaluate (empty-context)
                   (make-binary-expression
                    "+"
                    (make-binary-expression "*"
                                            (make-constant-expression 1)
                                            (make-constant-expression 2))
                    (make-unary-expression
                     "-"
                     (make-binary-expression "*"
                                             (make-constant-expression -20)
                                             (make-constant-expression 2)))))))
