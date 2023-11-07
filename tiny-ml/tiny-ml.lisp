(defmacro defunclass (name slots &optional docstring)
  (let* ((constructor (intern (format nil "MAKE-~A" name)))
         (getter (lambda (slot) (intern (format nil "GET-~A" (first slot)))))
         (keyword (lambda (slot) (intern (format nil "~A" (first slot)) :keyword))))
    `(progn
       (defclass ,name ()
         ,(mapcar
           (lambda (slot)
             `(,(first slot)
               :reader ,(funcall getter slot)
               :initarg ,(funcall keyword slot)
               :initform ,(second slot)))
           slots)
         ,docstring)
       (defun ,constructor ,(mapcar #'first slots)
         (make-instance ',name
                        ,@(apply #'append
                                 (mapcar (lambda (slot)
                                           (list (funcall keyword slot)
                                                 (first slot)))
                                         slots)))))))

(defunclass integer-value ((value 0))
  (:documentation "Integer value."))

(defunclass closure-value ((variable nil)
                           (expression nil)
                           (context (empty-context)))
  (:documentation "Closure value."))

(defunclass tuple-value ((left nil)
                         (right nil))
  (:documentation "Tuple value."))

(defunclass constant-expression ((value 0))
  (:documentation "Constant expression."))

(defunclass binary-expression ((function "")
                               (left nil)
                               (right nil))
  (:documentation "Binary expression."))

(defunclass variable-expression ((name ""))
  (:documentation "Variable expression."))

(defunclass unary-expression ((function "")
                              (right nil))
  (:documentation "Unary expression."))

(defunclass if-expression ((condition nil)
                           (then nil)
                           (else nil))
  (:documentation "If expression."))

(defunclass lambda-expression ((variable "")
                               (expression nil))
  (:documentation "Lambda expression."))

(defunclass application-expression ((function nil)
                                    (value nil))
  (:documentation "Application expression."))

(defunclass let-expression ((variable "")
                            (value nil)
                            (expression nil))
  (:documentation "Let expression."))

(defunclass tuple-expression ((left nil)
                              (right nil))
  (:documentation "Tuple expression."))

(defunclass get-tuple-expression ((left? t)
                                  (tuple nil))
  (:documentation "Extract from a tuple expression."))

(defunclass variable-context ((context (make-hash-table)))
  (:documentation "Context hash table."))

(defun empty-context ()
  (make-instance 'variable-context))

(defun variable-context-from-list (&rest pairs)
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

(defgeneric copy (collection)
  (:documentation "Copies the collection"))

(defmethod copy ((collection variable-context))
  (apply #'variable-context-from-list
         (maphash (lambda (key val) (list key val))
                  (get-context collection))))

(defgeneric plus (left right)
  (:documentation "Plus function."))

(defmethod plus ((left integer-value) (right integer-value))
  (make-integer-value (+ (get-value left) (get-value right))))

(defmethod plus ((left integer-value) (right closure-value))
  (error "Cannot apply + to ~a and ~a" left right))

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

(defmethod evaluate (context (exp lambda-expression))
  (make-closure-value (make-variable-expression (get-variable exp))
                      (get-expression exp)
                      context))

(defgeneric apply-input (context fn input)
  (:documentation "Applies input to fn"))

(defmethod apply-input (context (fn lambda-expression) input)
  (evaluate (put (get-variable fn)
                 input
                 (copy context))
            fn))

(defmethod apply-input (context (fn variable-expression) input)
  (let ((found (evaluate context fn)))
    (apply-input context found input)))

(defmethod apply-input (context (fn closure-value) input)
  (evaluate (put (get-name (get-variable fn))
                 input
                 (get-context fn))
            fn))

(defmethod apply-input (context fn input)
  (error (format nil "Cannot apply ~a to ~a" input fn)))

(defmethod evaluate (context (exp application-expression))
  (let ((input (evaluate context (get-value exp)))
        (function (get-function exp)))
    (evaluate
     context
     (apply-input context function input))))

(defmethod evaluate (context (exp closure-value))
  (evaluate (get-context exp)
            (get-expression exp)))

(defmethod evaluate (context (exp let-expression))
  (evaluate context
            (make-application-expression
             (make-lambda-expression (get-variable exp)
                                     (get-expression exp))
             (get-value exp))))

(defmethod evaluate (context (exp integer-value))
  exp)

(defmethod evaluate (context (exp get-tuple-expression))
  (cond
    ((get-left? exp) (evaluate context (get-left (get-tuple exp))))
    (t (evaluate context (get-right (get-tuple exp))))))

(defmethod evaluate (context (exp tuple-expression))
  (make-tuple-value (evaluate context (get-left exp))
                    (evaluate context (get-right exp))))

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

(test01 (variable-context-from-list (list "x" (make-integer-value 4))))
(test01 (variable-context-from-list (list "x" (make-integer-value 2))))

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

(format t "test03: ~a~%"
        (get-value
         (evaluate (empty-context)
                   (make-if-expression
                    (make-binary-expression "+"
                                            (make-constant-expression 5)
                                            (make-unary-expression "-"
                                             (make-constant-expression 4)))
                    (make-binary-expression "*"
                                            (make-constant-expression 21)
                                            (make-constant-expression 2))
                    (make-constant-expression 0)))))

(format t "test04: ~a~%"
        (get-value
         (evaluate (empty-context)
                   (make-if-expression
                    (make-binary-expression "+"
                                            (make-constant-expression 5)
                                            (make-constant-expression 4))
                    (make-constant-expression 0)
                    (make-binary-expression "*"
                                            (make-constant-expression 21)
                                            (make-constant-expression 2))))))

(format t "test05: ~a~%"
        (evaluate
         (empty-context)
         (make-lambda-expression
          (make-variable-expression "x")
          (make-binary-expression "*"
                                  (make-variable-expression "x")
                                  (make-constant-expression 2)))))

(format t "test06: ~a~%"
        (get-value
         (evaluate
          (empty-context)
          (make-application-expression
           (make-lambda-expression
            "x"
            (make-binary-expression "*"
                                    (make-variable-expression "x")
                                    (make-constant-expression 2)))
           (make-constant-expression 21)))))

 ;; (format t "test07: ~a~%"
 ;;         (get-value
 ;;          (evaluate
 ;;           (empty-context)
 ;;           (make-application-expression
 ;;            (make-constant-expression 21)
 ;;            (make-lambda-expression
 ;;             "x"
 ;;             (make-binary-expression
 ;;              "*"
 ;;              (make-variable-expression "x")
 ;;              (make-constant-expression 2)))))))

;; (format t "test08: ~a~%"
;;         (get-value
;;          (evaluate
;;           (empty-context)
;;           (make-binary-expression
;;            "+"
;;            (make-constant-expression 21)
;;            (make-lambda-expression
;;             "x"
;;             (make-binary-expression
;;              "*"
;;              (make-variable-expression "x")
;;              (make-constant-expression 2)))))))

(format t "test09: ~a~%"
        (get-value
         (evaluate
          (empty-context)
          (make-let-expression "x"
                               (make-constant-expression 2)
                               (make-binary-expression
                                "+"
                                (make-variable-expression "x")
                                (make-binary-expression
                                 "*"
                                 (make-variable-expression "x")
                                 (make-constant-expression 20)))))))

(format t "test10: ~a~%"
        (get-value
         (evaluate
          (empty-context)
          (make-let-expression "f"
                               (make-lambda-expression
                                "x"
                                (make-binary-expression
                                 "*"
                                 (make-variable-expression "x")
                                 (make-constant-expression 2)))
                               (make-binary-expression
                                "+"
                                (make-application-expression
                                 (make-variable-expression "f")
                                 (make-constant-expression 20))
                                (make-application-expression
                                 (make-variable-expression "f")
                                 (make-constant-expression 1)))))))

(format t "test11: ~a~%"
        (get-value
         (evaluate
          (empty-context)
          (make-get-tuple-expression
           t
           (make-tuple-expression
            (make-binary-expression
             "*"
             (make-constant-expression 2)
             (make-constant-expression 21))
            (make-constant-expression 123))))))

(format t "test12: ~a~%"
        (get-value
         (evaluate
          (empty-context)
          (make-get-tuple-expression
           nil
           (make-tuple-expression
            (make-binary-expression
             "*"
             (make-constant-expression 2)
             (make-constant-expression 21))
            (make-constant-expression 123))))))

;; (format t "test13: ~a~%"
;;         (get-value
;;          (evaluate
;;           (empty-context)
;;           (make-get-tuple-expression
;;            t
;;            (make-constant-expression 42)))))
