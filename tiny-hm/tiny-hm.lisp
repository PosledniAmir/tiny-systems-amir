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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fill-to-underscore (result lst)
    (cond
      ((null lst) (list result))
      ((eq (car lst) '_) (cons result (cdr lst)))
      (t (cons (car lst)
               (fill-to-underscore result (cdr lst)))))))
			 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun thread-list (result lst)
    (cond
      ((member '_ lst) (fill-to-underscore result lst))
      (t `(funcall ,lst ,result)))))

(defmacro -> (val &rest rest)
  (reduce (lambda (result fn)
            (cond
              ((listp fn) `,(thread-list result fn))
              (t `(funcall ,fn ,result))))
          rest
          :initial-value val))

(defunclass variable-type ((name nil))
  (:documentation "Variable."))

(defunclass bool-type ()
  (:documentation "Bool."))

(defunclass number-type ()
  (:documentation "Number."))

(defunclass list-type ((type nil))
  (:documentation "List."))

(defunclass function-type ((input nil) (output nil))
  (:documentation "Function type with input and output."))

(defgeneric occurs-check (name number)
  (:documentation "Checks whether variable 'name' is in the 'number'."))

(defmethod occurs-check (name (type variable-type))
  (string= name (get-name type)))

(defmethod occurs-check (name (type bool-type))
  nil)

(defmethod occurs-check (name (type number-type))
  nil)

(defmethod occurs-check (name (type list-type))
  (occurs-check name (get-type type)))

(defmethod occurs-check (name (type function-type))
  (let ((input (get-input type))
        (output (get-output type)))
    (or (occurs-check name input)
        (occurs-check name output))))

(defgeneric substitute-name (name what number)
  (:documentation "Substitutes variable 'name' by 'what' in 'number'."))

(defmethod substitute-name (name what (type variable-type))
  (cond
    ((string= name (get-name type)) what)
    (t type)))

(defmethod substitute-name (name what (type bool-type))
  type)

(defmethod substitute-name (name what (type number-type))
  type)

(defmethod substitute-name (name what (type list-type))
  (make-list-type (substitute-name name what (get-type type))))

(defmethod substitute-name (name what (type function-type))
  (make-function-type (substitute-name name what (get-input type))
                      (substitute-name name what (get-output type))))

(defun substitute-list (name what list)
  (mapcar (lambda (item) (substitute-name name what item)) list))

(defun substitute-names (pairs n)
  (cond
    ((null pairs) n)
    (t (let ((a (first pairs))
             (b (second pairs))
             (r (rest (rest pairs))))
         (substitute-names r (substitute-name a b n))))))

(defgeneric solve-aux (a b constraints)
  (:documentation "Solve helper function, takes two items from constraints and the rest."))

(defmethod solve-aux ((a number-type) (b number-type) lst)
  (solve lst))

(defmethod solve-aux ((a bool-type) (b bool-type) lst)
  (solve lst))

(defmethod solve-aux ((a list-type) (b list-type) lst)
  (let ((a-type (get-type a))
        (b-type (get-type b)))
    (solve-aux a-type b-type lst)))

(defmethod solve-aux ((a list-type) (b number-type) lst)
  (error "Cannot solve List and Number."))

(defmethod solve-aux ((a number-type) (b list-type) lst)
  (error "Cannot solve Number and List."))

(defmethod solve-aux ((a list-type) (b bool-type) lst)
  (error "Cannot solve List and Bool."))

(defmethod solve-aux ((a bool-type) (b list-type) lst)
  (error "Cannot solve Bool and List."))

(defmethod solve-aux ((a bool-type) (b number-type) lst)
  (error "Cannot solve Bool and Number."))

(defmethod solve-aux ((a number-type) (b bool-type) lst)
  (error "Cannot solve Number and Bool."))

(defmethod solve-aux (a (b variable-type) lst)
  (solve-aux b a lst))

(defmethod solve-aux ((a variable-type) b lst)
  (let ((name (get-name a)))
    (cond
      ((occurs-check name b) (error "Cannot solve occurs-check."))
      (t (let* ((c (substitute-list name b lst))
                (s (solve c))
                (n (substitute-names s b)))
           (cons name (cons n s)))))))

(defmethod solve-aux (a (b variable-type) lst)
  (solve-aux b a lst))

(defmethod solve-aux ((a function-type) (b function-type) lst)
  (let ((a-in (get-input a))
        (b-in (get-input b))
        (a-out (get-output a))
        (b-out (get-output b)))
    (solve (cons a-in
                 (cons b-in
                       (cons a-out
                             (cons b-out lst)))))))

(defun solve (constraints)
  (cond
    ((null constraints) nil)
    (t (let ((a (first constraints))
             (b (second constraints))
             (r (rest (rest constraints))))
         (solve-aux a b r)))))

(defun demo00 ()
  (solve (list
          (make-list-type (make-variable-type "a"))
          (make-list-type (make-number-type))
          (make-variable-type "b")
          (make-list-type (make-variable-type "a")))))

(defun demo01 ()
  (solve (list
          (make-list-type (make-variable-type "a"))
          (make-variable-type "b")
          (make-variable-type "b")
          (make-bool-type))))

(defun demo02 ()
  (solve (list
          (make-list-type (make-variable-type "a"))
          (make-variable-type "b")
          (make-variable-type "b")
          (make-list-type (make-number-type)))))

(defun demo03 ()
  (solve (list
          (make-list-type (make-variable-type "a"))
          (make-variable-type "a"))))

(defunclass constant-expression ((value 0))
  (:documentation "Constant expression with value."))

(defunclass binary-expression ((name nil) (first nil) (second nil))
  (:documentation "Binary expression with name, first, second."))

(defunclass if-expression ((condition nil) (first nil) (second nil))
  (:documentation "If expression with condition, first, second."))

(defunclass variable-expression ((name nil))
  (:documentation "Variable expression with name."))

(defunclass application-expression ((first nil) (second nil))
  (:documentation "Application expression with first and second."))

(defunclass lambda-expression ((name nil) (first nil))
  (:documentation "Lambda expression with name and first."))

(defunclass let-expression ((name nil) (first nil) (second nil))
  (:documentation "Let expression with name first and second."))

(defunclass typing-context ((context (make-hash-table)))
  (:documentation "Context with context."))

(defun empty-context ()
  (make-instance 'typing-context))

(defun typing-context-from-list (&rest pairs)
  (reduce (lambda (collection pair)
            (put! (first pair) (second pair) collection))
          pairs
          :initial-value (empty-context)))

(defgeneric put! (key value collection)
  (:documentation "Puts value under key in collection."))

(defmethod put! (key value (collection typing-context))
  (setf (gethash key (get-context collection)) value)
  collection)

(defgeneric put (key value collection)
  (:documentation "Puts value under key in collection."))

(defun breaker (item)
  (slynk::simple-break)
  item)

(defmethod put (key value (collection typing-context))
  (let ((item (put! key value (copy collection))))
    item))

(defgeneric contains? (key collection)
  (:documentation "Checks whether key is in collection."))

(defmethod contains? (key (collection typing-context))
  (multiple-value-bind (value found?) (gethash key (get-context collection))
    (declare (ignore value))
    (cond
      (found? t)
      (t nil))))

(defgeneric obtain (key collection)
  (:documentation "Obtains value according to key from collection."))

(defmethod obtain (key (collection typing-context))
  (gethash key (get-context collection)))

(defgeneric to-list (collection)
  (:documentation "Converts collection to list."))

(defmethod to-list ((collection typing-context))
  (loop for key being the hash-keys of (get-context collection) using (hash-value value)
        collect (list key value)))

(defgeneric copy (collection)
  (:documentation "Copies the collection"))

(defmethod copy ((collection typing-context))
  (apply #'typing-context-from-list (to-list collection)))

(defgeneric generate (context  expression)
  (:documentation "Generates constraints."))

(defmethod generate (context (expression constant-expression))
  (values (make-number-type) nil))

(defun generate-plus (context first second)
  (let ((tc1 (multiple-value-list (generate context first)))
        (tc2 (multiple-value-list (generate context second))))
    (values (make-number-type)
            (concatenate 'list
                         (second tc1)
                         (second tc2)
                         (list (first tc1)
                               (make-number-type)
                               (first tc2)
                               (make-number-type))))))

(defun generate-equals (context first second)
  (let ((tc1 (multiple-value-list (generate context first)))
        (tc2 (multiple-value-list (generate context second))))
    (values (make-bool-type)
            (concatenate 'list
                         (second tc1)
                         (second tc2)
                         (list (first tc1)
                               (make-number-type)
                               (first tc1)
                               (make-number-type))))))

(defmethod generate (context (expression binary-expression))
  (let* ((name (get-name expression))
         (first (get-first expression))
         (second (get-second expression)))
    (cond
      ((string= "+" name) (generate-plus context first second))
      ((string= "=" name) (generate-equals context first second))
      ((string= "*" name) (generate-plus context first second))
      (t (error (format nil "Binary operator ~a not supported." name))))))

(defmethod generate (context (expression variable-expression))
  (let ((name (get-name expression)))
    (values (obtain name context) nil)))

(defmethod generate (context (expression if-expression))
  (let ((condition (multiple-value-list (generate context (get-condition expression))))
        (first (multiple-value-list (generate context (get-first expression))))
        (second (multiple-value-list (generate context (get-second expression)))))
    (values (first first)
            (concatenate 'list
                         (second first)
                         (second second)
                         (list
                          (first condition)
                          (make-bool-type)
                          (first second)
                          (first first))))))

(defvar *type-variable-counter* 0)
(defun new-variable-type ()
  (incf *type-variable-counter*)
  (make-variable-type (format nil "_a~D" *type-variable-counter*)))

(defmethod generate (context (expression let-expression))
  (let* ((first (multiple-value-list (generate context (get-first expression))))
         (name (get-name expression))
         (ctx (put name (first first) context))
         (second (multiple-value-list (generate ctx (get-second expression)))))
    (values (first second)
            (concatenate 'list
                         (second first)
                         (second second)))))

(defmethod generate (context (expression lambda-expression))
  (let* ((name (get-name expression))
         (var (new-variable-type))
         (ctx  (put name var context))
         (tc (multiple-value-list (generate ctx (get-first expression)))))
    (values (make-function-type var (first tc))
            (second tc))))

(defmethod generate (context (expression application-expression))
  (let ((var (new-variable-type))
        (first (multiple-value-list (generate context (get-first expression))))
        (second (multiple-value-list (generate context (get-second expression)))))
    (values var
            (concatenate 'list
                         (second first)
                         (second second)
                         (list (make-function-type (first second) var)
                               (first first))))))

(defun infer (expression)
  (let* ((tc (multiple-value-list (generate (empty-context) expression)))
         (sub (solve (second tc)))
         (type (substitute-names sub (first tc))))
    type))

(defun demo04 ()
  (let* ((e1 (make-binary-expression "="
                                     (make-variable-expression "x")
                                     (make-binary-expression "+"
                                                             (make-constant-expression 10)
                                                             (make-variable-expression "x"))))
         (tc (multiple-value-list (generate
                                   (typing-context-from-list (list "x" (make-variable-type "a")))
                                   e1))))
    (solve (second tc))))

(defun demo05 ()
  (let* ((e2 (make-if-expression (make-variable-expression "x")
                                 (make-binary-expression "+"
                                                         (make-constant-expression 2)
                                                         (make-constant-expression 1))
                                 (make-variable-expression "y")))
         (tc (multiple-value-list
              (generate
               (typing-context-from-list (list "x" (make-variable-type "a"))
                                         (list "y" (make-variable-type "b")))
               e2))))
    (solve (second tc))))

(defun demo06 ()
  (let* ((e3 (make-if-expression (make-variable-expression "x")
                                 (make-binary-expression "+"
                                                         (make-constant-expression 2)
                                                         (make-constant-expression 1))
                                 (make-variable-expression "x")))
         (tc (multiple-value-list
              (generate
               (typing-context-from-list (list "x" (make-variable-type "a"))
                                         (list "y" (make-variable-type "b")))
               e3))))
    (solve (second tc))))

(defun demo07 ()
  (infer (make-let-expression "x"
                              (make-constant-expression 10)
                              (make-binary-expression "="
                                                      (make-variable-expression "x")
                                                      (make-constant-expression 10)))))

(defun demo08 ()
  (infer (make-let-expression "f"
                              (make-lambda-expression "x"
                                                      (make-binary-expression "*"
                                                                              (make-variable-expression "x")
                                                                              (make-constant-expression 2)))
                              (make-binary-expression "+"
                                                      (make-application-expression
                                                       (make-variable-expression "f")
                                                       (make-constant-expression 20))
                                                      (make-application-expression
                                                       (make-variable-expression "f")
                                                       (make-constant-expression 1))))))

(defun demo09 ()
  (infer (make-lambda-expression "x"
                                 (make-lambda-expression "f"
                                                         (make-application-expression
                                                          (make-variable-expression "f")
                                                          (make-application-expression
                                                           (make-variable-expression "f")
                                                           (make-variable-expression "x")))))))

(defun demo10 ()
  (infer (make-lambda-expression "f"
                                 (make-application-expression
                                  (make-variable-expression "f")
                                  (make-variable-expression "f")))))

(defun demo11 ()
  (infer (make-lambda-expression "f"
                                 (make-binary-expression
                                  "+"
                                  (make-application-expression
                                   (make-variable-expression "f")
                                   (make-constant-expression 1))
                                  (make-application-expression
                                   (make-variable-expression "f")
                                   (make-binary-expression
                                    "="
                                    (make-constant-expression 2)
                                    (make-constant-expression 3)))))))

