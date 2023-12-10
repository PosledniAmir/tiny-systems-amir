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

(defmethod put (key value (collection typing-context))
  (put! key value (copy collection)))

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

(defgeneric copy (collection)
  (:documentation "Copies the collection"))

(defmethod copy ((collection typing-context))
  (apply #'typing-context-from-list
         (maphash (lambda (key val) (list key val))
                  (get-context collection))))

(defgeneric generate (context  expression)
  (:documentation "Generates constraints."))

(defmethod generate (context (expression constant-expression))
  (let ((val (get-value expression)))
    (values (make-number-type) nil)))

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
      ((string= "=" name) (generate-equals contex first second))
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
