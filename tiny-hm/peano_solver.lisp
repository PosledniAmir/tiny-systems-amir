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

(defunclass zero-number () (:documentation "Zero."))

(defunclass succ-number ((number nil)) (:documentation "Succ."))

(defunclass variable-number ((name nil)) (:documentation "Variable"))

(defgeneric occurs-check (name number)
  (:documentation "Checks whether variable 'name' is in the 'number'."))

(defmethod occurs-check (name (n zero-number))
  nil)

(defmethod occurs-check (name (n succ-number))
  (let  ((next (get-number n)))
    (occurs-check name next)))

(defmethod occurs-check (name (n variable-number))
  (let ((var (get-name n)))
    (string= name var)))

(defgeneric substitute-name (name what number)
  (:documentation "Substitutes variable 'name' by 'what' in 'number'."))

(defmethod substitute-name (name what (n zero-number))
  n)

(defmethod substitute-name (name what (n succ-number))
  (let ((next (get-number n)))
    (make-succ-number (substitute-name name what next))))

(defmethod substitute-name (name what (n variable-number))
  (let ((var (get-name n)))
    (cond
      ((string= name var) what)
      (t n))))

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

(defmethod solve-aux ((a zero-number) (b zero-number) lst)
  (solve lst))

(defmethod solve-aux ((a zero-number) (b succ-number) lst)
  (error "Cannot solve 'Zero' and 'Succ'."))

(defmethod solve-aux ((a succ-number) (b zero-number) lst)
  (error "Cannot solve 'Succ' and 'Zero'."))

(defmethod solve-aux ((a succ-number) (b succ-number) lst)
  (let ((a-in (get-number a))
        (b-in (get-number b)))
    (solve (cons a-in (cons b-in lst)))))

(defmethod solve-aux ((a variable-number) b lst)
  (let ((name (get-name a)))
    (cond
      ((occurs-check name b) (error "Cannot solve occurs-check."))
      (t (let* ((c (substitute-list name b lst))
                (s (solve c))
                (n (substitute-names s b)))
           (cons name (cons n s)))))))

(defmethod solve-aux (a (b variable-number) lst)
  (solve-aux b a lst))

(defun solve (constraints)
  (cond
    ((null constraints) nil)
    (t (let ((a (first constraints))
             (b (second constraints))
             (r (rest (rest constraints))))
         (solve-aux a b r)))))

(defun demo00 ()
  (format T
          "~A~%"
          (solve (list (make-succ-number (make-variable-number "x"))
                       (make-succ-number (make-zero-number))))))

(defun demo01 ()
  (format T
          "~A~%"
          (solve (list (make-succ-number (make-succ-number (make-zero-number)))
                       (make-succ-number (make-zero-number))))))

(defun demo02 ()
  (format T
          "~A~%"
          (solve (list (make-succ-number (make-succ-number (make-variable-number "x")))
                       (make-succ-number (make-zero-number))))))

(defun demo03 ()
  (format T
          "~A~%"
          (solve (list (make-succ-number
                        (make-variable-number "x"))
                       (make-succ-number
                        (make-zero-number))
                       (make-variable-number "y")
                       (make-succ-number
                        (make-variable-number "x"))))))

(defun demo04 ()
  (format T
          "~A~%"
          (solve (list (make-variable-number "x")
                       (make-succ-number (make-succ-number (make-variable-number "z")))
                       (make-succ-number (make-variable-number "z"))
                       (make-succ-number (make-zero-number))))))

(defun demo05 ()
  (format T
          "~A~%"
          (solve (list (make-variable-number "x")
                       (make-succ-number (make-variable-number "x"))))))
