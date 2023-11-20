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

(defun fill-to-underscore (result lst)
  (cond
    ((null lst) (list result))
    ((eq (car lst) '_) (cons result (cdr lst)))
    (t (cons (car lst)
             (fill-to-underscore result (cdr lst))))))
			 
(defun thread-list (result lst)
  (cond
    ((member '_ lst) (fill-to-underscore result lst))
    (t `(,lst ,result))))


(defmacro -> (val &rest rest)
  (reduce (lambda (result fn)
            (cond
              ((listp fn) `,(thread-list result fn))
              (t `(,fn ,result))))
          rest
          :initial-value val))
