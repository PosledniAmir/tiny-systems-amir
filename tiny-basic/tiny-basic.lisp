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

(defunclass string-value ((value ""))
  (:documentation "Represents string."))

(defunclass const-expression ((value nil))
  (:documentation "Represents constant expression."))

(defunclass print-command ((expression nil))
  (:documentation "Represents print command."))

(defunclass run-command ()
  (:documentation "Represents run command."))

(defunclass goto-command ((value 0))
  (:documentation "Represents goto command."))

(defunclass state ((program nil))
  (:documentation "Represents state of the program."))

(defgeneric print-value (value)
  (:documentation "Pretty prints value."))

(defmethod print-value ((val string-value))
  (format t "~a" (get-value val)))

(defun get-program-line (state line)
  (cond
    ((null state) nil)
    (t (let ((fst (first state))
             (rst (rest state)))
         (cond
           ((= (car fst) line) (second fst))
           (t (get-program-line rst line)))))))

(defgeneric eval-expression (expression)
  (:documentation "Evaluates expression."))

(defmethod eval-expression ((expr const-expression))
  (get-value expr))

(defgeneric run-command (state line command)
  (:documentation "Runs command on line with state."))

(defmethod run-command (state line (command print-command))
  (-> command #'get-expression #'eval-expression #'print-value)
  (run-next-line state line))

(defmethod run-command (state line (command run-command))
  (let ((fst-line (first (get-program state))))
    (run-command state (first fst-line) (second fst-line))))

(defmethod run-command (state line (command goto-command))
  (let ((target (get-program-line (get-program state) (get-value command))))
    (run-command state (get-value command) target)))

(defun run-next-line (state line)
  (let ((result (find-if (lambda (item) (> (first item) line))
                         (get-program state))))
    (cond
      ((null result) state)
      (t (run-command state (first result) (second result))))))

(run-command
 (make-state (list
              (list 10 (make-print-command
                        (make-const-expression
                         (make-string-value (format nil "HELLO WORLD~%")))))))
 -1
 (make-run-command))

;; (run-command
;;  (make-state (list
;;               (list 10 (make-print-command
;;                         (make-const-expression
;;                          (make-string-value (format nil "HELLO WORLD~%")))))
;;               (list 20 (make-goto-command 10))))
;;  -1
;;  (make-run-command))
