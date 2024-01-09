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

(defunclass number-value ((value 0))
  (:documentation "Represents number."))

(defunclass boolean-value ((value nil))
  (:documentation "Represents boolean."))

(defunclass const-expression ((value nil))
  (:documentation "Represents constant expression."))

(defunclass function-expression ((name "")
                                 (arguments nil))
  (:documentation "Represents function expression."))

(defunclass variable-expression ((name ""))
  (:documentation "Represents variable expression."))

(defunclass print-command ((expressions nil))
  (:documentation "Represents print command."))

(defunclass run-command ()
  (:documentation "Represents run command."))

(defunclass goto-command ((value 0))
  (:documentation "Represents goto command."))

(defunclass assign-command ((name "")
                            (argument nil))
  (:documentation "Represents assign command."))

(defunclass if-command ((check nil)
                        (command nil))
  (:documentation "Represents if command."))

(defunclass clear-command ()
  (:documentation "Represents clear command."))

(defunclass poke-command ((x nil)
                          (y nil)
                          (e nil))
  (:documentation "Represents poke command."))

(defunclass input-command ((value nil))
  (:documentation "Represents input command."))

(defunclass stop-command ()
  (:documentation "Represents stop command."))

(defunclass state ((program nil)
                   (context (make-hash-table))
                   (screen (default-screen)))
  (:documentation "Represents state of the program."))

(defun default-screen ()
  (make-array '(20 60) :initial-element " "))

(defun poke-screen (state x y c)
  (let ((scr (get-screen state)))
    (setf (aref scr x y) c)
    state))

(defun clear-screen (state)
  (let ((p (get-program state))
        (c (get-context state)))
    (make-state p c (default-screen))))

(defun print-array (arr)
  (let ((rows (array-dimension arr 0))
        (cols (array-dimension arr 1)))
    (loop for i from 0 below rows do
          (let ((row-string ""))
            (loop for j from 0 below cols do
                  (setf row-string (concatenate 'string row-string (aref arr i j))))
            (format t "~A~%" row-string))))) 


(defun print-screen (state)
  (print-array (get-screen state))
  state)

(defun put (key val collection)
  (setf (gethash key collection) val)
  collection)

(defun contains? (key collection)
  (multiple-value-bind (val found?) (gethash key collection)
    (declare (ignore val))
    (cond
      (found? t)
      (t nil))))

(defun obtain (key collection)
  (gethash key collection))

(defun hash-table-from-list (&rest pairs)
  (reduce (lambda (collection pair)
            (put (first pair) (second pair) collection))
          pairs
          :initial-value (make-hash-table)))

(defun copy-hash-table (collection)
  (apply #'hash-table-from-list (maphash (lambda (key val)
                                           (list key val))
                                         collection)))

(defgeneric print-value (value)
  (:documentation "Pretty prints value."))

(defmethod print-value ((val string-value))
  (format t "~a" (get-value val)))

(defmethod print-value ((val number-value))
  (format t "~a" (get-value val)))

(defmethod print-value ((val boolean-value))
  (cond
    (val (format t "TRUE"))
    (t (format t "FALSE"))))

(defun get-program-line (state line)
  (cond
    ((null state) nil)
    (t (let ((fst (first state))
             (rst (rest state)))
         (cond
           ((= (car fst) line) (second fst))
           (t (get-program-line rst line)))))))

(defun add-program-line-aux (state line command)
  ( cond
    ((null state) (list (list line command)))
    (t (let ((fst (first state))
             (rst (rest state)))
         (cond
           ((= (first fst) line) (cons (list line command) rst))
           ((> (first fst) line)
            (cons (list line command) state))
           (t (cons fst (add-program-line-aux rst line command))))))))

(defun add-program-line (state line command)
  (cond
    ((null state) (list (list line command)))
    (t (let ((fst (first state)))
         (cond
           ((> (first fst) line) (cons (list line command) state))
           (t (add-program-line-aux state line command)))))))

(defun max-program-line (state)
  (apply #'max (mapcar #'first state)))

(defgeneric eval-expression (expression state)
  (:documentation "Evaluates expression."))

(defmethod eval-expression ((expr const-expression) state)
  (get-value expr))

(defun or-fn (&rest args)
  (some #'identity args))

(defmethod eval-expression ((expr function-expression) state)
  (let ((name (get-name expr))
        (arguments (mapcar (lambda (x) (eval-expression x state))
                           (get-arguments expr))))
    (cond
      ((string= name "-")
       (binary-numeric-lift #'- arguments))
      ((string= name "=")
       (binary-boolean-lift #'= arguments))
      ((string= name "RND")
       (make-number-value
        (random
         (get-value (first arguments)))))
      ((string= name "MIN")
       (make-number-value (min (get-value (first arguments))
                               (get-value (second arguments)))))
      ((string= name ">")
       (binary-relation-lift #'> arguments))
      ((string= name "<")
       (binary-relation-lift #'< arguments))
      ((string= name "||")
       (binary-boolean-lift #'or-fn arguments)))))

(defmethod eval-expression ((expr variable-expression) state)
  (let ((context (get-context state)))
    (cond
      ((contains? (get-name expr) context) (eval-expression (obtain (get-name expr) context) state))
      (t (error (format nil "Variable ~a not found." (get-name expr)))))))

(defgeneric run-command (state line command)
  (:documentation "Runs command on line with state."))

(defmethod run-command (state line (command print-command))
  (let ((exprs (mapcar (lambda (x) (eval-expression x state)) (get-expressions command))))
    (dolist (item exprs)
      (print-value item))
    (run-next-line state line)))

(defmethod run-command (state line (command run-command))
  (let ((fst-line (first (get-program state))))
    (run-command state (first fst-line) (second fst-line))))

(defmethod run-command (state line (command goto-command))
  (let ((target (get-program-line (get-program state) (get-value command))))
    (run-command state (get-value command) target)))

(defmethod run-command (state line (command assign-command))
  (let* ((expr (get-argument command))
         (name (get-name command)))
    (run-next-line
     (make-state (get-program state)
                 (put name
                      (make-const-expression (eval-expression expr state))
                      (get-context state))
                 (get-screen state))
     line)))

(defmethod run-command (state line (command if-command))
  (let* ((check (get-value (eval-expression (get-check command) state)))
         (cmd (get-command command)))
    (cond
      ((equal check T) (run-command state line cmd))
      (t (run-next-line state line)))))

(defmethod run-command (state line (command clear-command))
  (run-next-line (print-screen (clear-screen state)) line))

(defmethod run-command (state line (command poke-command))
  (let ((x (get-value (eval-expression (get-x command) state)))
        (y (get-value (eval-expression (get-y command) state)))
        (e (get-value (eval-expression (get-e command) state))))
    (run-next-line (print-screen (poke-screen state x y e)) line)))

(defmethod run-command (state line (command stop-command))
  state)

(defmethod run-command (state line (command input-command))
  (let ((value (get-value command)))
    (run-next-line
     (make-state (get-program state)
                 (put value
                      (make-const-expression (make-number-value (read)))
                      (get-context state))
                 (get-screen state))
     line)))

(defun run-next-line (state line)
  (let ((result (find-if (lambda (item) (> (first item) line))
                         (get-program state))))
    (cond
      ((null result) state)
      (t (run-command state (first result) (second result))))))

(defun run-input (state line command)
  (cond
    ((null line) (run-command state
                              (max-program-line (get-program state))
                              command))
    (t (make-state (add-program-line (get-program state) line command)
                   (get-context state)
                   (get-screen state)))))

(defun run-inputs (state lst)
  (reduce (lambda (s cmd) (run-input s (first cmd) (second cmd)))
          lst
          :initial-value state))

(defun binary-relation-lift (fn arguments)
  (let ((a (get-value (first arguments)))
        (b (get-value (second arguments))))
    (make-boolean-value (funcall fn a b))))

(defun binary-numeric-lift (fn arguments)
  (let ((a (get-value (first  arguments)))
        (b (get-value (second arguments))))
    (make-number-value (funcall fn a b))))

(defun binary-boolean-lift (fn arguments)
  (let ((a (get-value (first arguments)))
        (b (get-value (second arguments))))
    (make-boolean-value (funcall fn a b))))

(defun .num (value)
  (make-const-expression (make-number-value value)))

(defun .str (value)
  (make-const-expression (make-string-value value)))

(defun .var (name)
  (make-variable-expression name))

(defun .|| (a b)
  (make-function-expression "||" (list a b)))

(defun .< (a b)
  (make-function-expression "<" (list a b)))

(defun .> (a b)
  (make-function-expression ">" (list a b)))

(defun .- (a b)
  (make-function-expression "-" (list a b)))

(defun .= (a b)
  (make-function-expression "=" (list a b)))

(defun .@ (name &rest args)
  (make-function-expression name args))

(defun demo00 ()
  (run-command
   (make-state (list
                (list 10 (make-print-command
                          (list
                           (make-const-expression
                            (make-string-value (format nil "HELLO WORLD~%")))))))
               (make-hash-table)
               (default-screen))
   -1
   (make-run-command)))

(defun demo01 ()
  (run-command
   (make-state (list
                (list 10 (make-print-command
                          (list
                           (make-const-expression
                            (make-string-value (format nil "HELLO WORLD~%"))))))
                (list 20 (make-goto-command 10)))
               (make-hash-table)
               (default-screen))
   -1
   (make-run-command)))

(defun demo02 ()
  (run-inputs (make-state nil (make-hash-table) (default-screen))
              (list
               (list 10 (make-print-command
                         (list
                          (make-const-expression
                           (make-string-value (format nil "HELLO WORLD~%"))))))
               (list 10 (make-print-command
                         (list
                          (make-const-expression
                           (make-string-value (format nil "HELLO NPRG077~%"))))))
               (list nil (make-run-command)))))

(defun demo03 ()
  (run-inputs (make-state nil (make-hash-table) (default-screen))
              (list
               (list 10
                     (make-assign-command "S"
                                          (make-const-expression
                                           (make-string-value (format nil "HELLO WORLD~%")))))
               (list 20 (make-assign-command "I"
                                             (make-const-expression
                                              (make-number-value 1))))
               (list 30 (make-assign-command "B"
                                             (make-function-expression
                                              "="
                                              (list
                                               (make-variable-expression "I")
                                               (make-const-expression
                                                (make-number-value 1))))))
               (list 40 (make-print-command (list (make-variable-expression "S"))))
               (list 50 (make-print-command (list (make-variable-expression "I"))))
               (list 60 (make-print-command (list (make-variable-expression "B"))))
               (list nil (make-run-command)))))

(defun demo04 ()
  (run-inputs (make-state nil (make-hash-table) (default-screen))
              (list
               (list 10 (make-assign-command
                         "I"
                         (make-const-expression
                          (make-number-value 10))))
               (list 20 (make-if-command
                         (make-function-expression
                          "="
                          (list
                           (make-variable-expression "I")
                           (make-const-expression
                            (make-number-value 1))))
                         (make-goto-command 60)))
               (list 30 (make-print-command
                         (list
                          (make-const-expression
                           (make-string-value (format nil "HELLO WORLD~%"))))))
               (list 40 (make-assign-command
                         "I"
                         (make-function-expression
                          "-"
                          (list
                           (make-variable-expression "I")
                           (make-const-expression (make-number-value 1))))))
               (list 50 (make-goto-command 20))
               (list 60 (make-print-command (list (make-const-expression (make-string-value "YES")))))
               (list nil (make-run-command)))))

(defun demo05 ()
  (run-inputs (make-state nil (make-hash-table) (default-screen))
              (list
               (list 10 (make-assign-command
                         "X"
                         (make-const-expression (make-number-value 10))))
               (list 20 (make-assign-command
                         "Y"
                         (make-function-expression
                          "-"
                          (list
                           (make-variable-expression "X")
                           (make-const-expression (make-number-value 10))))))
               (list 30 (make-assign-command
                         "X"
                         (make-const-expression (make-number-value 20))))
               (list 40 (make-print-command (list (make-variable-expression "Y"))))
               (list nil (make-run-command)))))

(defun demo06 ()
  (run-inputs (make-state nil (make-hash-table) (default-screen))
              (list
               (list 10 (make-clear-command))
               (list 20 (make-poke-command
                         (.@ "RND" (.num 20))
                         (.@ "RND" (.num 60))
                         (.str "*")))
               (list 30 (make-assign-command "I" (.num 100)))
               (list 40 (make-poke-command
                         (.@ "RND" (.num 20))
                         (.@ "RND" (.num 60))
                         (.str "*")))
               (list 50 (make-assign-command "I" (.- (.var "I") (.num 1))))
               (list 60 (make-if-command (.> (.var "I") (.num 1))
                                         (make-goto-command 40)))
               (list nil (make-run-command)))))

(defun demo07 ()
  (run-inputs (make-state nil (make-hash-table) (default-screen))
              (list
               (list 10 (make-assign-command "M" (.num 20)))
               (list 20 (make-print-command
                         (list (.str "THERE ARE ")
                               (.var "M")
                               (.str " MATCHES LEFT")
                               (.str (format nil "~%")))))
               (list 30 (make-print-command
                         (list (.str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND ")
                               (.@ "MIN" (.num 5) (.var "M"))
                               (.str " MATCHES")
                               (.str (format nil "~%")))))
               (list 40 (make-print-command
                         (list (.str "HOW MANY MATCHES DO YOU TAKE?")
                               (.str (format nil "~%")))))
               (list 50 (make-input-command "P"))
               (list 60 (make-if-command
                         (.||
                           (.||
                             (.< (.var "P") (.num 1))
                             (.> (.var "P") (.num 5)))
                           (.> (.var "P") (.var "M")))
                         (make-goto-command 40)))
               (list 70 (make-assign-command "M" (.- (.var "M") (.var "P"))))
               (list 80 (make-if-command (.= (.var "M") (.num 0))
                                         (make-goto-command 200)))
               (list 90 (make-print-command
                         (list (.str "THERE ARE ")
                               (.var "M")
                               (.str " MATCHES LEFT")
                               (.str (format nil "~%")))))
               (list 100 (make-print-command
                          (list (.str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND ")
                                (.@ "MIN" (.num 5) (.var "M"))
                                (.str " MATCHES")
                                (.str (format nil "~%")))))
               (list 110 (make-print-command
                          (list (.str "HOW MANY MATCHES DO YOU TAKE?")
                                (.str (format nil "~%")))))
               (list 120 (make-input-command "P"))
               (list 130 (make-if-command
                          (.||
                            (.||
                              (.< (.var "P") (.num 1))
                              (.> (.var "P") (.num 5)))
                            (.> (.var "P") (.var "M")))
                          (make-goto-command 110)))
               (list 140 (make-assign-command "M" (.- (.var "M") (.var "P"))))
               (list 150 (make-if-command (.= (.var "M") (.num 0))
                                          (make-goto-command 220)))
               (list 160 (make-goto-command 20))
               (list 200 (make-print-command (list (.str "PLAYER 1 WINS!"))))
               (list 210 (make-stop-command))
               (list 220 (make-print-command (list (.str "PLAYER 2 WINS!"))))
               (list 230 (make-stop-command))
               (list nil (make-run-command)))))
