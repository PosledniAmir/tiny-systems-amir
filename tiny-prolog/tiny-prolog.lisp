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

(defunclass atom-term ((name nil))
  (:documentation "Atom with name."))

(defmethod print-object ((obj atom-term) stream)
  (format stream "<Atom: ~A>" (get-name obj)))

(defunclass variable-term ((name nil))
  (:documentation "Variable with name."))

(defmethod print-object ((obj variable-term) stream)
  (format stream "<Variable: ~A>" (get-name obj)))

(defunclass predicate-term ((name nil)
                            (terms nil))
  (:documentation "Predicate with name and terms."))

(defmethod print-object ((obj predicate-term) stream)
  (format stream "<Predicate: ~A~A>" (get-name obj)
          (get-terms obj)))

(defunclass call-term ((head nil)
                       (rest nil))
  (:documentation "Call with head and rest."))

(defmethod print-object ((obj call-term) stream)
  (format stream "<Call: ~A~A>" (get-head obj)
          (get-rest obj)))

(defunclass clause ((head nil)
                    (rest nil))
  (:documentation "Caluse with head and rest."))

(defun fact (head)
  (make-clause head nil))

(defun rule (head rest)
  (make-clause head rest))

(defun unify-lists (l1 l2)
  (cond
    ((and (null l1) (null l2)) (values nil t))
    ((or (null l1) (null l2)) (values nil nil))
    (t (let ((rest (multiple-value-list (unify-lists (rest l1) (rest l2))))
             (first (multiple-value-list (unify (first l1) (first l2)))))
         (cond
           ((or (null (second rest))
                (null (second first)))
            (values nil nil))
           (t (values (concatenate 'list (first first) (first rest)) t)))))))

(defgeneric unify (t1 t2)
  (:documentation "Unifies two terms if possible."))

(defmethod unify ((t1 atom-term) (t2 atom-term))
  (cond
    ((string= (get-name t1)
              (get-name t2))
     (values nil t))
    (t (values nil nil))))

(defmethod unify ((t1 predicate-term) (t2 predicate-term))
  (cond
    ((string= (get-name t1)
              (get-name t2))
     (unify-lists (get-terms t1)
                  (get-terms t2)))
    (t (values nil nil))))

(defmethod unify ((t1 variable-term) t2)
  (values (list (get-name t1) t2) t))

(defmethod unify (t1 (t2 variable-term))
  (unify t2 t1))

(defmethod unify ((t1 predicate-term) (t2 atom-term))
  (values nil nil))

(defmethod unify ((t1 atom-term) (t2 predicate-term))
  (values nil nil))

(defun .predicate (name &rest rest)
  (make-predicate-term name rest))

(defun .atom (name)
  (make-atom-term name))

(defun .variable (name)
  (make-variable-term name))

(defun print-result (result)
  (cond
    ((null (second result)) (format t "FAIL"))
    (t (format t "~A" (first result)))))

(defun demo00 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "human" (.atom "socrates"))
           (.predicate "human" (.variable "X"))))))

(defun demo01 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "human" (.atom "socrates"))
           (.predicate "mortal" (.variable "X"))))))

(defun demo02 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "parent" (.atom "charles") (.atom "harry"))
           (.predicate "parent" (.atom "charles") (.variable "X"))))))

(defun demo03 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "parent" (.variable "X") (.atom "harry"))
           (.predicate "parent" (.atom "charles") (.variable "Y"))))))

(defun demo04 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "succ"
                       (.predicate "succ"
                                   (.predicate "succ" (.atom "zero"))))
           (.predicate "succ" (.variable "X"))))))

(defun demo05 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "succ"
                       (.predicate "succ" (.atom "zero")))
           (.predicate "succ" (.atom "zero"))))))
