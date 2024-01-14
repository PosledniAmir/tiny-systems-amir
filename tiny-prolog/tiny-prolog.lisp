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
  
(defunclass substitution-map ((map (make-hash-table)))
  (:documentation "Hashtable for substitution."))

(defun empty-map ()
  (make-instance 'substitution-map))

(defun substitution-map-from-list (&rest pairs)
  (reduce (lambda (collection pair)
            (put! (first pair) (second pair) collection))
          pairs
          :initial-value (empty-map)))

(defgeneric put! (key value collection)
  (:documentation "Puts value under key in collection."))

(defmethod put! (key value (collection substitution-map))
  (setf (gethash key (get-map collection)) value)
  collection)

(defgeneric put (key value collection)
  (:documentation "Puts value under key in collection."))

(defmethod put (key value (collection substitution-map))
  (let ((item (put! key value (copy collection))))
    item))

(defgeneric contains? (key collection)
  (:documentation "Checks whether key is in collection."))

(defmethod contains? (key (collection substitution-map))
  (multiple-value-bind (value found?) (gethash key (get-map collection))
    (declare (ignore value))
    (cond
      (found? t)
      (t nil))))

(defgeneric obtain (key collection)
  (:documentation "Obtains value according to key from collection."))

(defmethod obtain (key (collection substitution-map))
  (gethash key (get-map collection)))

(defgeneric to-list (collection)
  (:documentation "Converts collection to list."))

(defmethod to-list ((collection substitution-map))
  (loop for key being the hash-keys of (get-map collection) using (hash-value value)
        collect (list key value)))

(defgeneric copy (collection)
  (:documentation "Copies the collection"))

(defmethod copy ((collection substitution-map))
  (apply #'substitution-map-from-list (to-list collection)))

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