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

(defmethod print-object ((obj clause) stream)
  (format stream "<Clause: ~A :- ~A>"
          (get-head obj)
          (get-rest obj)))
  
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

(defgeneric substitute-aux (subst term)
  (:documentation "Substitutes subst in term."))

(defmethod substitute-aux (subst (term atom-term))
  term)

(defmethod substitute-aux (subst (term variable-term))
  (let ((result (multiple-value-list (obtain (get-name term)
                                             subst))))
    (cond
      ((null (second result)) term)
      (t (first result)))))

(defmethod substitute-aux (subst (term predicate-term))
  (let ((result (mapcar (lambda (item)
                          (substitute-aux subst item))
                        (get-terms term))))
    (make-predicate-term (get-name term) result)))

(defun substitute-subst (new subst)
  (cond
    ((null subst) nil)
    (t (cons (first subst)
             (cons (substitute-aux new (second subst))
                   (substitute-subst new (rest (rest subst))))))))

(defun substitute-terms (subst terms)
  (cond
    ((null terms) nil)
    (t (cons (substitute-aux subst (first terms))
             (substitute-terms subst (rest terms))))))

(defun unify-lists (l1 l2)
  (cond
    ((and (null l1) (null l2)) (values nil t))
    ((or (null l1) (null l2)) (values nil nil))
    (t (let* ((s1 (multiple-value-list (unify (first l1) (first l2))))
              (map (substitution-map-from-list (first s1)))
              (t1 (substitute-terms map (rest l1)))
              (t2 (substitute-terms map (rest l2)))
              (s2 (multiple-value-list (unify-lists t1 t2)))
              (new-map (substitution-map-from-list (first s2)))
              (result (substitute-subst new-map (first s1))))
         (cond
           ((or (null (second s1))
                (null (second s2)))
            (values nil nil))
           (t (values (concatenate 'list result (first s2)) t)))))))

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

(defvar *variable-counter* 0)
(defun new-variable ()
  (incf *variable-counter*)
  (format nil "_~D" *variable-counter*))

(defgeneric free-variables (term)
  (:documentation "Obtains list of variables from term."))

(defmethod free-variables ((term atom-term))
  nil)

(defmethod free-variables ((term variable-term))
  (list (get-name term)))

(defmethod free-variables ((term predicate-term))
  (apply #'concatenate
         'list
         (mapcar #'free-variables (get-terms term))))

(defun give-fresh-variables (clause)
  (let* ((tlist (cons (get-head clause)
                      (get-rest clause)))
         (variables (remove-duplicates
                     (apply #'concatenate
                            'list
                            (mapcar #'free-variables tlist))))
         (renamed (mapcar (lambda (var)
                            (make-variable-term
                             (concatenate 'string var (new-variable))))
                          variables))
         (subst (mapcar #'list variables renamed))
         (fresh (substitute-terms (apply #'substitution-map-from-list subst) tlist)))
    (make-clause (first fresh) (rest fresh))))

(defun query (clauses query)
  (cond
    ((null clauses) nil)
    (t (let* ((fresh (give-fresh-variables (first clauses)))
             (unif (multiple-value-list (unify (get-head fresh) query))))
         (cond
           ((null (second unif)) (query (rest clauses) query))
           (t (cons (list fresh (first unif))
                    (query (rest clauses) query))))))))

(defun pair-up (list)
  (cond
    ((null list) nil)
    (t (cons (list (first list) (second list))
             (pair-up (rest (rest list)))))))

(defun solve-aux (clauses subst goals matches)
  (cond
    ((null matches) nil)
    (t (let* ((match (first matches))
              (clause (first match))
              (new-goals (concatenate 'list (get-rest clause) goals))
              (new-subst (second match))
              (new-map (apply #'substitution-map-from-list (pair-up new-subst)))
              (subst-goals (substitute-terms new-map new-goals))
              (fin-subst (concatenate 'list new-subst (substitute-subst new-map subst))))
         (cons (solve clauses fin-subst subst-goals)
               (solve-aux clauses subst goals (rest matches)))))))

(defun solve (clauses subst goals)
  (cond
    ((null goals) (list 'result subst))
    (t (let* ((goal (first goals))
              (rest (rest goals))
              (matches (query clauses goal)))
         (solve-aux clauses subst rest matches)))))

(defun filter-results (list)
  (cond
    ((null list) (list))
    ((eq (first list) 'result) (mapcar #'format-term (second list)))
    ((null (first list))
     (filter-results (rest list)))
    (t (concatenate 'list
                    (filter-results (first list))
                    (filter-results (rest list))))))

(defgeneric parse-number? (term)
  (:documentation "Tries to parse number, returns nil if it is not a number"))

(defmethod parse-number? ((term predicate-term))
  (cond
    ((and (string= (get-name term) "succ")
          (not (null (get-terms term)))
          (null (rest (get-terms term))))
     (let ((result (parse-number? (first (get-terms term)))))
       (cond
         ((null result) nil)
         (t (+ 1 result)))))
    (t nil)))

(defmethod parse-number? ((term atom-term))
  (cond
    ((string= (get-name term) "zero") 0)
    (t nil)))

(defmethod parse-number? ((term variable-term))
  nil)

(defgeneric format-term (term)
  (:documentation "Pretty prints the term."))

(defmethod format-term ((term variable-term))
  (get-name term))

(defmethod format-term ((term atom-term))
  (let ((result (parse-number? term)))
    (cond
      ((null result) (get-name term))
      (t (format nil "~A" result)))))

(defmethod format-term ((term predicate-term))
  (let ((result (parse-number? term)))
    (cond
      ((null result) (format nil "~A~A" (get-name term) (mapcar #'format-term (get-terms term))))
      (t (format nil "~A" result)))))

(defmethod format-term ((term string))
  term)

(defun solve-pretty (clauses subst goals)
  (pair-up (filter-results (solve clauses subst goals))))

(defun .predicate (name &rest rest)
  (make-predicate-term name rest))

(defun .atom (name)
  (make-atom-term name))

(defun .variable (name)
  (make-variable-term name))

(defun .rule (head &rest rest)
  (make-clause head rest))

(defun .fact (head)
  (make-clause head nil))

(defun .num (n)
  (cond
    ((> n 0) (.predicate "succ" (.num (- n 1))))
    (t (.atom "zero"))))

(defun print-result (result)
  (cond
    ((null (second result)) (format t "FAIL"))
    (t (first result))))

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

(defun demo06 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "loves" (.atom "narcissus") (.atom "narcissus"))
           (.predicate "loves" (.variable "X") (.variable "X"))))))

(defun demo07 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "loves" (.atom "odysseus") (.atom "penelope"))
           (.predicate "loves" (.variable "X") (.variable "X"))))))

(defun demo08 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "add"
                       (.atom "zero")
                       (.predicate "succ" (.atom "zero")))
           (.predicate "add"
                       (.variable "Y")
                       (.predicate "succ" (.variable "Y")))))))

(defun demo09 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "loves" (.variable "X") (.atom "narcissus"))
           (.predicate "loves" (.variable "Y") (.variable "X"))))))

(defun demo10 ()
  (print-result
   (multiple-value-list
    (unify (.predicate "add"
                       (.predicate "succ"
                                   (.variable "X"))
                       (.variable "X"))
           (.predicate "add"
                       (.variable "Y")
                       (.predicate "succ"
                                   (.variable "Z")))))))

(defun demo11 ()
  (give-fresh-variables (.rule (.predicate "grandparent"
                                           (.variable "X")
                                           (.variable "Y"))
                               (.predicate "parent"
                                           (.variable "X")
                                           (.variable "Z"))
                               (.predicate "parent"
                                           (.variable "Z")
                                           (.variable "Y")))))

(defvar *family* 
  (list 
   (.fact (.predicate "male" (.atom "William")))
   (.fact (.predicate "female" (.atom "Diana")))
   (.fact (.predicate "male" (.atom "Charles")))
   (.fact (.predicate "male" (.atom "George")))
   (.fact (.predicate "parent" (.atom "Diana") (.atom "William")))
   (.fact (.predicate "parent" (.atom "Charles") (.atom "William")))
   (.fact (.predicate "parent" (.atom "William") (.atom "George")))
   (.rule (.predicate "father" (.variable "X") (.variable "Y"))
          (.predicate "parent" (.variable "X") (.variable "Y"))
          (.predicate "male" (.variable "X")))))

(defun demo12 ()
  (query *family* (.predicate "male" (.variable "X"))))

(defun demo13 ()
  (query *family* (.predicate "father" (.variable "X") (.atom "William"))))

(defun demo14 ()
  (solve-pretty *family* nil (list (.predicate "father" (.variable "X") (.atom "William")))))

(defun demo15 ()
  (solve-pretty *family* nil (list (.predicate "father" (.variable "X") (.variable "Y")))))

(defvar *nums*
  (list
   (.fact (.predicate "add"
                      (.atom "zero")
                      (.variable "X")
                      (.variable "X")))
   (.rule (.predicate "add"
                      (.predicate "succ" (.variable "X"))
                      (.variable "Y")
                      (.predicate "succ" (.variable "Z")))
          (.predicate "add"
                      (.variable "X")
                      (.variable "Y")
                      (.variable "Z")))
   (.fact (.predicate "eq" (.variable "X") (.variable "X")))))

(defun demo16 ()
  (solve-pretty *nums* nil (list (.predicate "add" (.num 2) (.num 3) (.variable "X")))))

(defun demo17 ()
  (solve-pretty *nums* nil (list (.predicate "add" (.num 2) (.variable "Y") (.variable "X")))))

