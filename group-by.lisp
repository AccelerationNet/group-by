(defpackage :group-by
  (:use :cl :cl-user :iter)
  (:export
   :group-by
   :categorize-item
   :grouped-list
   :make-grouped-list
   :add-item-to-grouping
   :key-value
   :child-groupings
   :items-in-group
   :parent-grouping
   :keys :tests
   :make-child-grouped-list
   :group-by-repeated
   :group-by-repeated-in-hash-table
   :group-by-repeated-in-tree
   :grouped-list-speed-tester))

(in-package :group-by)

(defclass n-ary-tree ()
  ((children :accessor children :initarg :children :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (key :initform nil :initarg :key :accessor key)
   (data :initform nil :initarg :data :accessor data)))

(defmethod add-child ((tree n-ary-tree) child)
  (setf child (typecase child
                (n-ary-tree child)
                (T (make-instance 'n-ary-tree :key child))))
  (setf (parent child) tree)
  (push child (children tree))
  child)

(defun group-by (list &key (key #'car) (value #'cdr) (key-fn #'identity) (test #'equal))
  "groups the list into an alist using the key function and value function to group by key,
with a list of all values for that key.

key is used to determine the key in the a-list
value is used to determin the value in the a-list
key-fn is passed as the :key to assoc
test is passed as the :test to assoc

eg: (group-by '((a 1 2) (a 3 4) (b 5 6)))
=> ((A (1 2) (3 4)) (B (5 6)))"
  (iter (for i in list)
        (for k = (funcall key i))
        (for v = (funcall value i))
        (for cell = (assoc k results :test test :key key-fn))
        (if cell
            (push v (cdr cell))
            (collect (list k v) into results))
        (finally
         ;; reverse the values so that they appear in the same
         ;; sort order as previously
         (return (iter (for (k .  vals) in results)
                       (collect (cons k (nreverse vals))))))))

(defgeneric categorize-item (item root &key keys tests)
  (:documentation "Insert a new item into a grouped list "))

(defmethod categorize-item (item (root list) &key keys tests)
  "Categorize a new item into an alist as produced by group-by-repeated
     This will create new category nodes if necessary"
  (if (null keys)
      (push item root)
      (let ((key (funcall (first keys) item)))
        (let ((data (assoc key root :test (or (first tests) #'equal))))
          (if data
              ;; Add the rest of the categorization to the
              ;; data of this item
              (setf (cdr data) (categorize-item
                                item (cdr data)
                                :keys (rest keys)
                                :tests (rest tests)))
              ;; we have no data for this node, build a new subtree
              (push (cons key (categorize-item
                               item nil
                               :keys (rest keys)
                               :tests (rest tests)))
                    root)))))
  root)

(defun group-by-repeated (list &key keys tests)
  "Returns an alist tree that represents the items in the list as categorized
   by keys (compared with tests)
    ex: ((a 3 sam) (c 4 bob) (a 3 ted))


   keys: a list of key functions that describe the categorizations in order
   tests: how we are testing whether or not two keys are equal, defaults to #'equal
  "
  (let (root)
    (iter (for item in list)
          (setf root (categorize-item item root :keys keys :tests tests)))
    root))

(defmethod categorize-item (item (root n-ary-tree) &key keys tests)
  (iter (with node = root)
        (with tests = tests)
        (for keyfn in keys)
        (for testfn = (or (first tests) #'equal))
        (setf tests (rest tests))
        (for key = (funcall keyfn item))
        (setf node
              (or (find key (children node) :key #'key :test testfn)
                  (add-child node key)))
        (finally (push item (data node))))
  root)

(defun group-by-repeated-in-tree (list &key keys tests)
  "returns an n-ary-tree with list items grouped by keys specified in key-tests

keys is a list of key functions used to categorize the list
tests is a list of test functions to use to compare the keys (default is #'equal)
"
  (iter
    (with root = (make-instance 'n-ary-tree))
    (for item in list)
    (categorize-item item root :keys keys :tests tests )
    (finally (return root))))

(defmethod categorize-item (item (root hash-table) &key keys tests)
  "puts an item in a hash-table grouping as produced by group-by-repeated-in-hash-table"
  (iter (with node = root)
        (for keyfn on keys)
        (for key = (funcall (first keyfn) item))
        (for testsym = (or (typecase (first tests)
                             (symbol (first tests)))
                           'equal))
        (setf tests (rest tests))
        (when (rest keyfn)
          (setf node (alexandria:ensure-gethash
                      key node
                      (make-hash-table :test testsym))))
        (finally (let ((data (gethash key node)))
                   (push item data)
                   (setf (gethash key node) data))))
  root)

(defun group-by-repeated-in-hash-table (list &key keys tests)
  "returns

keys is a list of key functions used to categorize the list
tests is a list of hashtable tests to use (default is 'equal)
  *no significant speed increase was noticed by using differnt tests*
"
  (flet ((mk-tbl () (make-hash-table :test (or (typecase (first tests)
                                                 (symbol (first tests)))
                                               'equal))))
    (iter
      (with root = (mk-tbl))
      (for item in list)
      ;;ensure the leaf node
      (categorize-item item root :keys keys :tests (rest tests))
      (finally (return root)))))

(defclass grouped-list ()
  ((orig-list :accessor orig-list :initarg :orig-list :initform nil)
   (grouping-implementation
    :accessor grouping-implementation :initarg :grouping-implementation :initform :alist
    :documentation
    "What data structure should be used to perform the grouping
       :alist, :tree , :hash-table")
   (keys :accessor keys :initarg :keys :initform nil
         :documentation "A list of key functions we will use to group the list")
   (tests :accessor tests :initarg :tests :initform nil
          :documentation "A list of test functions we will use to test key equality
      tree: defaults to #'equal
      hash-table: this be a single hash-equality symbol (defaults to 'equal)")
   (grouped-list :accessor grouped-list :initarg :grouped-list :initform nil
                 :documentation "a list grouped according to the grouping-implementation")
   (parent-grouping :accessor parent-grouping :initarg :parent :initform nil
    :documentation "If this is a subgrouping of another grouped-list, what is the parent grouping we are apart of (mostly for testing)")
   (key-value :accessor key-value :initarg :key-value :initform nil
    :documentation "If this is a subgrouping of another grouped-list, what is the key this grouped-list represents in the parent grouping (mostly for testing)"))
  (:documentation "This class represents a list that we have grouped by multiple key values
     ala one of the group-by-repeatedly functions "))

(defun make-grouped-list (inp &key tests keys (grouping-implementation :alist))
  "Given a list of input, produce a grouped-list CLOS object that contains
the original list, configuration about the groupings and the result tree
of grouped-list objects

''keys'': a list of keys to group by<br />
''tests'': a list of tests to compare the keys with<br />

''grouping-implmentation'': What data structure should be used to perform the grouping<br />
  '':alist, :tree , :hash-table''<br />
  The implementation doesnt change the output, but it does change
  the performance characteristics of the grouped-object (see:
  grouped-list-speed-tester for help deciding which to use)
  "
  (make-instance 'grouped-list
                 :tests tests
                 :keys keys
                 :grouping-implementation grouping-implementation
                 :orig-list inp))

(defmethod initialize-instance :after ((o grouped-list) &key list &allow-other-keys)
  (when list (setf (orig-list o) list))
  (unless (grouped-list o)
    (setf (grouped-list o)
          (apply
           (ecase (grouping-implementation o)
             (:hash-table #'group-by-repeated-in-hash-table)
             (:tree #'group-by-repeated-in-tree)
             (:alist #'group-by-repeated ))
           (list (orig-list o) :keys (keys o) :tests (tests o))))))

(defmethod add-item-to-grouping (item (gl grouped-list))
  "puts a new item in the grouping of the grouped list (but not in the original list)"
  (setf (grouped-list gl)
        (categorize-item item (grouped-list gl) :keys (keys gl) :tests (tests gl))))

(defmethod %group-subgroups ((l list) key-value test &optional default)
  "Returns the sub groups for the different grouping implementations"
  (or (cdr (assoc key-value l :test (or test #'equal)))
      default))

(defmethod %group-subgroups ((tr n-ary-tree) key-value test &optional default)
  "Returns the sub groups for the different grouping implementations"
  (or
   (find key-value (children tr) :key #'key :test (or test #'equal))
   default))

(defmethod %group-subgroups ((ht hash-table) key-value test &optional default)
  "Returns the sub groups for the different grouping implementations"
  (declare (ignore test))
  (gethash key-value ht default))

(defmethod %grouping-items ((l list))
  "Returns the items in a given group"
  l)

(defmethod %grouping-items ((ht hash-table))
  "Returns the items in a given group"
  (iter (for (k v) in-hashtable ht)
        (appending (%grouping-items v))))

(defmethod %grouping-items ((n null)) n)

(defmethod %grouping-items ((tn n-ary-tree))
  "Returns the items in a given group"
  (append (data tn)
          (iter (for kid in (children tn))
                (appending (%grouping-items kid)))))

(defmethod make-child-grouped-list ((gl grouped-list) key-value grouped-list)
  (make-instance
   'grouped-list
   :orig-list (orig-list gl)
   :keys (rest (keys gl))
   :tests (rest (tests gl))
   :grouping-implementation (grouping-implementation gl)
   :parent-grouping gl
   :key-value key-value
   :grouped-list grouped-list))

(defmethod %grouping-children ((gl grouped-list) (l list))
  (when (keys gl)
    (iter (for (key . value) in l)
          (collect (make-child-grouped-list gl key value)))))

(defmethod %grouping-children ((gl grouped-list) (tn n-ary-tree))
  (when (keys gl)
    (iter (for i in (children tn))
          (collect (make-child-grouped-list gl (key i) i)))))

(defmethod %grouping-children ((gl grouped-list) (h hash-table))
  (when (keys gl)
    (iter (for (key value) in-hashtable h)
          (collect (make-child-grouped-list gl key value)))))

(defmethod child-groupings ((gl grouped-list))
  (%grouping-children gl (grouped-list gl)))

(defmethod items-in-group ((gl grouped-list) &rest key-values)
  " a list of key values that will produce a list of all the items in a given group"
  (let ((subgroup (grouped-list gl))
        (tests (tests gl)))
    (iter (for key in key-values)
          (for test = (or (first tests) #'equal))
          (setf tests (rest tests))
          (setf subgroup (%group-subgroups subgroup key test)))

    ;; alists are indistinguishable from lists containing cons cells so...
    ;; do what we know is correct right here
    (when (and (eql :alist (grouping-implementation gl)))
      ;; find the number of levels of alist that we still have left
      (let ((depth-left (- (length (keys gl)) (length key-values) 1 )))
        (iter (for i from 0 to depth-left)
              (setf subgroup (iter (for (key . values) in subgroup)
                                   (appending values))))))
    ;; Get all the items for that subgrouping (for alists this is a list we just produced)
    ;; and that list will simply pass through
    (%grouping-items subgroup)))

(defun grouped-list-speed-tester (&key list keys tests hash-tests (iterations 10) actions)
  "A function to help asses which implementation will work best in your given scenario
   actions : (lambda (gl) ...) -to help test whatever grouped list
             operations you will need to do repeatedly

  "
  (format *trace-output* "Grouping Implentation Speed Tests" )
  (format *trace-output* "~%~%HASH-TABLE Implementation~%" )
  (time
   (iter (for i from 1 to iterations)
         (let ((gl (make-instance
                    'grouped-list
                    :list list :keys keys :tests hash-tests
                    :grouping-implementation :hash-table)))
           (when actions (funcall actions gl)))))
  (format *trace-output* "~%~%TREE Implementation~%" )
  (time
   (iter (for i from 1 to iterations)
         (let ((gl (make-instance 'grouped-list :list list :keys keys :tests tests
                        :grouping-implementation :tree)))
           (when actions (funcall actions gl)))
         ))
  (format *trace-output* "~%~%ALIST Implementation~%" )
  (time
   (iter (for i from 1 to iterations)
         (let ((gl (make-instance 'grouped-list :list list :keys keys :tests tests
                        :grouping-implementation :alist)))
           (when actions (funcall actions gl)))
         )))
