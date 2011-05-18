(in-package :group-by)

(defparameter +example-timeclock-data+
  `(("russ" 1 :proj-a)
    ("russ" 2 :proj-a)
    ("bob" 1  :proj-a)
    ("russ" 2 :proj-b)
    ("bob" 1 :proj-b)
    ("bob" 1 :proj-b)
    ("russ" 2  :proj-b)
    ("bob" 1 :proj-c)
    ("russ" 4 :proj-c)))

(group-by +example-timeclock-data+)
;; results in
'(("russ"
   (1 :proj-a) (2 :proj-a) (2 "time on proj b")
   (2 "time on proj b") (4 :proj-c))
  ("bob"
   (1 :proj-a) (1 "time on proj b") (1 "time on proj b")
   (1 :proj-c)))

(defparameter +example-multiple-grouped-timeclock-data+
  (group-by-repeated
   +example-timeclock-data+
   :keys (list #'first #'third)
   :tests (list #'string-equal #'eql)))
;; results in
'(("bob"
   (:proj-c ("bob" 1 :proj-c))
   (:proj-b ("bob" 1 :proj-b) ("bob" 1 :proj-b))
   (:proj-a ("bob" 1 :proj-a)))
  ("russ"
   (:proj-c ("russ" 4 :proj-c))
   (:proj-b ("russ" 2 :proj-b) ("russ" 2 :proj-b))
   (:proj-a ("russ" 2 :proj-a) ("russ" 1 :proj-a))))

(defparameter +example-grouped-list-timeclock-data-alist+
  (make-grouped-list
   +example-timeclock-data+
   :keys (list #'first #'third)
   :tests (list #'string-equal #'eql)))

(defclass example-timeclock-record ()
  ((name :accessor name :initarg :name :initform nil)
   (hours :accessor hours :initarg :hours :initform 0)
   (proj :accessor proj :initarg :proj :initform nil)))

(defun example-tcr (name hours proj)
  (make-instance 'example-timeclock-record :name name :hours hours :proj proj))

(defparameter +example-timeclock-objs+
  (iter top (for i from 0 to 10)
	(iter (for rec in +example-timeclock-data+)
	      (in top (collect (apply #'example-tcr rec))))))

(defun print-timeclock-report ()
  (labels ((print-results (gl &optional (spaces ""))
	     (let ((further-groups (child-groupings gl)))
	       (if further-groups
		   (iter
		     (with hours = 0)
		     (for group in further-groups)
		     (format T "~?~A~%" spaces () (key-value group) )
		     (incf hours (print-results group (concatenate 'string spaces "~2,1@T")))
		     (finally
		      (format T "~?Total: ~D~%" spaces () hours )
		      (return hours)))
		   (iter (for kid in (items-in-group gl))
			 (sum (hours kid) into hours)
			 (finally
			  (format T "~?Total: ~D~%" spaces () hours )
			  (return hours)))))))
    
    (let ((by-person-project
	   (make-grouped-list
	    +example-timeclock-objs+
	    :keys (list #'name #'proj)
	    :tests (list #'string-equal #'eql)
	    :grouping-implementation :hash-table))
	  (by-project-person
	   (make-grouped-list
	    +example-timeclock-objs+
	    :keys (list #'proj #'name)
	    :tests (list #'eql #'string-equal)
	    :grouping-implementation :tree)))
      
      (format T "Hours BY Project > Person~%-----------~%")
      (print-results by-project-person)
      (format T "~%~%Hours BY Person > Project~%-----------~%")
      (print-results by-person-project)
      )))

#|
Hours BY Project > Person
-----------
PROJ-C
  russ
    Total: 44
  bob
    Total: 11
  Total: 55
PROJ-B
  bob
    Total: 22
  russ
    Total: 44
  Total: 66
PROJ-A
  bob
    Total: 11
  russ
    Total: 33
  Total: 44
Total: 165


Hours BY Person > Project
-----------
russ
  PROJ-A
    Total: 33
  PROJ-B
    Total: 44
  PROJ-C
    Total: 44
  Total: 121
bob
  PROJ-A
    Total: 11
  PROJ-B
    Total: 22
  PROJ-C
    Total: 11
  Total: 44
Total: 165
|#

