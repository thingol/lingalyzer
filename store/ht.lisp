(in-package :org.kjerkreit.lingalyzer.storage)

;; TODO
;; __gc
;; __get-by
;; __get-childless
;; __get-orphans


;;;; Class definition for the memory only db and index types

(defclass ht-db (lingalyzer-db)
  ((agent :initform (make-hash-table :test 'equal))
   (doc   :initform (make-hash-table :test 'equal))
   (mdoc  :initform (make-hash-table :test 'equal))
   (wform :initform (make-hash-table :test 'equal))))

(defclass ht-index (lingalyzer-index)
  ((agent :initform (make-array 200
				:element-type 'cons
				:adjustable   t
				:fill-pointer 0))
   (doc   :initform (make-array 150
				:element-type 'cons
				:adjustable   t
				:fill-pointer 0))
   (mdoc  :initform (make-array 150
				:element-type 'cons
				:adjustable   t
				:fill-pointer 0))
   (wform :initform (make-array 5000
				:element-type 'cons
				:adjustable   t
				:fill-pointer 0))))

;;;; low level operations
(defmethod __close ((store ht-db))
  (__drop store))

(defmethod __close ((store ht-index))
  (__drop store))

(defmethod __drop ((store ht-db))
  (setf *db* nil))

(defmethod __drop ((store ht-index))
  (setf *index* nil))

(defmethod __gc   ((store ht-db) delete))

(defmethod __gc   ((store ht-index) delete))



;;;; Content

(defmethod __add ((db ht-db) entity key)
  (setf (gethash key (slot-value db (type-of entity))) entity))

(defmethod __get ((db ht-db) type key)
  (gethash key (slot-value db 'type)))

(defmethod __get-all ((db ht-db)))

(defmethod __get-by ((db ht-db) type slot value))

(defmethod __get-childless ((db ht-db)))

(defmethod __get-orhpans ((db ht-db)))

(defmethod __increase-wf-count ((db ht-db) (form string))
  (incf (gethash form (wform db))))

(defmethod __remove ((db ht-db) type key)
  (remhash key (slot-value db (build-symbol 'type)))
  
(defmethod __update ((db ht-db) entity key)
  (__add db entity key))

;;;;; search

(defmethod __search ((db ht-db) query entity-type)
  (if entity-type
      (etypecase entity-type
	(:agent
	 

  )

;;;; internal support functions
(defun index-lookup (index query threshold)
  "Search the specified index."

  (let ((matches)
	(query-ngrams (gen-n-grams query)))
    
    (map 'list #'(lambda (x)
		   (when (>= (compare-n-grams query-ngrams (car x)) threshold)
		     (setf matches (cons (cdr x) matches))))
	 (get-index type))
    
    matches))