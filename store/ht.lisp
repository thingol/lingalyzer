(in-package :org.kjerkreit.lingalyzer.storage)

;; DONE DB
;; __close
;; __drop
;; __gc
;; __open
;;
;; Content
;; __add
;; __get
;; __increase-wf-count
;; __remove
;; __update

;; DONE INDEX
;; __close
;; __drop
;; __gc
;; __indexed-p
;; __open
;; __search

;; TODO DB
;; __gc
;; __get-all
;; __get-by
;; __get-childless

;; TODO INDEX
;; __gc
;; __merge


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



;;;; DB: content - general

(defmethod __add ((db ht-db) entity key)
  (setf (gethash key (slot-value db (type-of entity))) entity))

(defmethod __get ((db ht-db) type key)
  (gethash key (slot-value db 'type)))

(defmethod __get-all ((db ht-db)))

(defmethod __get-by ((db ht-db) type slot value))

(defmethod __remove ((db ht-db) type key)
  (remhash key (slot-value db (build-symbol 'type)))
  
(defmethod __update ((db ht-db) entity key)
  (__add db entity key))

;;;; DB: content - specific

(defmethod __get-childless ((db ht-db)))

(defmethod __increase-wf-count ((db ht-db) (form string))
  (incf (gethash form (wform db))))

;;;; Index

(defmethod __indexed-p ((index ht-index) type key)
  (loop for entry being the elements of (slot-value index type)
       until (string= (car entry) key)
       finally (return entry)))

(defmethod __search ((index ht-index) type query threshold)
  (let ((matches)
	(qng (gen-n-grams query)))
    
    (map 'list #'(lambda (x)
		   (when (>= (compare-n-grams qng (car x)) threshold)
		     (setf matches (cons (cdr x) matches))))
	 (slot-value index type))
    
    matches))