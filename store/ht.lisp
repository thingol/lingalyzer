(in-package :org.kjerkreit.lingalyzer.store)

;; TODO DB
;; __gc

;; TODO INDEX
;; __gc
;; __add

;;; Datatypes
;;;
;;;; DB
(defclass ht-db (lingalyzer-db)
  ((agent :initform (make-hash-table :test 'equal))
   (doc   :initform (make-hash-table :test 'equal))
   (mdoc  :initform (make-hash-table :test 'equal))
   (wform :initform (make-hash-table :test 'equal)))
  (:documentation "Memory only db."))

;;;; Index
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
				:fill-pointer 0)))
  (:documentation "Memory only index."))

;;; API
;;;
;;;; Store
(defmethod __close ((store ht-db))
  (__drop store))

(defmethod __close ((store ht-index))
  (__drop store))

(defmethod __drop ((store ht-db))
  (setf *db* nil))

(defmethod __drop ((store ht-index))
  (setf *index* nil))

(defmethod __gc   ((store ht-db) delete)
  (if delete
      nil
      (progn
	)))

(defmethod __gc   ((store ht-index) delete))

;;;; Store: content - general

(defmethod __add ((store ht-db) (entity lingalyzer-entity))
  (setf (gethash (slot-value entity 'key) (slot-value db (type-of entity))) entity))

(defmethod __add ((store ht-index) (entity lingalyzer-entity))

(defmethod __add ((store ht-index) (entity indexed-doc))
  (vector-push-extend (car indexed-doc) (slot-value store 'doc))
  
  (loop for wf being the elements of (cdr indexed-doc)
       when 
    (__indexed-p 
    
  
  
  

(defmethod __remove ((store ht-db) type key)
  (remhash key (slot-value db 'type)))

;;;; DB: content - general

(defmethod __get ((db ht-db) type key)
  (gethash key (slot-value db 'type)))

(defmethod __get-all ((db ht-db))
  (list (get-all-of-type db 'agent)
	(get-all-of-type db 'doc)
	(get-all-of-type db 'mdoc)
	(get-all-of-type db 'wform)))

(defmethod __get-by ((db ht-db) type slot value)
  (loop for entry being the hash-values of (slot-value db type)
       when (string= (slot-value entry slot) value)
       collect entry))
  
(defmethod __update ((db ht-db) entity key)
  (__add db entity key))

;;;; DB: content - specific

(defmethod __get-childless ((db ht-db))
  (let ((agents (get-all-of-type db 'agent))
	(docs   (get-all-of-type db 'doc))
	(mdocs  (get-all-of-type db 'mdoc)))
    (dolist (doc docs)
      (let ((scribe (slot-value doc 'scribe)))
      (setf agents (remove-if #'(lambda (x) (string= scribe (slot-value x 'name))) agents))))
    (dolist (doc docs)
      (let ((author (slot-value doc 'author)))
      (setf agents (remove-if #'(lambda (x) (string= author (slot-value x 'name))) agents))))
    agents))
       

(defmethod __increase-wf-count ((db ht-db) (form string))
  (incf (count (gethash form (wform db)))))

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

;;;; Internal support functions

(defmacro get-all-of-type (db type)
  "Returns all agents in db."

  `(loop for entity being the hash-values in (slot-value ,db ,type)
       collect entity))