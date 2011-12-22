(in-package :org.kjerkreit.lingalyzer.store)

;; TODO DB
;; __gc

;; TODO INDEX
;; __gc
;; __add-entity
;; __remove-entity

;;;; Internal support functions and macros

(defmacro get-all-of-type (db type)
  "Returns all agents in db."

  `(loop for entity being the hash-values in (slot-value ,db ,type)
       collect entity))

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
(defmethod __close-store ((store ht-db))
  (__drop store))

(defmethod __close-store ((store ht-index))
  (__drop store))

(defmethod __drop ((store ht-db))
  (setf *db* nil))

(defmethod __drop ((store ht-index))
  (setf *index* nil))

(defmethod __gc   ((store ht-db) delete)
  (if delete
      nil
      t))

(defmethod __gc   ((store ht-index) delete)
  (if delete
      nil
      t))

;;;; Store: content - general

(defmethod __add-entity ((store ht-db) (entity lingalyzer-entity))
  (setf (gethash (slot-value entity 'key) (slot-value store (type-of entity))) entity))

(defmethod __add-entity ((store ht-index) (entity lingalyzer-entity))
  t)

(defmethod __add-entity ((store ht-index) (entity indexed-doc))
  (vector-push-extend (car entity) (slot-value store 'doc))


  (dolist (wf (cdr entity))
    (let ((indexed (__indexed-p store 'wform (car wf))))
      (if indexed
	  t
          t)))
	
  '(loop for wf being the elements of (cdr entity)
	when (__indexed-p store 'wform (car wf))
	
	
	))

(defmethod __remove-entity ((store ht-db) type key)
  (remhash key (slot-value store 'type)))

;;;; DB: content - general

(defmethod __get-one ((db ht-db) type key)
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
  
(defmethod __update ((db ht-db) entity)
  (__add-entity db entity))

;;;; DB: content - specific

(defmethod __get-childless ((db ht-db))
  (let ((agents (get-all-of-type db 'agent))
	(docs   (get-all-of-type db 'doc))
	(mdocs  (get-all-of-type db 'mdoc)))
    (dolist (doc docs)
      (let ((scribe (slot-value doc 'scribe)))
      (setf agents (remove-if #'(lambda (x) (string= scribe (slot-value x 'name))) agents))))
    (dolist (mdoc mdocs)
      (let ((author (slot-value mdoc 'author)))
      (setf agents (remove-if #'(lambda (x) (string= author (slot-value x 'name))) agents))))
    agents))
       

(defmethod __increase-wf-count ((db ht-db) (form string))
  (incf (occurred (gethash form (slot-value db 'wform)))))

;;;; Index

(defmethod __indexed-p ((index ht-index) type key)
  (loop for entry being the elements of (slot-value index type)
       until (string= (car entry) key)
       finally (return entry)))

(defmethod __find-entities ((index ht-index) type query threshold)
  (let ((matches)
	(qng (gen-n-grams query)))
    
    (map 'list #'(lambda (x)
		   (when (>= (compare-n-grams qng (car x)) threshold)
		     (setf matches (cons (cdr x) matches))))
	 (slot-value index type))
    
    matches))
