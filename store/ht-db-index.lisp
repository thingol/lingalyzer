(in-package :org.kjerkreit.lingalyzer.store)

;; TODO DB
;; __gc

;; TODO INDEX
;; __gc

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))

;;;; Internal support functions and macros

(defmacro get-all-of-type (db type)
  "Returns all agents in db."

  `(loop for entity being the hash-values in (slot-value ,db ,type)
       collect entity))

;;; Datatypes
;;;
;;;; DB
(defclass ht-db (lingalyzer-db)
  ((agent     :initform (make-hash-table :test 'equal)
	      :type     hash-table)
   (doc       :initform (make-hash-table :test 'equal)
	      :type     hash-table)
   (mdoc      :initform (make-hash-table :test 'equal)
	      :type     hash-table)
   (word-form :initform (make-hash-table :test 'equal)
	      :type     hash-table))
  (:documentation "Memory only db."))

;;;; Index
(defclass ht-index (lingalyzer-index)
  ((agent     :initform (make-array 200
				    :element-type 'cons
				    :adjustable   t
				    :fill-pointer 0)
	      :type     (array cons))
   (doc       :initform (make-array 150
				    :element-type 'cons
				    :adjustable   t
				    :fill-pointer 0)
	      :type     (array cons))
   (mdoc      :initform (make-array 150
				    :element-type 'cons
				    :adjustable   t
				    :fill-pointer 0)
	      :type     (array cons))
   (word-form :initform (make-array 5000
				    :element-type 'cons
				    :adjustable   t
				    :fill-pointer 0)
	      :type     (array cons))
   (wf-fuzzy  :initform (make-array 5000
				    :element-type 'cons
				    :adjustable   t
				    :fill-pointer 0)
	      :type     (array cons)))
  (:documentation "Memory only index."))
      nil
      t))

(defmethod __gc ((store ht-index) delete)
  (if delete
      nil
      t))

;;;; Store: content - general

(defmethod __add-entity ((store ht-db) (entity lingalyzer-entity))
  (setf (gethash (slot-value entity 'key) (slot-value store (type-of entity))) entity))

(defmethod __add-entity ((store ht-index) (entity lingalyzer-entity))
  (let ((key (slot-value entity 'key)))
    (vector-push-extend (cons (gen-n-grams `(,key)) key) (slot-value store (type-of entity)))))

(defmethod __add-entity ((store ht-index) (entity word-form))
  (let ((key (slot-value entity 'key)))
    (vector-push-extend (list key) (slot-value store 'word-form))
    (vector-push-extend (cons (gen-n-grams `(,key)) key) (slot-value store 'wf-fuzzy))))

(defmethod __add-entity ((store ht-index) (entity doc-index))
  (vector-push-extend (forward entity) (slot-value store 'doc))

  (dolist (wf (inverse entity))
	  (push (cdr wf) (cdr (__indexed-p store 'word-form (car wf))))))
	
(defmethod __remove-entity ((store ht-db) type key)
  (remhash key (slot-value store type)))

(defmethod __remove-entity ((store ht-index) type key)
  (let ((index (slot-value store type)))
    (if (or (eq type 'doc)
	    (eq type 'word-form))
	(setf index (remove key index :test #'equal :count 1))
	(setf index (remove (car (gen-n-grams `(,key))) index :test #'equal :count 1)))
    (when (eq type 'word-form)
      (let ((wff (slot-value store 'wf-fuzzy)))
	(setf wff (remove (car (gen-n-grams `(,key))) wff :test #'equal :count 1))))))
    

  
;;;; DB: content - general

(defmethod __get-one ((db ht-db) key type)
  (gethash key (slot-value db type)))

(defmethod __get-all ((db ht-db))
  (list (get-all-of-type db 'agent)
	(get-all-of-type db 'doc)
	(get-all-of-type db 'mdoc)
	(get-all-of-type db 'word-form)))

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
       

(defmethod __increase-wf-count ((db ht-db) (form string) (delta fixnum))
  (incf (occurred (gethash form (slot-value db 'word-form))) delta))

;;;; Index

(defmethod __indexed-p ((index ht-index) type key)
  (if (eq type 'word-form)
      (loop for entry across (slot-value index 'word-form)
	 when (string= (car entry) key)
	 return entry)
      (loop for entry across (slot-value index type)
	 when (string= (cadar entry) key)
	 return entry)))

(defmethod __find-entities ((index ht-index) type query threshold)
  (let ((matches)
	(qng (gen-n-grams query)))
    
    (map 'list #'(lambda (x)
		   (when (>= (compare-n-grams qng (car x)) threshold)
		     (setf matches (cons (cdr x) matches))))
	 (slot-value index type))
    
    matches))
