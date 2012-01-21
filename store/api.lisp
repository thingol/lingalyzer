(in-package :org.kjerkreit.lingalyzer.store)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))

;;;; INIT

(defun new-store (name &optional (dtype 'ht-db) (itype 'ht-index) (debug nil))
  "Create a new database and index of the desired types."

  (and (setf *db*    (make-instance dtype))
       (setf *index* (make-instance itype))
       (or (and debug
		(cons *db* *index*))
	   t)))

;;;; Store

(defun close-store ()
  "Closes the store."
  
  (__close-store *db*)
  (__close-store *index*))

(defun drop ()
  "Drops the currently open store."
  
  (__drop *db*)
  (__drop *index*))

(defun gc (&optional (delete nil))
  "Marks orphaned entities as such, or removes them."

  (and
   (or (not (dirty *db*))
       (and
	(__gc  *db* delete)
	(setf (dirty *db*) nil)))
   (or (not (dirty *index*))
       (and
	(__gc  *index* delete)
	(setf (dirty *index*) nil)))))

(defun open-store (name)
  "Open a store. Close the currently open store (if one exists)."

  (print name)
  
  (and (if *db*
	   (__close-store *db*)
	   t)
       (if *index*
	   (__close-store *index*)
	   t)
       '(__open-store name)))

;;;; Store: content - general

(defun add-entity (entity &optional (partial-index nil))
  "Add an entity to the store. Agents and meta documents are indexed using their names. Documents
are stored in a forward index, and word forms in a inverse index."

  (__add-entity *db* entity)
  (if partial-index
      (__add-entity *index* partial-index)
      (__add-entity *index* entity)))

(defun remove-entity (key type)
  "Remove an entity from the store."

  (and (__remove-entity *db* key type)
       (__remove-entity *index* key type)
       (setf (dirty *db*) t)
       (setf (dirty *index*) t)))

;;;; DB: content - general


(defun exists-p (key &optional (type nil))
  "Synonym for (get-one ...)."

  (__get-one *db* key type))

(defun get-one (key &optional (type nil))
  "Get matching entity from the database."

  (__get-one *db* key type))

(defun get-all ()
  "Get all entities in the entire db."

  (__get-all *db*))

(defun get-by (type slot value)
  "Get all entities mathing value of specified slot."

  (__get-by *db* type slot value))

(defun update (entity)
  "Update an entity in the database."
  
  (and (setf (dirty *db*) t)
       (__update *db* entity)))

;;;; DB: content - specific

(defun get-childless ()
  "Return a list of all agents with no meta documents or documents."


  (unless (gc)
    (error "Garbage collection failed! The integrity of the store has been compromised!"))

  (__get-childless *db*))
  
(defun get-orphans ()
  "Return all orphaned meta documents and documents."

  (unless (gc)
    (error "Garbage collection failed! The integrity of the store has been compromised!"))

  (cons (__get-by *db* 'mdoc :author "John Doe")
	(__get-by *db* 'doc  :scribe "John Doe")))

(defun increase-wf-count (form &optional (delta 1))
  "Increase the count of a word form."
  
  (__increase-wf-count *db* form delta))

;;;; Index

(defun indexed-p (key &optional (type nil))
  "Is the key in the index?"

  (__indexed-p *index* key type))

(defun find-entities (query &optional (type nil) (threshold 0.65))
  "Search the entire index or a table for a matching key."

  (__find-entities *index* query type threshold))
