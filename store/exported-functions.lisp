(in-package :org.kjerkreit.lingalyzer.store)

;;;; INIT

(defun new-store (name &optional (dtype 'ht-db) (itype 'ht-index))
  "Create a new database and index of the desired types."

  (and (setf *db*    (make-instance dtype))
       (setf *index* (make-instance itype))))

;;;; Store

(defun close ()
  "Closes the store."
  
  (__close *db*)
  (__close *index))

(defun drop ()
  "Drops the currently open store."
  
  (__drop *db*)
  (__drop *index))

(defun gc (&optional (delete nil))
  "Marks orphaned entities as such, or removes them."

  (and (__gc *db*     delete) (dirty-db    *db*    nil)
       (__gc *index*)         (dirty-index *index* nil)))

(defun open (name)
  "Open a store. Close the currently open store (if one exists)."

  (and (if *db*
	   (__close *db*)
	   t)
       (if *index*
	   (__close *index*)
	   t)
       (__open name)))

;;;; Store: content - general

(defun add (entity &optional (partial-index nil))
  "Add an entity to the store. Agents and meta documents are indexed using their names. Documents
are stored in a forward index, and word forms in a inverse index."

  (__add *db* entity)
  (if partial-index
      (__add *index* partial-index)
      (__add *index* entity)))

(defun remove (key type)
  "Remove an entity from the store."

  (and (__remove *db* key type)
       (__remove *index* key type)
       (dirty-db *store* t)
       (dirty-index *store* t)))

;;;; DB: content - general


(defun exists-p (key &optional (type nil))
  "Synonym for (get ...)."

  (__get *db* key type))

(defun get (key &optional (type nil))
  "Get all matching entities from the database."

  (__get *db* key type))

(defun get-all ()
  "Get all entities in the entire db."

  (__get-all *db*))

(defun get-by (type slot value)
  "Get all entities mathing value of specified slot."

  (__get-by *db* type slot value))



(defun update (entity)
  "Update an entity in the database."
  
  (and (db-dirty *db*)
       (__update *db* entity)))


;;;; DB: content - specific

(defun get-childless ()
  "Return a list of all agents with no meta documents or documents."

  (when (db-dirty-p *db*)
    (gc-db))

  (__get-childless *db*))
  
(defun get-orphans ()
  "Return all orphaned meta documents and documents."

  (when (db-dirty-p *db*)
    (gc-db))

  (cons (__get-by *db* 'mdoc :author "John Doe")
	(__get-by *db* 'doc  :scribe "John Doe")))

(defun increase-wf-count (form)
  "Increase the count of a word form."

  (and (exists-p form 'wform)
       (__increase-wf-count *db* form)))

;;;; Index

(defun indexed-p (key &optional (type nil))
  "Is the key in the index?"

  (__indexed-p *index* key type))

(defun search (key &optional (type nil))
  "Search the entire index or for a specific type for a matching key."

  (__search *index* query type))
