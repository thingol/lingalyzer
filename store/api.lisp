(in-package :org.kjerkreit.lingalyzer.storage)

;; storage api

;;; State

(defvar *db* nil)
(defvar *index* nil)

;;; Datatypes

(defclass lingalyzer-db ()
  "Toplevel db class."
  ((name  :accessor name
	  :initarg  :name
	  :initform "Unnamed"
	  :type     'string)
   (state :reader   db-dirty-p
	  :writer   db-dirty
	  :initform nil)))

(defclass lingalyzer-index ()
  "Toplevel index class."
  ((state :reader   index-dirty-p
	  :writer   index-dirty
	  :initform nil)))

(defstruct agent
  "Agent: authors or scribes."
  (name     "John Doe"  :type string)
  (bday     "000-00-00" :type string)
  (dday     "000-00-00" :type string)
  (authored nil         :type list)
  (copied   nil         :type list))

(defstruct doc
  "Documents, an actual version of the text, of which multipe make up an mdoc."
  (mdoc     nil         :type array)
  (scribe   "John Doe"  :type string)
  (length   0           :type integer)
  (hash     nil         :type array))

(defstruct mdoc
  "Meta documents, container for copies of a document."
  (name     "A tale"    :type string)
  (author   "John Doe"  :type string)
  (genre    "Unknown"   :type string)
  (docs     nil         :type list)
  (hash     nil         :type array))

(defstruct word-form
  "Word forms, all forms are stored separately."
  (form     nil         :type string)
  (count    1           :type integer))


;;; Public functions

;;;; INIT

(defun new-store (name &optional (dtype 'ht) (itype 'ht))
  "Create a new database and index of the desired types."

  (setf *db*    (make-instance dtype))
  (setf *index* (make-instance itype)))

;;;; DB

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

;;;; DB: content - general

(defun add (entity)
  "Add an entity to the database."

  (__add *db* entity))

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

(defun remove (key type)
  "Remove an entity from the database."

  (and (__remove *db* key type)
       (dirty-db *db* t)))

(defun update (entity)
  "Update an entity in the database."
  
  (and (db-dirty *db*)
       (__update *db* entity)))

(defun search (key &optional (type nil))
  "Search the entire db or a specific table for a matching key."

  (__search *db* key type))

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

;;; Private functions

;;;; Common: db and index

(defgeneric __close (store))

(defgeneric __drop (store))

(defgeneric __gc (store rem-ent))

(defgeneric __open (dtype itype name))

;;;; DB: content - general 

(defgeneric __add (db entity key))

(defgeneric __get (db type key))

(defgeneric __get-all (db))

(defgeneric __get-by (db type slot value))

(defgeneric __remove (db type key))

(defgeneric __update (db entity key))

;;;; DB: content - specific

(defgeneric __get-childless (db))

(defgeneric __increase-wf-count (db wf))

;;;; Index

(defgeneric __indexed-p (index type key))

(defgeneric __merge (index partial-index))

(defgeneric __sync-with-db (index db))

(defgeneric __search (index type key))
