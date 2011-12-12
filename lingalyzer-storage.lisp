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

(defun new-db (name &optional (type 'ht))
  "Create a new database of the desired type."

  (setf *db* (make-instance type)))

;;;; DB

(defun close-db ()
  "Closes the db."
  
  (__close-db *db*))

(defun drop-db ()
  "Drops the currently open db."
  
  (__drop-db *db*))

(defun gc-db ((&optional rem-ent nil))
  "Marks orphaned entities as such, or removes them."

  (__gc-db *db* rem-ent)
  (dirty-db *db* nil))

(defun open-db (type name)
  "Open a db. Close the currently open db (if one exists)."
  
  (when *db*
    (__close-db *db*))

  (__open type name))

;;;; Content

(defun add (entity)
  "Add an entity to the database."

  (__add *db* entity))

(defun exists-p (query (&optional type nil))
  "Do we have a matching entity?"

  (__exists-p *db* query type))

(defun get (entity (&optional type nil))
  "Get an entity from the database."

  (__get *db* entity type))

(defun get-orphans ()
  "Return all orphaned entities"

  (when (db-dirty-p *db*)
    (gc-db))
  )

(defun increase-wf-count (wf)
  "Increase the count of a word form."

  (__increase-wf-count *db* wf))

(defun remove (entity)
  "Remove an entity from the database."

  (__remove *db* entity)
  (dirty-db *db* t))

(defun update (entity)
  "Update an entity in the database."

  (__update *db* entity))

(defun search (query (&optional entity-type nil))
  "Search the entire db or a specific table."

  (__search *db* query type))


;;; Private functions

;;;; DB

(defgeneric __close-db (db))

(defgeneric __drop-db (db))

(defgeneric __gc-db (db rem-ent))

(defgeneric __open-db (type name))

;;;; Content

(defgeneric __add (db entity))

(defgeneric __exists-p (db query type))

(defgeneric __get (db entity))

(defgeneric __increase-wf-count (db wf))

(defgeneric __remove (db entity))

(defgeneric __update (db entity))

(defgeneric __search (db query type))