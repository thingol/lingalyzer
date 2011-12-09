(in-package :org.kjerkreit.lingalyzer.storage)

;; storage api

;;; State

(defvar *db* nil)

;;; Toplevel db class

(defclass lingalyzer-db ()
  ((name  :accessor name
	  :initarg  :name
	  :initform "Unnamed")
   (state :reader   db-dirty-p
	  :writer   dirty-db
	  :initform nil)))

;;; Internal representation of entities

;;;; Agent: authors or scribes

(defstruct agent
  (name       "John Doe"  :type string)
  (bday       "000-00-00" :type string)
  (dday       "000-00-00" :type string)
  (authored   nil         :type list)
  (copied     nil         :type list))

;;;; Documents, an actual version of the text, of which multipe make up an mdoc

(defstruct doc
  (mdoc       nil         :type array)
  (copied-by  "key"       :type string)
  (word-count -1          :type integer)
  (file       "bogus"     :type string)
  (hash       nil         :type array))

;;; Meta documents, container for copies of a document

(defstruct mdoc
  (name       "A tale"    :type string)
  (author     "key"       :type string)
  (genre      "Unknoiwn"   :type string)
  (docs       nil         :type list)
  (hash       nil         :type array))

;;;; Word forms, all forms are stored separately

(defstruct word-form
  (form       ""          :type string)
  (count      0           :type integer))


;;; Public functions

;;;; INIT

(defun new-db (name &optional (type 'ht))
  "Create a new database of the desired type and return reference."

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

(defun open-db (name type)
  "Open a db. Complain if one is already open."
  
  (when *db*
    (__close-db *db*))

  )

;;;; Content

(defun add (entity)
  "Add an entity to the database."

  (__add *db* entity))

(defun get (entity (&optional type nil))
  "Get an entity from the database."

  (__get *db* entity type))

(defun get-orphans ()
  "Return all orphaned entities"

  (when (db-dirty-p *db*)
    (gc-db))
  )

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

(defgeneric __open-db (db))

;;;; Content

(defgeneric __add (db entity))

(defgeneric __get (db entity))

(defgeneric __increment-wf-count (db wf))

(defgeneric __remove (db entity))

(defgeneric __update (db entity))

(defgeneric __search (db query entity-type))