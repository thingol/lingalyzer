(in-package :org.kjerkreit.lingalyzer.storage)

;; storage api


;;; State

(defparameter *db* nil)

;;; Public functions


;;;; DB

(defun close-db ()
  "Closes the db."
  
  (__close-db *db*))

(defun drop-db ()
  "Drops the currently open db."
  
  (__drop-db *db*))

(defun gc-db ()
  "Removes orphaned entities."

  (__gc-db *db*))

(defun new-db (name &optional (type 'ht))
  "Create a new database of the desired type and return reference."

  (setf *db* (make-instance type)))

(defun open-db (name type)
  "Open a db. Complain if one is already open."
  
  (when *db*
    (__close-db *db*))

  )



;;;; Content

(defun add (entity)
  "Add an entity to the database."

  (__add *db* entity))

(defun get (entity)
  "Get an entity from the database."

  (__get *db* entity))

(defun remove (entity)
  "Remove an entity from the database."

  (__remove *db* entity))

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

(defgeneric __gc-db (db))

(defgeneric __open-db (db))

;;;; Content

(defgeneric __add (db entity))

(defgeneric __get (db entity))

(defgeneric __remove (db entity))

(defgeneric __update (db entity))

(defgeneric __search (db query entity-type))