;(in-package :org.kjerkreit.lingalyzer.store)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))

(defvar *db-path* nil)

(define-condition db-integrity-error (error)
  ((store-name :initarg :store-name :reader store-name))
  (:report (lambda (condition stream)
               (format stream "Meta data db for store ~A fails integrity test."
                       (store-name condition)))))

;;;; DB init

(defun init-db (store-name)
  "Create the database and apply the schema."

  (setf *db-path* (concatenate 'string store-name "/db.sqlite"))

  
  ;; Create and open db file
  (with-open-database (db *db-path*)
      
    ;; Apply schema
    (execute-non-query
     db
     "CREATE TABLE agent
      (name TEXT PRIMARY KEY)")

    (execute-non-query
     db
     "CREATE TABLE doc
      (dhash TEXT PRIMARY KEY,
       mdoc TEXT REFERENCES mdoc(name) ON DELETE CASCADE,
       scribe TEXT REFERENCES agent(name) ON DELETE CASCADE,
       len INTEGER)")

    (execute-non-query
     db
     "CREATE TABLE mdoc
      (name TEXT PRIMARY KEY,
       author TEXT REFERENCES agent(name) ON DELETE CASCADE),
       genre TEXT")

    (execute-non-query
     db
     "CREATE TABLE word_form
      (form TEXT PRIMARY KEY,
       count INTEGER)")))

;;;; Auxilliary functions

(defun decode-record-spec (record-spec &optional (update nil))
  "Converts the record spec into a list of arguments for the query builder."
  (declare (dynamic-extent record-spec))

  (let ((table (symbol-name (pop record-spec))))
    (if update
	(let* ((columns (list (symbol-name (pop record-spec))))
	       (values (list (pop record-spec))))
	  (do ((args record-spec))
	      ((not args))
	    (setf columns (append (list (symbol-name (pop args))) columns))
	    (setf values (append (list (pop args)) values)))
	  (values table columns values))
	(let* ((columns (symbol-name (pop record-spec)))
	       (values (pop record-spec)))
	  (do ((args record-spec))
	      ((not args))
	    (setf columns (concatenate 'string columns "," (symbol-name (pop args))))
	    (setf values (concatenate 'string values "','" (pop args))))
	  (values table columns values)))))

(defun make-delete-query (record-spec)
  "Uses a decoded record to build a delete query."

  (multiple-value-bind (table columns values) (decode-record-spec record-spec)
    (concatenate 'string
		 "DELETE FROM " table
		 " WHERE " columns  "=" values)))
  
(defun make-insert-query (record-spec)
  "Uses a decoded record to build an insert query."

  (multiple-value-bind (table columns values) (decode-record-spec record-spec)
    (concatenate 'string
		 "INSERT INTO " table
		 " (" columns  ")"
		 " values ('" values "')")))

(defun make-select-query (record-spec)
  "Uses a decoded record to build a select query."

  (multiple-value-bind (table columns values) (decode-record-spec record-spec)
    (concatenate 'string
		 "SELECT * FROM " table
		 " WHERE " columns  "='" values "'")))

(defun make-update-query (record-spec)
  "Uses a decoded record to build an update query."

  (multiple-value-bind (table columns values) (decode-record-spec record-spec t)
    (concatenate 'string
		 "UPDATE " table
		 " SET " (first columns) " = " (first values)
		 " WHERE " (second columns) " = '" (second values) "'")))

(defmacro db-enq (&body body)
  "Executes query without returning data. Used for DELETE, INSERT, UPDATE etc."
  
  `(with-open-database (db *db-path*)
     (execute-non-query db "PRAGMA foreign_keys=1")
     (execute-non-query
      db
      ,@body)))

(defmacro db-etl (&body body)
  "Executes query and returns data as list."
  
  `(with-open-database (db *db-path*)
    (execute-to-list
     db
     ,@body)))


;;;; DB: content - general

(defun add-rec (record-spec)
  "Adds a record to the db."
  
  (db-enq (make-insert-query record-spec)))

(defun delete-rec (record-spec)
  "Removes record(s) from db, possibly triggering a cascading delete."
  
  (db-enq (make-delete-query record-spec)))

(defmacro exists-p (record-spec)
  "Synonym for (get-rec-one ...)"

  `(get-rec-one ,record-spec))

(defun get-rec-all ()
  "Retrieves everything we have."
  
  (list
   (db-etl "SELECT * FROM agent")
   (db-etl "SELECT * FROM doc")
   (db-etl "SELECT * FROM mdoc")
   (db-etl "SELECT * FROM word_form")))

(defun get-rec-by (record-spec)
  "Retrieves all rows matching the specified column and value from the specified table."
  
  (db-etl (make-select-query record-spec)))

(defun get-rec-one (record-spec)
  "Retrieves one row from the db and returns it as a list."
  (declare (inline get-rec-by))
 
  (car (get-rec-by record-spec)))

(defun update-rec (record-spec)
  "Updates a record, but only one attribute at a time."
  
  (db-enq (make-update-query record-spec)))

;;;; DB: content - specific

(defun update-wf-count (form delta)
  "Updates the word form count."
  
  (db-enq (make-update-query
	   (list :word_form
		 :form form
		 :count (concatenate 'string "count+" (write-to-string delta))))))
