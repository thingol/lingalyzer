(in-package :org.kjerkreit.lingalyzer.store)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))


;;;; DB init

(defun init-db (store-name)
  "Create the database and apply the schema."

  ;; Create and open db file
  (with-open-database (db (merge-pathnames store-name "/db.sqlite"))
      
    ;; Apply schema
    (execute-non-query
     db
     "CREATE TABLE agent
      (id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
      name TEXT NOT NULL)")

    (execute-non-query
     db
     "CREATE TABLE doc
      (id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
      mdoc INTEGER NOT NULL,
      scribe INTEGER NOT NULL,
      len INTEGER NOT NULL,
      FOREIGN KEY(mdoc,scribe) REFERENCES mdoc(id) agent(id))
                               ON DELETE CASCADE
                               ON UPDATE CASCADE")

    (execute-non-query
     db
     "CREATE TABLE mdoc
      (id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
      name TEXT NOT NULL,
      author INTEGER NOT NULL,
      genre TEXT,
      FOREIGN KEY(author) REFERENCES agent(id))
                          ON DELETE CASCADE
                          ON UPDATE CASCADE)")

    (execute-non-query
     db
     "CREATE TABLE word_form
      (id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
      form TEXT NOT NULL,
      count INTEGER NOT NULL)")))

;;;; Auxilliary functions

(defun decode-record-spec (record-spec)
  "Converts the record spec into a list of arguments for the query builder."
  
  (let* ((table (symbol-name (pop record-spec)))
	 (columns (symbol-name (pop record-spec)))
	 (values (pop record-spec)))
    (do ((args record-spec))
	((not args))
      (setf columns (concatenate 'string columns "," (symbol-name (pop args))))
      (setf values (concatenate 'string values "," (pop args))))
    (values table columns values)))

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
		 " values (" values ")")))

(defun make-select-query (record-spec)
  "Uses a decoded record to build a select query."

  (multiple-value-bind (table columns values) (decode-record-spec record-spec)
    (concatenate 'string
		 "SELECT * FROM " table
		 " WHERE " columns  "=" values)))

(defun make-update-query (record-spec)
  "Uses a decoded record to build an update query."

  (multiple-value-bind (table columns values) (decode-record-spec record-spec)
    (concatenate 'string
		 "UPDATE " table
		 " SET " (first columns) " = " (first values)
		 " WHERE " (second columns) " = " (second values))))

(defmacro db-enq (&body)
  `(with-open-database (db *db-path*)
    (execute-non-query
     db
     ,@body)))

(defmacro db-etl (&body)
  `(with-open-database (db *db-path*)
    (execute-to-list
     db
     ,@body)))


;;;; DB: content - general

(defun add-rec (record-spec)
  (db-enq (make-insert-query record-spec)))

(defun delete-rec (record-spec)
  "Removes record(s) from db, possibly triggering a cascading delete."
  
  (db-enq (make-delete-query record-spec)))

(defun exists-p (type key)
  "Synonym for (get-one ...)"

  (get-one type key))  

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
 
  (car (get-by record-spec)))

(defun update-ent (record-spec)
  "Updates a record, but only one attribute at a time."
  
  (db-enq (make-update-query record-spec)))


;;;; DB: content - specific

(defun update-wf-count (form delta)
  "Updates the word form count."
  
  (db-enq (make-update-query
	   (list :word_form
		 :count (concatenate 'string "count+" (write-to-string delta))
		 :key form))))