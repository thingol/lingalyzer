(in-package :org.kjerkreit.lingalyzer.store)

;; TODO DB

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))


;;;; DB init
(defmethod initialize-instance :after ((o db) &key)
  (with-slots (name) o

    ;; Create and open db file
    (with-open-database (db name)

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
        count INTEGER NOT NULL)"))))


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
    (list table columns values)))

(defun build-insert-q (arg-list)
  "Uses a decoded record to build an insert query."

  (concatenate 'string
	       "INSERT INTO " (pop arg-list)
	       " (" (pop arg-list) ") "
	       "values (" (pop arg-list) ")"))



;; DB is opened and closed for every query, so implementing this is a bit hard...

(defmethod __close-store ((store sqlite-db))
  t)

;;;; Store: content - general

(defmethod __add-entity ((store sqlite-db) (entity list))
  (with-slots (db-path)
      (with-open-database (db db-path)
	(execute-non-query db
			   (build-insert-q (decode-record-spec entity))))))

(defmethod __remove-entity ((store sqlite-db) (type string) (string key))
  (with-slots (db-handle)
      (execute-non-query
       db-handle
       (concatenate 'string
		    "DELETE FROM " type
		    " WHERE KEY = " key))))
  

;;;; DB: content - general

(defmethod __get-one ((db sqlite-db) (type string) (key string))
  (with-slots (db-handle)
      (car (execute-to-list
	    db-handle
	    (concatenate 'string
			 "SELECT * FROM " type
			 " WHERE key = " key)))))

(defmethod __get-all ((db sqlite-db))
  (list
   (execute-to-list "SELECT * FROM agent")
   (execute-to-list "SELECT * FROM doc")
   (execute-to-list "SELECT * FROM mdoc")
   (execute-to-list "SELECT * FROM word_form")))

(defmethod __get-by ((db sqlite-db) (type string) (slot string) value)
  (with-slots (db-handle)
      (execute-to-list
       db-handle
       (concatenate 'string
		    "SELECT * FROM " type
		    " WHERE " slot
		    " = " value)))) ;; arg decoder needed to deal with things other than strings

(defmethod __update ((db sqlite-db) entity))


;;;; DB: content - specific

(defmethod __increase-wf-count ((db sqlite-db) (form string) (delta fixnum))
  (with-slots (db-handle)
      (let ((cur
	     (parse-integer
	      (execute-single
	       db-handle "SELECT count FROM word_form WHERE form = ?" form))))
	(execute-non-query
	 db-handle
	 (concatenate 'string
		      "UPDATE word_form SET count = " (write-to-string (+ delta cur))
		      " WHERE form = " form)))))