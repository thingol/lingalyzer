(in-package :org.kjerkreit.lingalyzer.store)

;; storage api

;;; State

(defvar *store* nil)

;;; Datatypes
(defclass lingalyzer-db ()
  ((version :reader     version
	    :initform   1
	    :type       fixnum
	    :allocation :class))
  (:documentation "Toplevel db class."))

(defclass lingalyzer-index ()
  ((version :reader     version
	    :initform   1
	    :type       fixnum
	    :allocation :class))
  (:documentation "Toplevel index class."))

(defclass lingalyzer-store ()
  ((name   :accessor name
	   :initarg  :name
	   :initform "Unnamed"
	   :type     'string)
   (db     :reader   db
	   :type     'lingalyzer-db)
   (index  :reader   index
	   :type     'lingalyzer-index)
   (dstate :reader   db-dirty-p
	   :writer   db-dirty
	   :initform nil)
   (istate :reader   index-dirty-p
	   :writer   index-dirty
	   :initform nil))
  (:documentation "Toplevel store class."))

(defclass lingalyzer-entity ()
  ((version :reader   version
	    :initform 1
	    :type     fixnum))
  (:documentation "Toplevel entity class."))

(defclass agent (lingalyzer-entity)
  ((key      :reader   name
	     :initarg  :name
	     :initform "John Doe"
	     :type     string)
   (bday     :reader   bday
	     :initarg  :bday
	     :initform "0000-00-00"
	     :type     string)
   (dday     :reader   dday
	     :initarg  :dday
	     :initform "0000-00-00"
	     :type     string))
  (:documentation "Agent: authors or scribes."))

(defclass doc (lingalyzer-entity)
  ((mdoc   :reader  mdoc
	   :initarg :mdoc
	   :type    md5sum)
   (scribe :reader  scribe
	   :initarg :scribe
	   :type    string)
   (length :reader  length
	   :initarg :length
	   :type    fixnum)
   (key    :reader  hash
	   :initarg :hash
	   :type    md5sum))
  (:documentation "Documents, an actual version of the text, of which multipe make up an mdoc."))

(defclass mdoc (lingalyzer-entity)
  ((key    :reader   name
	   :initarg  :name
	   :type     string)
   (author :reader   author
	   :initarg  :author
	   :type     string)
   (genre  :reader   genre
	   :initarg  :genre
	   :initform "-"
	   :type     string)
   (docs   :accessor docs
	   :initarg  :docs
	   :type     (array 'md5dum))
   (hash   :reader   hash
	   :initarg  :hash
	   :type     md5sum))
  (:documentation "Meta documents, container for copies of a document."))

(defclass word-form (lingalyzer-entity)
  ((key   :reader   form
	  :initarg  :form
	  :type     string)
   (count :reader   count
	  :initform 1
	  :type     fixnum))
  (:documentation "Word forms, all forms are stored separately."))


;;; Public functions

;;;; INIT

(defun new-store (name &optional (dtype 'ht) (itype 'ht))
  "Create a new database and index of the desired types."

  (setf *db*    (make-instance dtype))
  (setf *index* (make-instance itype)))

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

;;; Private functions

;;;; Store

(defgeneric __close (store))

(defgeneric __drop (store))

(defgeneric __gc (store rem-ent))

(defgeneric __open (dtype itype name))

;;;; Store: content - general

(defgeneric __add (store data))

(defgeneric __remove (store type key))

;;;; DB: content - general 

(defgeneric __get (db type key))

(defgeneric __get-all (db))

(defgeneric __get-by (db type slot value))

(defgeneric __update (db entity key))

;;;; DB: content - specific

(defgeneric __add-doc-to-mdoc (db mdhash dhash))

(defgeneric __add-doc-scribe (db scribe dhash))

(defgeneric __add-mdoc (db author mdhash))

(defgeneric __get-childless (db))

(defgeneric __increase-wf-count (db wf))

;;;; Index

(defgeneric __indexed-p (index type key))


#| This would require a way of converting from one format to the other. In itself this is not a big
   issue, but it does require a third (and known) format everyone can convert to and from. We'll
   see.
(defgeneric __sync-with-db (index db))
|#

(defgeneric __search (index type key))
