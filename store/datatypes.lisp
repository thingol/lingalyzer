(in-package :org.kjerkreit.lingalyzer.store)

(defun indexed-doc-p (obj)
  "Does this object conform to my idea of an indexed document?"

  (and (type-of (car obj) 'fixnum)
       (type-of (caadr obj) 'md5sum)
       (eql (list-length (caddr obj)) 3)))

(deftype indexed-doc ()
  "Implemented as a SATISFIES type because I don't know any better :)"
  `(and list
       (satisfies indexed-doc-p)))
       

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
