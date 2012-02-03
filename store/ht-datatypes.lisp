(in-package :org.kjerkreit.lingalyzer.store)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))

(defun forward-index-p (obj)
  (and (typep (car obj) 'md5sum)
       (typep (cdr obj) 'cons)))

(deftype forward-index ()
  `(satisfies forward-index-p))

(defun inverse-index-p (obj)
  (and (typep (caadar obj) 'md5sum)
       (typep (car (cdadar obj)) 'array)))

(deftype inverse-index ()
  `(satisfies inverse-index-p))

(defclass lingalyzer-db ()
  ((version :reader     version
	    :initform   1
	    :type       fixnum
	    :allocation :class)
   (state   :accessor   dirty
	    :initform   nil
	    :type       symbol))
  (:documentation "Toplevel db class."))

(defclass lingalyzer-index ()
  ((version :reader     version
	    :initform   1
	    :type       fixnum
	    :allocation :class)
   (state   :accessor   dirty
	    :initform   nil
	    :type       symbol))
  (:documentation "Toplevel index class."))

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
   (len    :reader  len
	   :initarg :len
	   :type    fixnum)
   (key    :reader  hash
	   :initarg :hash
	   :type    md5sum))
  (:documentation "Documents, an actual version of the text, of which multipe make up an mdoc."))


(defclass doc-index (lingalyzer-entity)
  ((forward :reader  forward
	    :initarg :forward
	    :type    forward-index
	    :documentation "Document to word forms.")
   (inverse :reader  inverse
	    :initarg :inverse
	    :type    inverse-index
	    :documentation "Word forms to position in document"))
  (:documentation "An indexed document ready to be merge into the index proper."))

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
	   :type     (array md5sum))
   (hash   :reader   hash
	   :initarg  :hash
	   :type     md5sum))
  (:documentation "Meta documents, container for copies of a document."))

(defclass word-form (lingalyzer-entity)
  ((key         :reader   form
	        :initarg  :form
	        :type     string)
   (occurrences :accessor occurred
	        :initform 0
	        :type     fixnum))
  (:documentation "Word forms, all forms are stored separately."))
