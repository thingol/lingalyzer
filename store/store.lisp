(in-package :org.kjerkreit.lingalyzer.store)

(defclass store ()
  ((name    :initarg    name)
   (version :reader     version
	    :initform   1
	    :type       fixnum
	    :allocation :class)
   (dirty   :initform   nil
	    :type       symbol)
   (db      :type       sqlite-handle)
   (index   :type       index))
  (:documentation "Lingalyzer store. Uses SQLite as the metadata database, and flat files with sexps
  as the index."))

(defclass index ()
  ((forward :type          forward-index)
   (inverse :type          inverse-index)
   (mapping :type          cons
	    :documentation "Bi-gram to word-form map.")
  (:documentation "Lingalyzer index."))

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
