(in-package :org.kjerkreit.lingalyzer.storage)

(defstruct sql 
)

;;; agent agent-name
;;; wf              
;;; mdoc  mdoc-hash  (author-name  + mdoc-name)
;;; doc   doc-hash   (copyist-name + mdoc-name)
;;;

;; structs for entity types

(def-view-class agent ()
  ((name
    :db-kind        :key
    :db-constraints :not-null
    :reader         name
    :type           (string 40)
    :initarg        :name)
   (born
    :accessor       born
    :type           date
    :initarg        :bday)
   (died
    :accessor       died
    :type           date
    :initarg        :dday)
   (authored
    :db-kind        :join
    :db-info        (:join-class  mdoc
	             :home-key    name
	             :foreign-key author
	             :set t)
    :reader         authored)
   (copied
    :db-kind        :join
    :db-info        (:join-class  doc
	             :home-key    name
	             :foreign-key copyist
	             :set t)
    :reader          copied)))

;;; documents, an actual version of the text, of which multipe make up an mdoc
(def-view-class doc ()
  ((id
    :db-kind        :key
    :db-constraints :not-null
    :accessor       id
    :type           (string 16)
    :initarg        :id)
   (mdoc
    :db-constraints :not-null
    :accessor       mdoc
    :type           (string 16)
    :initarg        :mdoc)))

;;; meta documents, container for copies of a document
(def-view-class mdoc ()
  ((id
    :db-kind        :key
    :db-constraints :not-null
    :accessor       id
    :type           (string 16)    
    :initarg        :id)
   (name
    :db-constraints :not-null
    :accessor       name
    :type           (string 100)
    :initarg        :name)
   (author
    :db-constraints :not-null
    :accessor       author
    :type           (string 40)
    :initarg        :author)
   (genre
    :accessor       genre
    :type           (string 20)
    :initarg        :genre
    :initform       "N/A")
   (written-by
    :db-kind        :join
    :db-info        (:join-class  agent
	             :home-key    author
	             :foreign-key name
	             :set         t)
    :reader         written-by)
   (docs
    :db-kind        :join
    :db-info        (:join-class doc
	             :home-key    id
	             :foreign-key mdoc
	             :set         t)
    :reader         docs)))

;;; word forms
(def-view-class word-form ()
  ((form
    :db-kind        :key
    :db-constraints :not-null
    :accessor       form
    :type           (string 20)
    :initarg        :form)
   (occurences
    :accessor       occurences
    :type           :integer
    :iniform        1)))

;;; authors or copyists

;; low level db operations
(defmethod add-file ((db ht) fn)
  )

(defmethod close-db ((db ht))
  (drop-db db))

(defmethod drop-db ((db ht))
  (setf db nil))

;; high level db operations
(defmethod add ((db clsql) (entity agent))
  )

(defmethod add ((db clsql) (entity doc))
  )

(defmethod add ((db clsql) (entity mdoc))
  )

(defmethod add ((db ht) (entity word-form))
  )


(defmethod get ((db ht) (entity agent))
  )

(defmethod get ((db ht) (entity doc))
  )

(defmethod get ((db ht) (entity mdoc))
  )

(defmethod get ((db ht) (entity word-form))
  )


(defmethod remove ((db ht) (entity agent))
  )

(defmethod remove ((db ht) (entity doc))
  )

(defmethod remove ((db ht) (entity mdoc))
  )

(defmethod remove ((db ht) (entity word-form))
  )


(defmethod update ((db ht) (entity agent))
  )

(defmethod update ((db ht) (entity doc))
  )

(defmethod update ((db ht) (entity mdoc))
  )

(defmethod update ((db ht) (entity word-form))
  )


(defmethod search ((db ht) (entity agent))
  )

(defmethod search ((db ht) (entity doc))
  )

(defmethod search ((db ht) (entity mdoc))
  )

(defmethod search ((db ht) (entity word-form))
  )
