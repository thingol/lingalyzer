(in-package :org.kjerkreit.lingalyzer.storage)

(defstruct sql 
)

;;; db-kind
;;;    constraints
;;;    info
;;; accessor/reader/writer
;;; type
;;; initarg
;;;

;;; agent agent-name
;;; wf              
;;; mdoc  mdoc-hash  (author-name  + mdoc-name)
;;; doc   doc-hash   (copyist-name + mdoc-name)
;;;


(def-view-class agent ()
  ((name
    :db-kind        :key
    :db-constraints :not-null
    :accessor       :name
    :type           (string 40)
    :initarg        :name)
   (bday
    :accessor       bday
    :type           date
    :initarg        :bday)
   (dday
    :accessor       dday
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
    :reader          copied)
   (:base-table     agent)))

(def-view-class word-form ()
  ((form
    :db-kind        :key
    :db-constraints :not-null
    :accessor       form
    :type           (string 20)
    :initarg        :form)
   (count
    :accessor       count
    :type           :integer
    :initarg        1)
   (:base-table     word-form)))

(def-view-class mdoc ()
  ((mdocid
    :db-kind        :key
    :db-constraints :not-null
    :accessor       mdocid
    :type           (string 16)    
    :initarg        :id)
   (name
    :db-constraints :not-null
    :accessor       mdoc-name
    :type           (string 100)
    :initarg        :name)
   (author
    :db-constraints :not-null
    :accessor       mdoc-author
    :type           (string 40)
    :initarg        :author)
   (genre
    :accessor       mdoc-genre
    :type           (string 20)
    :initarg        "N/A")
   (written-by
    :db-kind        :join
    :db-info        (:join-class  agent
	             :home-key    author
	             :foreign-key name
	             :set         t)
    :reader         written-by)
   (docs
    :db-kind        :join
    :db-info        (:joint-class doc
	             :home-key    mdocid
	             :foreign-key mdocid
	             :set         t)
    :reader         docs)
   (:base-table     mdoc)))

(def-view-class doc ()
  ((docid
    :db-type        :key
    :db-constraints :not-null
    :accessor       doc-id
    :type           (string 16)
    :initarg        :id)
   (mdocid
    :db-constraints :not-null
    :accessor       mdocid
    :type           (string 16)
    :initarg        :mdocid)
   (:base-table :doc)))