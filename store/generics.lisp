(in-package :org.kjerkreit.lingalyzer.store)

;;;; Store

(defgeneric __close-store (store))

(defgeneric __drop (store))

(defgeneric __gc (store rem-ent))

(defgeneric __open-store (type name))

;;;; Store: content - general

(defgeneric __add-entity (store entity))

(defgeneric __remove-entity (store type key))

;;;; DB: content - general 

(defgeneric __get-one (db key type))

(defgeneric __get-all (db))

(defgeneric __get-by (db type slot value))

(defgeneric __update (db entity))

;;;; DB: content - specific

(defgeneric __add-doc-to-mdoc (db mdhash dhash))

(defgeneric __add-doc-scribe (db scribe dhash))

(defgeneric __add-mdoc (db author mdhash))

(defgeneric __get-childless (db))

(defgeneric __increase-wf-count (db wf delta))

;;;; Index

(defgeneric __indexed-p (index type key))


#| This would require a way of converting from one format to the other. In itself this is not a big
   issue, but it does require a third (and known) format everyone can convert to and from. We'll
   see.
(defgeneric __sync-with-db (index db))
|#

(defgeneric __find-entities (index type key threshold))
