(in-package :org.kjerkreit.lingalyzer.store)

;;;; Store

(defgeneric __close-store (store))

(defgeneric __open-store (type name))

;;;; Store: content - general

(defgeneric __add-entity (store entity))

(defgeneric __remove-entity (store type key))

;;;; DB: content - general 

(defgeneric __get-one (db type key))

(defgeneric __get-all (db))

(defgeneric __get-by (db type slot value))

(defgeneric __update (db entity))

;;;; DB: content - specific

(defgeneric __increase-wf-count (db wf delta))

;;;; Index

(defgeneric __indexed-p (index type key))


#| This would require a way of converting from one format to the other. In itself this is not a big
   issue, but it does require a third (and known) format everyone can convert to and from. We'll
   see.
(defgeneric __sync-with-db (index db))
|#

(defgeneric __find-entities (index type key threshold))
