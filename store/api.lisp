(in-package :org.kjerkreit.lingalyzer.store)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))

;;;; INIT

(defun new-store (name)
  "Create a new store."

  (if (probe-file name)
      (error "Store appears to already exist.")
      (ensure-directories-exist name :mode 555)))

;;;; Store

(defmethod close-store ()
  "Closes the store.")

(defmethod open-store (name)
  "Open a store. Close the currently open store (if one exists).")


;;;; Store: content - general

(defmethod add-ent (entity)
  "Add an entity to the store.")

(defmethod remove-ent (type key)
  "Remove an entity from the store.")


;;;; DB: content - general

(defmethod exists-p (type key)
  "Synonym for (get-ent ...)")

(defmethod get-all-ent ()
  "Get all entities in the store.")

(defmethod get-ent (type key)
  "Get matching entity of specified type from the store.")

(defmethod get-ent-by (type attribyte value)
  "Get all entities mathing value of specified attribute.")

(defmethod update-ent (entity)
  "Update an entity in the store.")

;;;; Index

(defmethod find-entities (query &optional (type nil) (threshold 0.65))
  "Search the index for a matching key.")

(defmethod indexed-p (type key)
  "Is the key in the index?")
