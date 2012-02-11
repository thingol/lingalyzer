(in-package :org.kjerkreit.lingalyzer.store)

#-la-full-speed (declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
#+la-full-speed (declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))

;;;; INIT

(defun new-store (name)
  "Create a new store."

  (setf *default-store* (make-instance '
  (if (probe-file name)
      (error "Store appears to already exist.")
      (ensure-directories-exist name :mode 555)))

;;;; Store

(defun close-store ()
  "Closes the store."

  (__close-store *default-store*))

(defun open-store (name)
  "Open a store. Close the currently open store (if one exists).")


;;;; Store: content - general

(defun add-ent (entity)
  "Add an entity to the store."

  (__add-enttity *default-store* entity))

(defun remove-ent (type key)
  "Remove an entity from the store.")


;;;; DB: content - general

(defun exists-p (type key)
  "Synonym for (get-ent ...)"

  (get-ent type key))

(defun get-all-ent ()
  "Get all entities in the store.")

(defun get-ent (type key)
  "Get matching entity of specified type from the store.")

(defun get-ent-by (type attribyte value)
  "Get all entities mathing value of specified attribute.")

(defun update-ent (entity)
  "Update an entity in the store.")

;;;; Index

(defun find-entities (query &optional (type nil) (threshold 0.65))
  "Search the index for a matching key.")

(defun indexed-p (type key)
  "Is the key in the index?")
