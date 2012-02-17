(in-package :org.kjerkreit.lingalyzer.store)

(defparameter *store-name* nil)

(defun create-store (store-name)
  "Creates a new store. Returns t if succsessful and nil if not."
      
  (and
   (not (probe-file store-name))
   (ensure-directories-exist
    (merge-pathnames store-name "/forward-index/")
    :mode 755)
   (ensure-directories-exist
    (merge-pathnames store-name "/inverse-index/")
    :mode 755)
   (init-db store-name)
   (setf *store-name* store-name)))

(defun close-store ()
  "Closes the currently open store."

  (cond ((not *store-name*)
	 'no-open-store)
	(t
	 (setf *store-name* nil)
	 t)))

(defun delete-store (&optional (store-name *store-name*))
  "Deletes the named store, or the currently open store if called without arguments."

  (cond ((not store-name)
	 'no-store-to-delete)
	((file-exists-p
	  (merge-pathnames store-name "/db.sqlite"))
	 (delete-directory-and-files store-name) ;; no return value
	 t)
	(t 'not-a-store)))

(defun open-store (store-name)
  "Open a store. Close the currently open store (if one exists)."

  (if (file-exists-p (merge-pathnames store-name "/db.sqlite"))
      (progn
	(when *store-name*
	  (close-store))
	(setf *store-name* store-name))
      'not-a-store))
