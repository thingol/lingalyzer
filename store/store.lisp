(in-package :org.kjerkreit.lingalyzer.store)

;; to be read from config file, for the time being this will default to whatever directory the
;; software is run from, will handled properly in the future
(defconst +store-root+ nil)

(defvar *store-name* nil)
(defvar *store-components* '("metadata.db" "/index/" "/index/forward/"))


(define-condition store-integrity-error (error)
  ((store-name :initarg :store-name :reader store-name)
   (store-component :initarg :store-component :reader store-component))
  (:report (lambda (condition stream)
               (format stream "~A fails integrity test: ~A."
                       (store-name condition)
                       (store-component condition)))))

(defun check-store (&optional (store-root +store-root+)
                    (store-name *store-name*)
                    (store-components *store-components*))
  "Checks if store named <store-name> exists and is complete. Assumes that the store root is valid,
i.e. it has been checked somewhere else"

  (let ((store (merge-strings store-root store-name)))

    (if (not (probe-file store))
        (error 'store-integrity-error
               :store-name store-name
               :err-msg "store directory."))

    (dolist (el store-components)
      (if (not (probe-file (merge-pathnames store el)))
          (error 'store-integrity-error
                 :store-name store-name
                 :component el))))
  
  t)

(defun create-store (store-name)
  "Creates a new store. Returns t if succsessful and nil if the directory already exists."
      
  (and
   (not (probe-file store-name))
   (ensure-directories-exist
    (merge-pathnames store-name "/index/forward/")
    :mode 755)
   (init-db store-name)
   (setf *store-name* store-name)))

(defun close-store ()
  "Closes the currently open store."

  (cond ((not *store-name*)
	 nil)
	(t
	 (setf *store-name* nil))))

(defun delete-store (&optional (store-name *store-name*))
  "Deletes the named store, or the currently open store if called without arguments."

  (cond ((not store-name)
	 nil)
	((file-exists-p
	  (merge-pathnames store-name "/db.sqlite"))
	 (delete-directory-and-files store-name) ;; no return value
	 t)
	(t nil)))

(defun open-store (store-name)
  "Open a store. Close the currently open store (if one exists)."

  (cond ((file-exists-p (merge-pathnames store-name "/db.sqlite"))
         (if *store-name*
	    (close-store))
	(setf *store-name* store-name))
      'not-a-store))