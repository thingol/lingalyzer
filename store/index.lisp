(in-package :org.kjerkreit.lingalyzer.store)

;; 0: forward       - hash table "docid0" => ("wf0" "wf1" ...)
;; 1: inverse       - hash table "word form" => '(("<docid>" . #(pos0 pos1)))
;; 2: agent         - array '#((("$J" "Ju" "ul" "li" "iu" "us" "s$") . "Julius"))
;; 3: mdoc          - array '#((("$D" "De" "e " " B" "Be" "el" "ll" "lo" "o " " G" "Ga" "al" "ll"
;;                               "li" "ic" "co" "o$") . "De Bello Gallico"))
;; 4: word-form     - array '#((("$i" "im" "mp" "pe" "er" "ri" "io" "o$") . "imperio"))
;; 4+n: other types e.g. k-gram.

;; simple vector, loaded from disk. Merged with new index upon request or upon closing of store
(defvar *index* (make-array '(5) :element-type t))

(defvar *index-path* 'nil)

;; for things we have indexed during the current session
(defvar *temp-index* (make-array '(100) :element-type t :adjustable t :fill-pointer t))

;;; support
(defmacro load-index-from-file (path &body body)
  `(with-open-file (file ,path
		    :direction :input)
     ,@body))

(defmacro save-to-index-file (path &body body)
  `(with-open-file (file ,path
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
     ,@body))

(defmacro process-hash-table (table &body body)
  `(with-hash-table-iterator (generator-fn ,table)
     (loop     
	(multiple-value-bind (more? key value) (generator-fn)
	  (if (not more?) (return))
	  ,@body))))


;;; high-level functions

(defun load-index (&optional (store *store-name*) (index *index*))
  "Loads an index from either the currently open store (as part of the process of opening it), or
from a specified store."

  (setf (aref index 1) (load-forward store))
  'load-inverse
  'load-agent
  'load-mdoc
  'load-word-form
  )

(defun save-index (&optional (store *store-name*) (index *index*))
  "Saves the index to disk."

  (destructuring-bind (forward inverse agent mdoc word-form) index
    
    (save-forward store forward)
    (save-inverse store inverse)
    (save-fuzzy store 'agent agent)
    (save-fuzzy store 'mdoc mdoc)
    (save-fuzzy store 'word-form word-form)))

;;; low-level functions

;;;; misc
(defun merge-inverse (new-index &optional (index *index*))
  "Merges the newly indexed document with the existing index."

  (if (aref index 1)
      'merge
      (setf (aref index 1) new-index)))

;;;; loading
(defun load-forward (&optional (store *store-name*))
  "Loads the forward index from the specified store and returns it as a hash-table."

  (let ((index (make-hash-table :test 'equal :size 50)))
    (dolist (f (cl-fad:list-directory (merge-strings store "/index/forward/")))
      (load-index-from-file (f)
	(setf (gethash (pathname-name f) index) (read file nil nil))))

    index))

(defun load-inverse (&optional (store *store-name*))
  "Loads the inverse index from the specified store and returns it as a hash-table."

  (let ((index (make-hash-table :test 'equal :size 50)))
    (load-index-from-file (merge-strings store "/index/inverse")
      (loop for line = (read-line file nil nil)
	 until (eq line 'nil)
	 do (setf (gethash (car line) index) (cdr line))))

    index))

(defun load-fuzzy (&optional (store *store-name*) type)
  "Loads..."

  (load-index-from-file (merge-strings store "/index/fuzzy." (symbol-name type))
    (read file nil nil)))

;;;; saving
(defun save-forward (store index)
  "Saves the forward index as one file per document containing a list of the word forms as a
sepx. Files are named according to the following scheme:  <store>/index/forward/<dhash>"

  (process-hash-table
   index
   (save-to-index-file
    (merge-strings store "/index/forward/" (car key))
    (write value :stream file))))

(defun save-inverse (store index)
  "Saves the inverse index as sexps in a file named <store>/index/inverse"

  (save-to-index-file
   (merge-strings store "/index/inverse")
   (process-hash-table
    index
    (write `(,key . ,value) :stream file))))

(defun save-fuzzy (store index type)
  "Saves the inverse index as sexps in a file named <store>/index/fuzzy/<type>"
  
  (save-to-index-file
   (merge-strings store "/index/fuzzy." (symbol-name type))
   (write index :stream file)))

