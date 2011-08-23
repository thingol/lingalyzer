(in-package :org.kjerkreit.lingalyzer.utils)

;;
;; +agents+: authors (politicians, scholars, etc.) and copyists (monks,
;;           scholars etc.)
;; +docs+:   documents with relevant information including an array containing
;;           the different versions addeded to the collection
;; +word-forms+:  all word forms in collection; also ngrams for fuzzy matching and
;;           counter for frequency
;;
;; +*-index+: adjustable arrays in the form of '(<ngram> . key) for fast
;; searching using fuzzy matching

;; TODO
;; remove doc/file
;; list all docs
;;              - alphabetically 
;;              - according to length
;;              - number of versions
;; list all authors
;;              - alphabetically
;;              - number of docs
;; list all copyists
;;              - alphabetically
;;              - number of docs
;; list all word forms
;;              - alphabeticall
;;              - number of docs
;;              - number of occurences
;; output statistics
;;              - total
;;                     - docs
;;                     - authors
;;                     - copyist
;;                     - word forms
;;              - longest do
;;              - most frequent word form
;;              - most versions
;;              - most prolific
;;                             - author
;;                             - copyist
;;


(defun init-store (&optional (type 'memory))
  "Set up the required datastructures.
Let user chose whether to use an ngram based index.
TODO: Let user chose type of store, e.g. memory, sql (cl-sql), git."

  (when type
    (print 'shut-up-about-that-unused-variable))
  
  (defconstant +agents+          (make-hash-table :test 'equal))
  (defconstant +docs+            (make-hash-table :test 'equal))
  (defconstant +word-forms+      (make-hash-table :test 'equal))
  (defconstant +agent-index+     (make-array 50   :element-type 'cons :adjustable t :fill-pointer 0))
  (defconstant +doc-index+       (make-array 150  :element-type 'cons :adjustable t :fill-pointer 0))
  (defconstant +word-form-index+ (make-array 5000 :element-type 'cons :adjustable t :fill-pointer 0)))
  

(init-store)

;; low-level
(defun read-file (path)
  "Read file from disk. Returns list of strings."

  (labels ((file-string (path)
	     "Sucks up an entire file from PATH into a freshly-allocated string, returning
two values: the string and the number of bytes read. (shamelessly stolen
from Cliki)"
	     (with-open-file (s path)
	       (let* ((len (file-length s))
		      (data (make-string len)))
		 (values data (read-sequence data s)))))
	   (good-char-p (char &optional (newline nil))
	     "Is it an alphanumeric, a space or possibly a newline?"

	     (or (alpha-char-p char)
		 (char-equal char #\Space)
		 (when newline (char-equal #\Newline))))
	   (strip-text (text)
	     "Strip text of all unwanted characters and convert to lowercase."

	     (let ((stripped-string))
	       (loop for char across text
		    when (good-char-p char)
		    collect (char-downcase char) into good-chars
		    finally (setf stripped-string (coerce good-chars 'string)))
	       stripped-string)))
	   
  
  (split-sequence #\Space (strip-text (file-string path)) :remove-empty-subseqs t)))

(defun read-metadata (path)
  "Read supplied metadata from file and return as alist."

    (with-open-file (metadata-file (concatenate 'string path ".meta") :direction :input)
      (read metadata-file)))

(defun gen-doc-hash (author name)
  "Make an md5sum from meta data. Useful in case two documents have the same name."

  (md5sum-string (concatenate 'string author name)))

;; First pass: check all word-forms, add if new
(defun process-word-forms (word-forms table)
  "Process word forms. Add if not already in table, otherwise increase count by one.
Returns a list of references to the word-forms processed in the orderd received."

  (flet ((get-word-form (word-form)
	   (let ((wf (gethash word-form table)))
	     (cond (wf (incf (word-form-count wf)) wf)
		   (t (setf (gethash word-form table)
			    (make-word-form :form word-form
				       :ngrams (gen-n-grams word-form)
				       :count 1)))))))

    (do ((wfs
	  (cdr word-forms)
	  (cdr wfs))
	 (processed-word-forms
	  (list (get-word-form (car word-forms)))
	  (cons (get-word-form (car wfs) processed-word-forms))))
	 ((not wfs) (reverse processed-word-forms)))))
  
(defun get-object (name table)
  "Get the reference to the agent in question. Make it if we don't already have
 them."

  (let ((object (gethash name table)))
    (if object
	object
	(setf (gethash name table) (make-agent :name name)))))


(defun process-doc (path tables)
  "Add document and relevant meta data to document db."

  (let* ((md        (read-metadata path))
	 (at        (first tables))
	 (dt        (second tables))
         (wft       (third tables))
	 (an        (cdr (assoc 'author md)))
	 (dn        (cdr (assoc 'name md)))
	 (cn        (cdr (assoc 'copied-by md)))
	 (dh        (gen-doc-hash an dn))
	 (author    (gethash an at))
	 (doc       (gethash dh dt))
	 (copied-by (gethash cn at))
	 (p-wfs     (process-word-forms path wft))
	 (add-doc-version
	  #'(lambda (v)
	      (make-doc-version
	       :version v
	       :copied-by (gethash (cdr (assoc 'copied-by md)) at)
	       :word-count (fillpointer p-wfs)
	       :org-file path
	       :org-file-hash (md5sum-file path)
	       :word-forms p-wfs)))
	      
	 (new-doc
	  #'(lambda ()
	      (let* ((nd (setf doc
			       (setf (gethash dh dt)
				     (make-doc
				      :name   dn
				      :ngrams (gen-n-grams dn)
				      :author author
				      :genre  (cdr (assoc 'genre metadata)))))

    (while (not doc)
      (new-doc (cdr (assoc 'name md))))
    (while (not author)
	 
	 

    (let* ((dv (doc-versions (gethash dh dt)))
	   (fp (fillpointer dv)))
      (setf (aref dv fp) (add-doc-version fp)))))

