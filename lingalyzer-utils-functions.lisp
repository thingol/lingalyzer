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
  
  (list
   (list
    (make-hash-table :test 'equal)
    (make-hash-table :test 'equal)
    (make-hash-table :test 'equal))
   (list
    (make-array 200  :element-type 'cons :adjustable t :fill-pointer 0)
    (make-array 150  :element-type 'cons :adjustable t :fill-pointer 0)
    (make-array 5000 :element-type 'cons :adjustable t :fill-pointer 0))))
 

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

(defun process-word-forms (word-forms db)
  "Process word forms. Add if not already in table, otherwise increase count by one.
Returns a list of references to the word-forms processed in the orderd received."

  (let* ((wft (first db))
	 (wfi (second db))
	 (get-word-form #'(lambda (word-form)
			    (let ((wf (gethash word-form wft)))
			      (if wf
				  (incf (word-form-count wf))
				  (progn
				    (vector-push-extend (cons (gen-n-grams word-form) word-form) wfi)
				    (setf wf
					  (setf (gethash word-form wft)
						(make-word-form
						 :form word-form
						 :ngrams (gen-n-grams word-form)
						 :count 1)))))
			      wf))))

    (do ((wfs
	  (cdr word-forms)
	  (cdr wfs))
	 (processed-word-forms
	  (list (funcall get-word-form (car word-forms)))
	  (cons (funcall get-word-form (car wfs)) processed-word-forms)))
	((not wfs) (reverse processed-word-forms)))))
  
(defun process-doc (path db)
  "Add document and relevant meta data to document db."

  (let* ((metadata             (read-metadata path))
	 (agent-table          (first  (first  db)))
	 (agent-index          (second (first  db)))
	 (meta-doc-table       (first  (second db)))
	 (meta-doc-index       (second (second db)))
	 (word-form-table      (first  (third  db)))
	 (word-form-index      (second (third  db)))
	 (author-name          (cdr (assoc 'author    metadata)))
	 (meta-doc-name        (cdr (assoc 'name      metadata)))
	 (copyist-name         (cdr (assoc 'copied-by metadata)))
	 (meta-doc-hash        (md5sum-string (concatenate 'string author-name meta-doc-name)))
	 (author               (gethash author-name   agent-table))
	 (meta-doc             (gethash meta-doc-hash meta-doc-table))
	 (copied-by            (gethash copyist-name  agent-table))
	 (processed-word-forms (process-word-forms path (cons word-form-table word-form-index)))
	 (doc)
	 (new-doc
	  #'(lambda ()
	      (setf doc
		    (setf (meta-doc-docs meta-doc)
			  (cons
			   (make-doc
			    :meta-doc      meta-doc
			    :copied-by     copied-by
			    :word-count    (list-length processed-word-forms)
			    :org-file      path
			    :org-file-hash (md5sum-file path)
			    :word-forms    processed-word-forms)
			   (meta-doc-docs meta-doc))))))
	 (new-author
	  #'(lambda ()
	      (setf (meta-doc-author meta-doc)
		    (setf (gethash author-name agent-table)
			  (make-agent
			   :name     author-name
			   :authored (list meta-doc))))
	      (vector-push-extend (cons (gen-n-grams author-name) author-name) agent-index)))
	 (new-copyist
	  #'(lambda ()
	      (setf copied-by
		    (setf (gethash copyist-name agent-table)
			  (make-agent
			   :name copyist-name)))
	      (vector-push-extend (cons (gen-n-grams copyist-name) copyist-name) agent-index)))
	 (new-meta-doc
	  #'(lambda ()
	      (setf doc
		    (setf (gethash meta-doc-hash meta-doc-table)
			  (make-meta-doc
			   :name   meta-doc-name
			   :genre  (cdr (assoc 'genre metadata)))))
	      (vector-push-extend (cons (gen-n-grams meta-doc-name) meta-doc-hash) meta-doc-index))))

	 (when (not meta-doc)
	   (funcall new-meta-doc))

	 (if (not author)
	     (funcall new-author)
	     (and
	      (setf (meta-doc-author meta-doc) author)
	      (setf (agent-authored author) (cons meta-doc (agent-authored author)))))

	 (when (not copied-by)
	   (funcall new-copyist))

	 (funcall new-doc)

	 (setf (agent-copied copied-by) (cons doc (agent-copied copied-by)))
    

    t))



