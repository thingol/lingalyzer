(in-package :org.kjerkreit.lingalyzer.utils)

;;
;; +agents+: authors (politicians, scholars, etc.) and copyists (monks,
;;           scholars etc.)
;; +docs+:   documents with relevant information including an array containing
;;           the different versions addeded to the collection
;; +terms+:  all word forms in collection; also ngrams for fuzzy matching and
;;           counter for frequency
;;
;; +*-index+: adjustable arrays in the form of '(<ngram> . key) for fast
;; searching using fuzzy matching



(defun init-store (&optional (type 'memory) (ngram-index t))
  "Set up the required datastructures.
Let user chose whether to use an ngram based index.
TODO: Let user chose type of store, e.g. memory, sql (cl-sql), git."

  (when type
    (print 'shut-up-about-that-unused-variable))
  
  (defconstant +agents+ (make-hash-table :test 'equal))
  (defconstant +docs+ (make-hash-table :test 'equal))
  (defconstant +terms+ (make-hash-table :test 'equal))


  (when ngram-index
    ;; highly uneducated guesses
    (defconstant +agent-index+ (make-array
				50
				:element-type 'cons
				:adjustable t
				:fill-pointer 0))
    (defconstant +doc-index+ (make-array
			      150
			      :element-type 'cons
			      :adjustable t
			      :fill-pointer 0))
    (defconstant +term-index+ (make-array
			       5000
			       :element-type 'cons
			       :adjustable t
			       :fill-pointer 0))))
  

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
	   
  
  (split-sequence:split-sequence #\Space (strip-text (file-string path)) :remove-empty-subseqs t)))

(defun read-metadata (path)
  "Read supplied metadata from file and return as alist."

    (with-open-file (metadata-file (concatenate 'string path ".meta") :direction :input)
      (read metadata-file)))

(defun gen-doc-hash (md)
  "Make an md5sum from meta data. Useful in case two documents have the same name."

  (sb-md5:md5sum-string (concatenate 'string (cdr (assoc 'author md)) (cdr (assoc 'name md)))))

(defun has-doc-p (doc-hash)
  "Compare meta data from two files to determine document equality."

  (gethash doc-hash +docs+))
  

;; First pass: check all terms, add if new
(defun process-terms (terms)
  "Process terms. Add if not already in table, otherwise increase count by one.
Returns a list of references to the terms processed in the orderd received."

  (flet ((get-term (term)
	   (let ((th (gethash term +terms+)))
	     (cond (th (incf (term-count th)) th)
		   (t (setf (gethash term +terms+)
			    (make-term :form term
				       :ngrams (gen-n-grams term)
				       :count 1)))))))

    (do ((ts
	  (cdr terms)
	  (cdr ts))
	 (processed-terms
	  (list (get-term (car terms)))
	  (append processed-terms (list (get-term (car ts))))))
	 ((not ts) processed-terms))))
  
(defun get-agent (name)
  "Get the reference to the agent in question. Make it if we don't already have
 them."

  (let ((agent (gethash name +agents+)))
    (if agent
	agent
	(setf (gethash name +agents+) (make-agent :name name)))))


(defun process-doc (pterms path)
  "Add document and relevant meta data to document db."

  (let* ((md (read-metadata path))
	 (dh (gen-doc-hash md))
	 (doc (has-doc-p dh))
	 (add-doc-version
	  #'(lambda (v)
	      (make-doc-version
	       :version v
	       :copied-by (get-agent (cdr (assoc 'copied-by md)))
	       :word-count (fillpointer pterms)
	       :org-file path
	       :org-file-hash (sb-md5:md5sum-file path)
	       :terms pterms)))
	      
	 (new-doc
	  #'(lambda (name)
	      (setf (gethash dh +docs+)
		    (make-doc
		     :name     name
		     :ngrams   (gen-n-grams name)
		     :author   (get-agent (cdr (assoc 'author metdata)))
		     :genre    (cdr (assoc 'genre metadata))
		     :versions (make-array 3
					   :element-type 'doc-version
					   :adjustable t
					   :fill-pointer 1))))))
				     
    (cond (doc (let* ((dv (doc-versions doc))
		      (fp (fillpointer dv)))
		 (setf (aref dv fp) (funcall #'add-doc-version fp))))
	  (t (funcall #'new-doc (cdr (assoc 'name md)))))))

;; exported functions
;;
;; manage documents
;;
(defun add-file (path)
  "Add a (new) file to db."

	(process-doc (process-terms (read-file path)) path))
;;
;; search
;;
(defun search-agents (query &optional (threshold 0.65))
  "Search the agents in the collection. Threshold is set to 65% similarity
  based on ngram comparison if indexing is in use."
  
  (search-index +agent-index+ query threshold))

(defun search-docs (query &optional (threshold 0.65))
  "Search the documents in the collection. Threshold is set to 65% similarity
  based on ngram comparison if indexing is in use."
  
  (search-index +doc-index+ query threshold))

(defun search-terms (query &optional (threshold 0.65))
  "Search the terms in the collection. Threshold is set to 65% similarity
  based on ngram comparison if indexing is in use."

  (let ((res))
    (dolist (key (search-index +term-index+ query threshold))
      (setf res (cons (gethash key +terms) res)))
    (setf res (sort res #'string< :key #'term-name))))
    

(defun search-index (index query threshold)
  "Check the document collection for documents matching the supplied name."

  (let ((matches)
	(query-ngrams (gen-n-grams query)))
    
    (maphash #'(lambda (k v)
		 (when (>= (compare-n-grams query-ngrams k) threshold)
		   (setf matches (cons v matches))))
	     index)
    
    matches))