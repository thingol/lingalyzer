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

;; low-level
(defun good-char-p (char)
  "Is it an alphanumeric or a space?"

  (or (alpha-char-p char)
      (char-equal char #\Space)))

(defun format-string (string
		      &optional (filter #'good-char-p) (delim #\Space) (remove-empty-subseqs t))
  "Strip text of all unwanted characters, convert to lowercase and tokenize."

  (let ((stripped-string))
    (loop for char across text
       when (filter char)
       collect (char-downcase char) into good-chars
       finally (setf stripped-string (coerce good-chars 'string)))
    (split-sequence delim stripped-string :remove-empty-subseqs remove-empty-subseqs)))


(defun read-file (path)
  "Read file from disk. Returns list of strings."

  (flet ((file-string (path)
	   "Sucks up an entire file from PATH into a freshly-allocated string, returning
two values: the string and the number of bytes read. (shamelessly stolen from Cliki)"
	   
	   (with-open-file (s path)
	     (let* ((len (file-length s))
		    (data (make-string len)))
	       (values data (read-sequence data s))))))
    (format-string (file-string path))))

(defun read-metadata (path)
  "Read supplied metadata from file. The format is currently an sexp denoting an alist."

  (with-open-file (metadata-file (concatenate 'string path ".meta") :direction :input)
    (read metadata-file)))

(defun process-word-forms (word-forms db)
  "Process word forms. Add if not already in table, otherwise increase count by one.
Returns a list of references to the word-forms processed in the orderd received."

  (let* ((wft (car db))
	 (wfi (cdr db))
	 (get-word-form
	  #'(lambda (word-form)
	      (let ((wf (gethash word-form wft)))
		(if wf
		    (incf (word-form-count wf))
		    (progn
		      (vector-push-extend (cons (gen-n-grams word-form) word-form) wfi)
		      (setf wf
			    (setf (gethash word-form wft)
				  (make-word-form
				   :form word-form
				   :count 1)))))
		wf))))

    (do ((wfs
	  (cdr word-forms)
	  (cdr wfs))
	 (processed-word-forms
	  (list (funcall get-word-form (car word-forms)))
	  (cons (funcall get-word-form (car wfs)) processed-word-forms)))
	((not wfs) (reverse processed-word-forms)))))
  
(defun process-doc (path)
  "Add document and relevant meta data to document db."
  
  (let* ((metadata             (read-metadata path))
	 (meta-doc-name        (cdr (car    metadata)))
	 (author-name          (cdr (cadr   metadata)))
	 (copyist-name         (cdr (cadddr metadata)))
	 (meta-doc-hash        (md5sum-string (concatenate 'string author-name  meta-doc-name)))
	 (doc-hash             (md5sum-string (concatenate 'string copyist-name meta-doc-name)))
	 (author               (gethash author-name   agent-table))
	 (meta-doc             (gethash meta-doc-hash meta-doc-table))
	 (copied-by            (gethash copyist-name  agent-table))
	 (processed-word-forms (process-word-forms (read-file path) (cons word-form-table word-form-index)))
	 
    (when (not meta-doc)
      (funcall new-meta-doc))

    (if (not author)
	(funcall new-author)
	(and
	 (setf (meta-doc-author meta-doc) author-name)
	 (setf (agent-authored author) (cons meta-doc-hash (agent-authored author)))))

    (when (not copied-by)
      (funcall new-copyist))

    (funcall new-doc)

    (setf (agent-copied copied-by) (cons doc-hash (agent-copied copied-by)))
    

    t))