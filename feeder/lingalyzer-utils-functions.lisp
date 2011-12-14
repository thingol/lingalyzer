(in-package :org.kjerkreit.lingalyzer.utils)

;; TODO
;; list all docs
;;              - alphabetically 
;;              - according to length
;;              - number of versions
;; list all authors
;;              - alphabetically
;;              - number of docs
;; list all scribes
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
;;                     - scribes
;;                     - word forms
;;              - longest doc
;;              - most frequent word form
;;              - most versions
;;              - most prolific
;;                             - author
;;                             - scribe
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

(defun index-document (word-forms doc-hash)
  "Indexes a document. Adds word forms if new, otherwise increase count by one.
Returns a list of references to the word-forms processed in the orderd received."

  (let ((indexed-doc)
	(len 0))
    (dolist (wf word-forms)
      (incf len)
      (let ((wf-found (assoc wf indexed-doc :test #'string=)))
	(if wf-found
	    (push len (cdr wf-found))
	    (setf indexed-doc (append indexed-doc (list (list wf len)))))))
    (append (list (list doc-hash len word-forms)) indexed-doc)))
  
(defun process-doc (path)
  "Add document and relevant meta data to document db. Returns nil if document alrady exists and an
update has not been requested."

  (let* ((metadata   (read-metadata path))
	 (mdoc-name  (cdr (car      metadata)))
	 (scribe     (cdr (cadddr   metadata)))
	 (doc-hash   (md5sum-string (concatenate 'string scribe mdoc-name))))
    (if (exists-p doc-hash)
	nil
	(let* ((author     (cdr (cadr     metadata)))
	       (mdoc-hash  (md5sum-string (concatenate 'string author mdoc-name)))
	       (clean-text (read-file     path))
	       (doc-len    (list-length   clean-text)))

	  (unless (exists-p  author     'agent)
	    (add (make-agent :name      author
			     :authored  (list mdoc-hash))))
    
	  (unless (exists-p  mdoc-hash 'mdoc)
	    (add (make-mdoc  :name      mdoc-name
			     :author    author
			     :genre     (cdr (caddr metadata))
			     :docs      (list doc-hash)
			     :hash      mdoc-hash)))
    
	  (unless (exists-p  scribe     'agent)
	    (add (make-agent :name      scribe
			     :copied    (list doc-hash))))

	  (add   (make-doc   :mdoc      mdoc-hash
			     :scribe    scribe
			     :length    doc-len
			     :hash      doc-hash))))))