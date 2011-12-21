(in-package :org.kjerkreit.lingalyzer.utils)

;; TODO

;; low-level
(defun make-index-entry (wf dhash pos)
  "Make an index entry for a given word form."

  (list
   (list wf
	 dhash
	 (make-array 1 :element-type 'fixnum :initial-contents `(,pos) :adjustable t :fill-pointer 1))))

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

(defun index-document (word-forms dhash)
  "Indexes a document and returns a partial index.

The structure of the partial index:

((dhash length-of-doc (form0 form1 form2 form0 ...))
 (form0 dhash #(1 4))
 (form1 dhash #(2))
 (form2 dhash #(3)))"

  (let ((indexed-doc)
	(pos 0))
    (dolist (wf word-forms)
      (incf pos)
      (let ((wf-found (assoc wf indexed-doc :test #'string=)))
	(if wf-found
	    (vector-push-extend pos (caddr wf-found))
	    (setf indexed-doc (append indexed-doc (make-index-entry wf dhash pos))))))
    (append (list (list dhash len word-forms)) indexed-doc)))
  
(defun process-doc (path)
  "Add document and relevant meta data to document db. Returns nil if document alrady exists."

  (let* ((metadata  (read-metadata            path))
	 (mdoc-name (cdr (car                 metadata)))
	 (scribe    (cdr (cadddr              metadata)))
	 (dhash     (md5sum-strings-to-string scribe mdoc-name)))
    (if (exists-p doc-hash)
	nil
	(let* ((author      (cdr (cadr                metadata)))
	       (mdhash      (md5sum-strings-to-string author mdoc-name)))
	       (indexed-doc (read-file                path)))

	  (if (exists-p author 'agent)
	      (add-mdoc author mdhash)
	      (add (make-agent :name     author
			       :authored (make-array 1
						     :element-type     md5sum
						     :initial-contents mdhash
						     :adjustable       t
						     :fill-pointer     1))))
    
	  (if (exists-p mdhash 'mdoc)
	      (add-doc-to-mdoc mdhash dhash)
	      (add (make-mdoc  :name     mdoc-name
			       :author   author
			       :genre    (cdr (caddr metadata))
			       :docs     (make-array 1
						     :element-type     md5sum
						     :initial-contents dhash
						     :adjustable       t
						     :fill-pointer     1)
			       :hash     mdhash)))
    
	  (if (exists-p scribe 'agent)
	      (add-doc-to-scribe scribe dhash)
	      (add (make-agent :name     scribe
			       :copied   (make-array 1
						     :element-type     md5sum
						     :initial-contents dhash
						     :adjustable       t
						     :fill-pointer     1))))

	  (add   (make-doc   :mdoc      mdhash
			     :scribe    scribe
			     :length    (cadar indexed-doc)
			     :hash      dhash)))))