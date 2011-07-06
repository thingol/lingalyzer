(in-package :org.kjerkreit.lingalyzer.utils)

;; low-level functions
(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string, returning two values: the string and the number of bytes read. (shamelessly stolen from Cliki)"
  
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun good-char-p (char &optional ((newline nil))
  "Is it an alphanumeric, a space or possibly a newline?"
  
  (or (alpha-char-p char)
      (char-equal char #\Space)
      (if newline (char-equal #\Newline))))

(defun strip-text (text)
  "Strip text of all unwanted characters and convert to lowercase."
  
  (let ((stripped-string))
    (loop for char across text
	 when (good-char-p char)
	 collect (char-downcase char) into good-chars
	 finally (setf stripped-string (coerce good-chars 'string)))
    stripped-string))

(defun read-file (path)
  "Read file from disk. Returns list of strings."
  
  (split-sequence:split-sequence #\Space (strip-text (file-string path)) :remove-empty-subseqs t))


;; misc
;;(defun clean-text-to-file (doc)
;;  "Saves a copy of the cleaned text to file."
;;
;;  (with-open-file (out (concatenate 'string (doc-org-path doc) ".clean")
;;		       :direction :output :if-exists :supersede)
;;    (pprint terms out)))



(defun read-metadata (path)
  "Read supplied metadata from file and return as alist."

    (with-open-file (metadata-file (concatenate 'string path ".meta") :direction :input)
      (read metadata-file)))

(defun gen-doc-hash (metadata)
  "Make an md5sum from meta data."

  (sb-md5:md5sum-string
		    (format nil "~a.~a.~a.~a"
			    (cdr (nth 0 metadata))
			    (cdr (nth 1 metadata))
			    (cdr (nth 3 metadata))
			    (cdr (nth 4 metadata)))))

(defun has-doc-p (doc-hash docs-ht)
  "Compare meta data from two files to determine document equality."

  (multiple-value-bind (value pres) (gethash doc-hash docs-ht) pres))
  

;; First pass: check all terms, add if new
(defun process-terms (terms terms-ht)
  "Process terms. Add if not already in table, otherwise increase count by one."

  (defun add-term (term-m)
    (setf (gethash term-m terms-ht) (make-term :form term-m :count 1)))

  (defun inc-term (term-n)
    (setf (term-count term-n) (1+ (term-count term-n))))

  (defun rec (terms)
    (if terms
	(progn
	  (let* ((term-m (car terms))
		 (term-n (gethash term-m terms-ht)))
	    (if term-n
		(inc-term term-n)
		(add-term term-m))
	    (rec (cdr terms))))))
 
  (rec terms))

(defun get-author (metadata authors-ht)
  "Get the reference to the author's structure. Make it if we don't already have
 them."

  (let* ((author-name (cdr (assoc 'author metadata))
	 (author (gethash author-name authors-ht))))
    (if author
	author
	(progn
	  (setf author (make-author :name author-name))
	  author))))

(defun get-copyist (metadata copyists-ht)
  "Get the reference to the copyist's structure. Make it if we don't already have
 them."

  (let* ((copyist-name (cdr (assoc 'copied-by metadata))
	 (copyist-name (gethash copyist-name copyists-ht))))
    (if copyist
	copuist
	(progn
	  (setf copyist (make-copyist :name copyist-name))
	  copyist))))

(defun process-doc (terms path terms-ht docs-ht authors-ht)
  "Add document and relevant meta data to document db."

  (defun gen-terms-ref (terms-in terms-ref terms-ht)
    (if terms-in
	(gen-terms-ref
	 (cdr terms-in)
	 (append terms-ref
		 (cons (gethash (car terms-in) terms-ht) nil))
	 terms-ht)
	terms-ref))

  (let* ((metadata (read-metadata path))
	 (doc-hash (gen-doc-hash metadata)))

    (if (has-doc-p doc-hash docs-ht)
	nil
	(progn
	  (setf (gethash doc-hash docs-ht)
		(make-doc :name (cdr (nth 0 metadata))
			  :author (get-author metdata authors-ht)
			  :genre (cdr (nth 2 metadata))
			  :version (cdr (nth 3 metadata))
			  :copied-by (get-copyist metadata copyists-ht)
			  :word-count (list-length terms)
			  :org-file path))
	  (process-terms terms terms-ht)
	  (setf (doc-terms (gethash (cdr (nth 0 metadata)) docs-ht))
		(gen-terms-ref terms nil terms-ht))
	  t))))

;; document processing
(defun add-file (path terms-ht docs-ht)
  "Add a (new) file to db."

  (let ((terms (read-file path)))
	(process-terms terms terms-ht)
	(process-doc terms path terms-ht docs-ht)))

(defun search-docs (name docs-ht)
  "Check the document collection for a document matching the supplied name."

  