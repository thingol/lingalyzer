(in-package :org.kjerkreit.lingalyzer.utils)

;; low-level functions
(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string, returning
two values: the string and the number of bytes read. (shamelessly stolen
from Cliki)"
  
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
  
  (split-sequence:split-sequence #\Space (strip-text (file-string path))
				 :remove-empty-subseqs t))


(defun read-metadata (path)
  "Read supplied metadata from file and return as alist."

    (with-open-file (metadata-file
		     (concatenate 'string path ".meta")
		     :direction :input)
      (read metadata-file)))

(defun gen-doc-hash (md)
  "Make an md5sum from meta data."

  (sb-md5:md5sum-string
   (concatenate 'string
		(cdr (assoc 'author md))
		(cdr (assoc 'genre md))
		(cdr (assoc 'copied-by md))
		(cdr (assoc 'name md)))))

(defun has-doc-p (doc-hash docs-ht)
  "Compare meta data from two files to determine document equality."

  (multiple-value-bind (value pres) (gethash doc-hash docs-ht) pres))
  

;; First pass: check all terms, add if new
(defun process-terms (terms terms-ht)
  "Process terms. Add if not already in table, otherwise increase count by one.
Returns a list of references to the terms processed in the orderd received."

  (flet ((get-term (term)
	   (let ((th (gethash term terms-h)))
	     (if th
		 (progn (incf (term-count th)) th)
		 (setf (gethash term terms-ht)
			  (make-term :form term :count 1))))))

    (do ((ts
	  (cdr terms)
	  (cdr ts))
	 (processed-terms
	  (list (get-term (car terms)))
	  (append processed-terms (list (get-term (car ts))))))
	 ((not ts) processed-terms))))
  
(defun get-author (metadata authors-ht)
  "Get the reference to the author's structure. Make it if we don't already have
 them."

  (let* ((an (cdr (assoc 'author metadata))
	 (author (gethash an authors-ht))))
    (if author
	author
	(progn
	  (setf author (make-author :name an))
	  author))))

(defun get-copyist (metadata copyists-ht)
  "Get the reference to the copyist's structure. Make it if we don't already have
 them."

  (let* ((cn (cdr (assoc 'copied-by metadata))
	 (copyist (gethash cn copyists-ht))))
    (if copyist
	copyist
	(progn
	  (setf copyist (make-copyist :name cn))
	  copyist))))

(defun process-doc (pterms path docs-ht authors-ht)
  "Add document and relevant meta data to document db."

  (let* ((metadata (read-metadata path))
	 (doc-hash (gen-doc-hash metadata)))

    (cond ((has-doc-p doc-hash docs-ht)
	   (let ((ndv
	   (setf (doc-versions(gethash doc-hash docs-ht)
	(progn
	  (setf (gethash doc-hash docs-ht)
		(make-doc :name (cdr (nth 0 metadata))
			  :ngrams (ngram:gen-n-grams (cdr (nth 0 metadata)))
			  :author (get-author metdata authors-ht)
			  :genre (cdr (nth 2 metadata))
			  :versions
			  (list (make-doc-version
				 :version 0
				 :word-count (list-length pterms)
				 :org-file path
				 :org-file-hash (sb-md5:md5sum-file path))))
	  (setf (doc-terms (gethash (cdr (nth 0 metadata)) docs-ht)) pt)))))

;; document processing
(defun add-file (path terms-ht docs-ht)
  "Add a (new) file to db."

  (let ((pt (process-terms (read-file path) terms-ht)))
	(process-doc pt path docs-ht)))

(defun search-docs (name docs-ht &optional (threshold 0.65))
  "Check the document collection for documents matching the supplied name."

  "maphash compare-n-grams if > threshold append to list, return"
  1)
