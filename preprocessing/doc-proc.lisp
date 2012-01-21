(in-package :org.kjerkreit.lingalyzer.preprocessing)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))

(defun index-document (word-forms dhash)
  "Indexes a document and returns a cons of the length of the doc and a partial index.

The structure of the partial index:

'(length-of-doc . doc-index)"

  (let ((inverse)
	(pos 0)
	(tokenized-wfs (convert-string-to-tokens word-forms))
	(occurrences (make-hash-table :test 'equal)))
    (dolist (wf tokenized-wfs)
      (unless (gethash wf occurrences)
	(setf (gethash wf occurrences) 1))
      (incf (gethash wf occurrences))
      (incf pos)
      (let ((wf-found (assoc wf inverse :test #'string=)))
	(if wf-found
	    (vector-push-extend pos (cadadr wf-found))
	    (push (make-index-entry wf dhash pos) inverse))))
    (maphash #'(lambda (k v)
		 (unless (exists-p k 'word-form)
		   (add-entity (make-instance 'word-form :form k)))
		 (increase-wf-count k v)) occurrences)
    (list pos
	  (make-instance 'doc-index
			 :forward (list dhash tokenized-wfs)
			 :inverse inverse))))

(defun make-index-entry (wf dhash pos)
  "Make an index entry for a given word form."
  
  (list wf
	(list dhash
	      (make-array 1
			  :element-type     'fixnum
			  :initial-contents `(,pos)
			  :adjustable       t
			  :fill-pointer     1))))
  
(defun process-doc (path)
  "Add document and relevant meta data to document db. Returns nil if document alrady exists."

  (let* ((doc       (read-file path))
	 (metadata  (car doc))
	 (mdoc-name (cdr (car                 metadata)))
	 (scribe    (cdr (cadddr              metadata)))
	 (dhash     (md5sum-strings-to-string scribe mdoc-name)))

    (if (exists-p dhash 'doc)
	nil
	(let* ((author      (cdr (cadr                metadata)))
	       (mdhash      (md5sum-strings-to-string author mdoc-name))
	       (indexed-doc (index-document (cdr doc) dhash)))
	  
	  (unless (exists-p author 'agent)
	    (add-entity (make-instance 'agent :name author)))
	  
	  (unless (exists-p mdhash 'mdoc)
	    (add-entity (make-instance 'mdoc
				       :name     mdoc-name
				       :author   author
				       :genre    (cdr (caddr metadata))
				       :docs     (make-array 1
							     :element-type     'md5sum
							     :initial-contents `(,dhash)
							     :adjustable       t
							     :fill-pointer     1)
				       :hash     mdhash)))
	  
	  (unless (exists-p scribe 'agent)
	    (add-entity (make-instance 'agent :name scribe)))
	  
	  (add-entity (make-instance 'doc
				     :mdoc      mdhash
				     :scribe    scribe
				     :len       (car indexed-doc)
				     :hash      dhash)
		      (cadr indexed-doc))))))

