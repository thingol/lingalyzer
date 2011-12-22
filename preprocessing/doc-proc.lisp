(in-package :org.kjerkreit.lingalyzer.preprocessing)

(defun index-document (word-forms dhash)
  "Indexes a document and returns a partial index.

The structure of the partial index:

'(length-of-doc . indexed-doc)"

  (let ((inverse)
	(pos 0))
    (dolist (wf (convert-string-to-tokens word-forms))
      (incf pos)
      (let ((wf-found (assoc wf inverse :test #'string=)))
	(if wf-found
	    (vector-push-extend pos (caddr wf-found))
	    (push (make-index-entry wf dhash pos) inverse))))
    (append (list pos
		  (make-instance 'indexed-doc
				 :forward (list dhash word-forms)
				 :inverse inverse)))))

(defun make-index-entry (wf dhash pos)
  "Make an index entry for a given word form."

  (list
   (list wf
	 dhash
	 (make-array 1 :element-type 'fixnum :initial-contents `(,pos) :adjustable t :fill-pointer 1))))

  
(defun process-doc (path)
  "Add document and relevant meta data to document db. Returns nil if document alrady exists."

  (let* ((doc       (read-file path))
	 (metadata  (car doc))
	 (mdoc-name (cdr (car                 metadata)))
	 (scribe    (cdr (cadddr              metadata)))
	 (dhash     (md5sum-strings-to-string scribe mdoc-name)))
    (if (exists-p dhash)
	nil
	(let* ((author      (cdr (cadr                metadata)))
	       (mdhash      (md5sum-strings-to-string author mdoc-name))
	       (indexed-doc (index-document (cdr doc) dhash)))

	  (if (exists-p author 'agent)
	      (add-entity (make-instance 'agent
					 :name     author
					 :authored (make-array 1
							       :element-type     'md5sum
							       :initial-contents mdhash
							       :adjustable       t
							       :fill-pointer     1))))
    
	  (if (exists-p mdhash 'mdoc)
	      (add-entity (make-instance 'mdoc
					 :name     mdoc-name
					 :author   author
					 :genre    (cdr (caddr metadata))
					 :docs     (make-array 1
							       :element-type     'md5sum
							       :initial-contents dhash
							       :adjustable       t
							       :fill-pointer     1)
					 :hash     mdhash)))
    
	  (if (exists-p scribe 'agent)
	      (add-entity (make-instance 'agent
					 :name     scribe
					 :copied   (make-array 1
							       :element-type     'md5sum
							       :initial-contents dhash
							       :adjustable       t
							       :fill-pointer     1))))

	  (add-entity (make-instance 'doc
				     :mdoc      mdhash
				     :scribe    scribe
				     :len       (car indexed-doc)
				     :hash      dhash)
		      (cdr indexed-doc))))))

