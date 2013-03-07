(in-package :org.kjerkreit.lingalyzer.indexer)

(defun index (word-forms dhash)
  "Indexes a document and returns a cons of the length of the doc and a partial index.

The structure of the partial index:

'(length-of-doc forward-index inverse-index word-form-frequencies)"

  (let ((inverse)
	(pos 0)
	(tokenized-wfs (convert-string-to-tokens word-forms))
	(occurrences (make-hash-table :test 'equal)))
    (dolist (wf tokenized-wfs)
      (if (not (gethash wf occurrences))
	(setf (gethash wf occurrences) 1))
      (incf (gethash wf occurrences))
      (incf pos)
      (let ((wf-found (assoc wf inverse :test #'string=)))
	(if wf-found
	    (vector-push-extend pos (cadadr wf-found))
	    (push (make-index-entry wf dhash pos) inverse))))
    (values pos (list dhash tokenized-wfs) inverse occurrences)))

(defun make-index-entry (wf dhash pos)
  "Make an index entry for a given word form."
  
  (list wf
	(list dhash
	      (make-array 1
			  :element-type     'fixnum
			  :initial-contents `(,pos)
			  :adjustable       t
			  :fill-pointer     1))))