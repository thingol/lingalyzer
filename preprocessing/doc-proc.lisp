(in-package :org.kjerkreit.lingalyzer.preprocessing)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))

#|

record spec =: ('<type> :slot0 value0 .. :slotN valueN)
decoded by methods add-entity and update-entity.

|#

(defun process-doc (path)
  "Add document and relevant meta data to document db. Returns nil if document alrady exists."

  (let* ((doc       (read-file path))
	 (metadata  (car doc))
	 (mdoc-name (cdr (car                 metadata)))
	 (scribe    (cdr (cadddr              metadata)))
	 (dhash     (md5sum-strings-to-string scribe mdoc-name)))

    (if (exists-p 'doc dhash)
	nil
	(let* ((author      (cdr (cadr                metadata)))
	       (mdhash      (md5sum-strings-to-string author mdoc-name)))
	  
	  (unless (exists-p 'agent author)
	    (add-entity (list 'agent :name author)))
	  
	  (unless (exists-p 'mdoc mdhash)
	    (add-entity (list 'mdoc :name mdoc-name :author author :genre (cdr (caddr metadata)
	    :mdhash mdhash))))
	  
	  (unless (exists-p scribe 'agent)
	    (add-entity (list 'agent :name scribe)))
	  
	  (add-entity (list 'doc :mdhash mdhash :scribe scribe :dhash dhash))))))
