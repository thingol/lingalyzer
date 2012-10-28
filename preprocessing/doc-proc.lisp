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
	 (dhash     (ku:md5sum-strings scribe mdoc-name)))

    (if (exists-p 'doc dhash)
	nil ;; signal that the document already exists?
	(let* ((author      (cdr (cadr metadata)))
	       (mdhash      (ku:md5sum-strings author mdoc-name)))
	  
	  (if (not (exists-p 'agent author))
              (add-rec (list 'agent
                                :name author)))
	  
	  (if (not (exists-p 'mdoc mdhash))
              (add-rec (list 'mdoc
                                :name mdoc-name
                                :author author
                                :genre (cdr (caddr metadata))
                                :mdhash mdhash)))
	  
	  (if (not (exists-p scribe 'agent))
              (add-rec (list 'agent
                                :name scribe)))
	  
	  (add-rec (list 'doc
                            :mdhash mdhash
                            :scribe scribe
                            :dhash dhash))))))
