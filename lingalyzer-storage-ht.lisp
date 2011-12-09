(in-package :org.kjerkreit.lingalyzer.storage)

;; struct for db

(defstruct ht
  (dirty-db   nil)      
  (agent-t    (make-hash-table  :test 'equal)
	      :type hash-table)
  (agent-i    (make-array       200
			        :element-type 'cons
			        :adjustable   t
			        :fill-pointer 0)
	      :type array)
  (mdoc-t     (make-hash-table  :test         'equalp)
	      :type hash-table)
  (mdoc-i     (make-array       150
			        :element-type 'cons
			        :adjustable t
			        :fill-pointer 0)
	      :type array)
  (wform-t    (make-hash-table  :test         'equal)
	      :type hash-table)
  (wform-i    (make-array       5000
			        :element-type 'cons
			        :adjustable   t
			        :fill-pointer 0)
	      :type array)
  (doc-t      (make-hash-table  :test         'equalp)
	      :type hash-table)
  (doc-cont-t (make-hash-table  :test         'equalp)
	      :type hash-table))


;;;; low level db operations
(defmethod close-db ((db ht))
  (drop-db db))

(defmethod drop-db ((db ht))
  (setf db nil))

(defmethod __gc-db ((db ht) rem-ent))

;; old versions, and rather messy...
(new-doc
	  #'(lambda ()
	      (setf (gethash doc-hash doc-table)
		    (make-doc
		     :meta-doc      meta-doc-hash
		     :copied-by     copyist-name
		     :word-count    (list-length processed-word-forms)
		     :org-file      path
		     :org-file-hash (md5sum-file path)
		     :word-forms    doc-hash))
	      (setf (gethash doc-hash doc-content-table) processed-word-forms)
	      (setf (meta-doc-docs meta-doc)
		    (cons
		     doc-hash
		     (meta-doc-docs meta-doc)))))
	 (new-author
	  #'(lambda ()
	      (setf (gethash author-name agent-table)
		    (make-agent
		     :name     author-name
		     :authored (list meta-doc-hash)))
	      (setf (meta-doc-author meta-doc) author-name)
	      (vector-push-extend (cons (gen-n-grams author-name) author-name) agent-index)))
	 (new-copyist
	  #'(lambda ()
	      (setf copied-by
		    (setf (gethash copyist-name agent-table)
			  (make-agent
			   :name copyist-name
			   :copied (list doc-hash))))
	      (vector-push-extend (cons (gen-n-grams copyist-name) copyist-name) agent-index)))
	 (new-meta-doc
	  #'(lambda ()
	      (setf meta-doc
		    (setf (gethash meta-doc-hash meta-doc-table)
			  (make-meta-doc
			   :name   meta-doc-name
			   :genre  (cdr (caddr metadata)))))
	      (vector-push-extend (cons (gen-n-grams meta-doc-name) meta-doc-hash) meta-doc-index))))
    
;;;; Content

;;;;; add

(defmethod __add ((db ht) (entity agent))
      (setf (gethash (agent-name entity) agent-t) entity)
      (vector-push-extend (cons (gen-n-grams author-name) author-name) agent-i))

(defmethod __add ((db ht) (entity doc))
  (let ((doc-hash (doch-hash doc))
	(mdoc-docs (mdoc-docs (gethash (doc-mdoc doc) mdoc-t))))
    
      (setf (gethash doc-hash doc-t) entity)
      (setf mdoc-docs (cons doc-hash mdoc-docs))))

(defmethod __add ((db ht) (entity mdoc))
  (let* ((mdoc-name (mdoc-name entity))
	 (mdoc-hash (md5sum-string (concatenate 'string	(mdoc-author entity) mdoc-name))))
    
    (setf (gethash mdoc-hash mdoc-t) entity)
    (vector-push-extend (cons (gen-n-grams mdoc-name) mdoc-hash) mdoc-i)))

(defmethod __add ((db ht) (entity word-form))
  (setf (gethash (word-form-form entity) wform-t) entity))


;;;;; get

(defmethod __get ((db ht) (entity agent))
  (gethash (agent-name entity) agent-t))

(defmethod __get ((db ht) (entity doc))
  (gethash (doc-hash entity) doc-t))

(defmethod __get ((db ht) (entity mdoc))
  (gethash (mdoc-hash entity) mdoc-t))

(defmethod __get ((db ht) (entity word-form))
  (gethash (word-form-form entity) wform-t))

;;;;; increase-wf-count

(defmethod __increase-wf-count ((db ht) wf))


;;;;; remove

(defmethod __remove ((db ht) (entity agent))
  (remhash (agent-name entity) agent-t))

(defmethod __remove ((db ht) (entity doc))
  (remhash (doc-hash entity) doc-t))

(defmethod __remove ((db ht) (entity mdoc))
  (remhash (mdoc-hash entity) mdoc-t))

(defmethod __remove ((db ht) (entity word-form))
  (remhash (word-form-form entity) wform-t))

;;;;; update

(defmethod __update ((db ht) (entity word-form))
  (setf (gethash (word-form-form entity) wform-t) enity))

;;;;; search

(defmethod __search ((db ht) query entity-type)
  (if entity-type
      (etypecase entity-type
	(:agent
	 

  )


;;;; internal support functions
(defun index-lookup (index query threshold)
  "Search the specified index."

  (let ((matches)
	(query-ngrams (gen-n-grams query)))
    
    (map 'list #'(lambda (x)
		   (when (>= (compare-n-grams query-ngrams (car x)) threshold)
		     (setf matches (cons (cdr x) matches))))
	 (get-index type))
    
    matches))