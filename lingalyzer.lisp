;;
;; 
;;
(in-package :org.kjerkreit.lingalyzer)

;; exported functions
;;
;; manage documents
;;
(defun add-file (path)
  "Add a (new) file to db."

	(process-doc path +docs+ +word-forms+))
;;
;; search
;;
(defun search-agents (query &optional (threshold 0.65))
  "Search the agents in the collection. Threshold is set to 65% similarity
  based on ngram comparison if indexing is in use."
  
  (search-index +agent-index+ query threshold))

(defun search-docs (query &optional (threshold 0.65))
  "Search the documents in the collection. Threshold is set to 65% similarity
  based on ngram comparison if indexing is in use."
  
  (search-index +doc-index+ query threshold))

(defun search-word-forms (query &optional (threshold 0.65))
  "Search the word-forms in the collection. Threshold is set to 65% similarity
  based on ngram comparison if indexing is in use."

  (let ((res))
    (dolist (key (search-index +word-form-index+ query threshold))
      (setf res (cons (gethash key +word-forms+) res)))
    (setf res (sort res #'string< :key #'word-form-name))))
    

(defun search-index (index query threshold)
  "Check the document collection for documents matching the supplied name."

  (let ((matches)
	(query-ngrams (gen-n-grams query)))
    
    (maphash #'(lambda (k v)
		 (when (>= (compare-n-grams query-ngrams k) threshold)
		   (setf matches (cons v matches))))
	     index)
    
    matches))

