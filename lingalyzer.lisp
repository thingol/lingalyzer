;;
;; 
;;
(in-package :org.kjerkreit.lingalyzer)

;; globals
(defvar *agent-table* nil)
(defvar *agent-index* nil)

(defvar *meta-doc-table* nil)
(defvar *meta-doc-index* nil)

(defvar *word-form-table* nil)
(defvar *word-form-index* nil)

(defvar *db* (list (list *agent-table* *meta-doc-table* *word-form-table*)
		   (list *agent-index* *meta-doc-index* *word-form-index*)))

;; exported functions
;;
(defun new ()
  "Temporary function till I can figure out how I want to do this..."


    (setf *agent-table* (make-hash-table :test 'equal))
    (setf *agent-index* (make-array 200  :element-type 'cons :adjustable t :fill-pointer 0))

    (setf *meta-doc-table* (make-hash-table :test 'equal))
    (setf *meta-doc-index* (make-array 150  :element-type 'cons :adjustable t :fill-pointer 0))
    
    (setf *word-form-table* (make-hash-table :test 'equal))
    (setf *word-form-index* (make-array 5000 :element-type 'cons :adjustable t :fill-pointer 0)))


;; manage documents
;;
(defun add-file (path)
  "Add a (new) file to db."

	(process-doc path *db*))
;;
;; search
;;
(defun search-agents (query &optional (threshold 0.65))
  "Search the agents in the collection. Threshold is set to 65% similarity
  based on ngram comparison."
  
  (search-index *agent-index* query threshold))

(defun search-docs (query &optional (threshold 0.65))
  "Search the documents in the collection. Threshold is set to 65% similarity
  based on ngram comparison."
  
  (search-index *meta-doc-index* query threshold))

(defun search-word-forms (query &optional (threshold 0.65))
  "Search the word-forms in the collection. Threshold is set to 65% similarity
  based on ngram comparison."

  (let ((res))
    (dolist (key (search-index *word-form-index* query threshold))
      (setf res (cons (gethash key *word-form-table*) res)))
    (setf res (sort res #'string< :key #'word-form-form))))
    

(defun search-index (index query threshold)
  "Search the specified index."

  (let ((matches)
	(query-ngrams (gen-n-grams query)))
    
    (map 'list #'(lambda (k v)
		   (when (>= (compare-n-grams query-ngrams k) threshold)
		     (setf matches (cons v matches))))
	 index)
    
    matches))

