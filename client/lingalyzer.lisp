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

(defvar *doc-table* nil)
(defvar *doc-content-table* nil)

(defvar *db* nil)

;; low level
(defun get-index (type)
  "Returns the symbol used to store the index for the given type of object."

  (symbol-value
   (find-symbol
    (concatenate
     'string "*" (if (stringp type)
		     (format nil "~:@(~a~)" type)
		     (format nil "~a" type)) "-INDEX*"))))

(defun get-table (type)
  "Returns the symbol used to store the table for the given type of object."

  (symbol-value
   (find-symbol
    (concatenate
     'string "*" (if (stringp type)
		     (format nil "~:@(~a~)" type)
		     (format nil "~a" type)) "-TABLE*"))))



;; exported functions
;;

(defun add-file (path)
  "Add a (new) file to db."

  (process-doc path *db*))
;;
;; search
;;
(defun find-agent (query &optional (threshold 0.65))
  "Search the agents in the collection. Threshold is set to 65% similarity
  based on ngram comparison."

  (find-object 'agent query threshold))

(defun find-doc (query &optional (threshold 0.65))
  "Search the documents in the collection. Threshold is set to 65% similarity
  based on ngram comparison."

  (find-object 'meta-doc query threshold))

(defun find-word-form (query &optional (threshold 0.65))
  "Search the word-forms in the collection. Threshold is set to 65% similarity
  based on ngram comparison."

  (find-object 'word-form query threshold))

(defun find-object (type query threshold)
  "Return all objects of specified type using matches produces by index-lookup."

  (let ((res))
    (dolist (key (search-index (get-index type) query threshold))
      (setf res (cons (gethash key (get-table type) res)))
    (setf res (sort res #'string< :key #'(lambda (obj) (slot-value 'denom obj))))

    res))

(defun index-lookup (index query threshold)
  "Search the specified index."

  (let ((matches)
	(query-ngrams (gen-n-grams query)))
    
    (map 'list #'(lambda (x)
		   (when (>= (compare-n-grams query-ngrams (car x)) threshold)
		     (setf matches (cons (cdr x) matches))))
	 (get-index type))
    
    matches))

;;
;; stats
;;
(defmacro get-that-number (table slot pred)
  "Used to generate stats."

  `(let ((res)
	 (cur (if (eql ,pred #'<)
		  (MOST-POSITIVE-FIXNUM)
		  0))
	 (filter #'(lambda (obj)
		     (let* ((sval (slot-value ,slot obj))
			    (val (if (listp sval)
				     (list-length sval)
				     sval)))
		       (cond (((funcall ,pred val cur)) (setf res (list obj)) (setf cur val))
			     ((= val cur) (setf res (cons obj res))))))))

     (maphash #'(lambda (k v)
		  (funcall filter v)) ,table)

     res))


(defun most-authored ()
  "Find the most prolific author(s)."

  (get-that-number *agent-table* 'authored #'>))

(defun least-authored ()
  "Find the most prolific author(s)."

  (get-that-number *agent-table* 'authored #'<))

(defun most-copied ()
  "Find the most prolific copyist(s)."

  (get-that-number *agent-table* 'copied #'>))

(defun least-copied ()
  "Find the most prolific copyist(s)."

  (get-that-number *agent-table* 'copied #'<))

(defun longest-doc ()
  "Find the longest document(s)."

  (get-that-number *doc-table* 'word-count #'>))

(defun shortest-doc ()
  "Find the longest document(s)."

  (get-that-number *doc-table* 'word-count #'<))

(defun most-used-word-form ()
  "Find the most used word form(s)."

  (get-that-number *word-form-table* 'count #'>))

(defun least-used-word-form ()
  "Find the least used word form(s)."

  (get-that-number *word-form-table* 'count #'<))

(defun agent-count (&optional (f nil))

  (if (eql f nil)
      (let ((c 0))
	(maphash #'(lambda (k v)
		     (when (> (list-length (funcall f v)) 0)
		       (incf c))) *agent-table*)
	c)
      (hash-table-count *agent-table*)))

(defun author-count ()

  (agent-count #'(lambda (obj) (slot-value 'authored obj))))

(defun copyist-count ()

  (agent-count #'(lambda (obj) (slot-value 'copied obj))))

(defun db-stats ()
  "Lots of nice little numbers :)"

    `((MOST-AUTHORED        . ,(most-authored))
      (MOST-COPIED          . ,(most-copied))
      (LONGEST-DOC          . ,(longest-doc))
      (MOST-USED-WORD-FORM  . ,(most-used-word-form))
      (LEAST-USED-WORD-FORM . ,(least-used-word-form))
      (NUMBER-OF-DOCS       . ,(hash-table-count (get-table meta-doc))
      (NUMBER-OF-AUTHORS    . ,(author-count))
      (NUMBER-OF-COPYISTS   . ,(copyist-count))
      (NUMBER-OF-AGENTS     . ,(hash-table-count (get-table agent)))
      (NUMBER-OF-WORD-FORMS . ,(hash-table-count *word-form-table*))))
