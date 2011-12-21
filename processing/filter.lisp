(in-package :org.kjerkreit.lingalyzer.utils)

(defun good-char-p (char)
  "Is it an alphanumeric or a space?"

  (or (alpha-char-p char)
      (char-equal char #\Space)))

(defun format-string (string
		      &optional (filter #'good-char-p) (delim #\Space) (remove-empty-subseqs t))
  "Strip text of all unwanted characters, convert to lowercase and tokenize."

  (let ((stripped-string))
    (loop for char across text
       when (filter char)
       collect (char-downcase char) into good-chars
       finally (setf stripped-string (coerce good-chars 'string)))
    (split-sequence delim stripped-string :remove-empty-subseqs remove-empty-subseqs)))