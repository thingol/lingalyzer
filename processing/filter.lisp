(in-package :org.kjerkreit.lingalyzer.preprocessing)

(defun good-char-p (char)
  "Is it an alphanumeric or a space?"

  (or (alpha-char-p char)
      (char-equal char #\Space)))

(defun convert-string-to-tokens (string
		      &optional (filter #'good-char-p) (delim #\Space) (rempty t))
  "Strip text of all unwanted characters, convert to lowercase and tokenize."

  (loop for char across text
     when (filter char)
     collect (char-downcase char) into good-chars
     finally (split-sequence delim
			     (coerce good-chars 'string)
			     :remove-empty-subseqs rempty)))