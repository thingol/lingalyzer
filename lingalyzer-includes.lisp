(in-package :org.kjerkreit.lingalyzer.utils)

(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string, returning two values: the string and the number of bytes read. (shamelessly stolen from Cliki)"
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun good-char-p (char &optional (newline nil))
  "Is it an alphanumeric, a space, a dot or maybe a newline?"
  (or (alpha-char-p char)
      (char-equal char #\Space)
;;      (char-equal char #\.)
      (if newline (char-equal #\Newline))))

;;(defun split-into-lines (text &optional (len 80))
;;  "Split input text into lines of a given length. Default is 80 characters."
;;  (read-char (make-string-input-stream text) nil nil)
;;   while char
;;	    collect char into line

(defun strip-text (text)
  "Strip text of all unwanted characters and convert to lowercase."
  (loop for char across text
	when (good-char-p char)
	collect (char-downcase char) into stripped-text
	finally (format t "~&~{~A~}~%" stripped-text)))

(defun slice-text (text)
  "Simple tokenizer that removes empty subsequences and uses newline as the delimiter written to get some practice writing in lisp."
  (let ((tokens ()))
    hent char fra text
    sjekk char = space | alpha
    hvis space sjekk neste
    hvis alpha ta vare på
    sjekk neste (peekchar)
    hvis space lagre
    gå ut av loop
    hvis alpha sjekk neste
    