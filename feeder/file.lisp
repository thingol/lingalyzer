(in-package :org.kjerkreit.lingalyzer.feeder)

(defun read-file (path)
  "Read file from disk. Returns list of strings."

  (flet ((file-string (path)
	   "Sucks up an entire file from PATH into a freshly-allocated string, returning
two values: the string and the number of bytes read. (shamelessly stolen from Cliki)"
	   
	   (with-open-file (s path)
	     (let* ((len (file-length s))
		    (data (make-string len)))
	       (values data (read-sequence data s))))))
    (format-string (file-string path))))

(defun read-metadata (path)
  "Read supplied metadata from file. The format is currently an sexp denoting an alist."

  (with-open-file (metadata-file (concatenate 'string path ".meta") :direction :input)
    (read metadata-file)))