(in-package :org.kjerkreit.lingalyzer.feeder)

(defun read-file (path &optional (ignore-missing nil))
  "Reads a file and its associated metadata from disk. Returns a cons the metadata alist and the
  contents of the file as a string."

  (let ((file  (probe-file path))
	(mdata (probe-file (concatenate 'string path ".meta"))))

    (unless ignore-missing
      
      (unless file
	(error "No such file: ~S" path))
  
      (unless mdata
	(error "No metadata associated with file: ~S" path)))

    (if (and ignore-missing (or (not file) (not mdata)))
	nil
	(let ((data))
	  (with-open-file (s path)
	    (setf data (make-string (file-length s)))
	    (read-sequence data s))
	  (with-open-file (s (concatenate 'string path ".meta"))
	     
	    (cons (read s) data))))))

(defun read-files (files &optional (ignore-missing nil))
  "Calls (read-file ...) on each file."

  nil)