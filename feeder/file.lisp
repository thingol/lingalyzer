(in-package :org.kjerkreit.lingalyzer.feeder)

(defun read-file (path &optional (ignore-missing nil))
  "Reads a file and its associated metadata from disk. Returns a cons the metadata alist and the
  contents of the file as a string."

  (let ((file  (probe-file path))
	(mdata (probe-file (concatenate 'string path ".meta"))))

    (if (not ignore-missing)
        (when (not file)
          (error "No such file: ~S" path))
        (when (not mdata)
          (error "No metadata associated with file: ~S" path)))

    (if (and ignore-missing
             (or (not file)
                 (not mdata)))
	nil
        (with-open-file (f path)
	  (with-open-file (meta (concatenate 'string path ".meta"))
            (let ((data (make-string (file-length f))))
              (read-sequence data f)	     
              (cons (read meta) data)))))))

(defun read-files (files &optional (ignore-missing nil))
  "Calls read-file() on each file in files."

  (loop for file in files
       collect (read-file file ignore-missing)))