(in-package :org.kjerkreit.lingalyzer.feeder)

(defun read-dir (path)
  "Reads all (text) files in a directory. Returns a list of the result of applying read-file() to
all files in the directory (and sub-directories)."

  (let ((files))
    (cl-fad:walk-directory path #'(lambda (f)
                                    (push (read-file f t) files)))
    files))