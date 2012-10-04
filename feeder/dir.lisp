(in-package :org.kjerkreit.lingalyzer.feeder)

(defun read-dir (path)
  "Reads all (text) files in a directory."

  (let ((files))
    (cl-fad:walk-directory path #'(lambda (f)
                                    (push (read-file f t) files)))
    files))