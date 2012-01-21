(in-package :org.kjerkreit.lingalyzer.tests)

(defun feeder-test ()
  (let* ((store (new-store "bla" 'ht-db 'ht-index t))
	 (db (car store))
	 (index (cdr store)))
    ;;  (process-doc
    ;;  "/home/marius/Dropbox/Kode/cl/lingalyzer/test-data/de-bello-gallico-01-01-latin-library.txt")

    (process-doc
     "/home/marius/Dropbox/Kode/cl/lingalyzer/test-data/kort")
  

    (describe db)
    (describe index)
    (print (slot-value index 'doc))
    (slot-value index 'word-form)))
