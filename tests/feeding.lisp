(in-package :org.kjerkreit.lingalyzer.tests)

(defparameter *db* nil)
(defparameter *index* nil)

(defun feeder-test ()
  (let ((store (create-store "bla"))
	(dir "/home/marius/Dropbox/Kode/cl/lingalyzer/test-data/")
	(test-data '("de-bello-gallico-01-01-latin-library.txt"
		     "de-bello-gallico-01-01-wikisource.txt"
		     "de-bello-gallico-liber-primvs-thelatinlibrary.com.txt"
		     "de-bello-gallico-liber-qvartvs-thelatinlibrary.com.txt"
		     "de-bello-gallico-liber-qvintvs-thelatinlibrary.com.txt"
		     "de-bello-gallico-liber-secundus-thelatinlibrary.com.txt"
		     "de-bello-gallico-liber-septimvs-thelatinlibrary.com.txt"
		     "de-bello-gallico-liber-sextvs-thelatinlibrary.com.txt"
		     "de-bello-gallico-liber-tertius-thelatinlibrary.com.txt")))

    (setf *db* (car store))
    (setf *index* (cdr store))
    
    (dolist (test-datum test-data)
      (process-doc (concatenate 'string dir test-datum)))

    ;;(process-doc
    ;; "/home/marius/Dropbox/Kode/cl/lingalyzer/test-data/kort")
  

    ;;(describe db)
    ;;(describe index)
    ;;(print (slot-value index 'doc))
    ;;(slot-value index 'word-form))

    ))
