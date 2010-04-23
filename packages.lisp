(defpackage :org.kjerkreit.lingalyzer.utils
  (:use :common-lisp)
  (:export :file-string
	   :split-into-lines
	   :strip-text))

(defpackage :org.kjerkreit.lingalyzer
  (:use :common-lisp :org.kjerkreit.lingalyzer.utils))