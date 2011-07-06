(in-package :cl-user)

(asdf:load-system :split-sequence)
(asdf:load-system :sb-md5)

(defpackage :org.kjerkreit.lingalyzer.utils
  (:nicknames "LL-UTILS")
  (:use :common-lisp :split-sequence :sb-md5)
  (:export add-file))

;; TODO
;; search for: - doc
;;             - author
;;             - translator
;; generate unique ID for docs
;; remove doc/file

(defpackage :org.kjerkreit.lingalyzer
  (:nicknames "LL")
  (:use :common-lisp :org.kjerkreit.lingalyzer.utils))