;;; -*- Lisp -*-

(in-package #:cl-user)
(defpackage #:lingalyzer-system
  (:use #:asdf #:cl))

(in-package #:lingalyzer-system)
(asdf:load-system :split-sequence)
(asdf:load-system :sb-md5)
(asdf:load-system :org.kjerkreit.ngram)

(defsystem org.kjerkreit.lingalyzer
  :version "0.0.2"
  :components ((:file "defpackage")
	       (:file "lingalyzer-utils-structures" :depends-on ("defpackage"))
               (:file "lingalyzer-utils-functions" :depends-on ("defpackage"))
	       (:file "lingalyzer" :depends-on ("lingalyzer-utils-functions"
						"lingalyzer-utils-structures"))))
