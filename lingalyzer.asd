;;; -*- Lisp -*-

(in-package #:asdf)

(defsystem org.kjerkreit.lingalyzer
  :version "0.0.4"
  :author "Marius Hårstad Kjerkreit"
  :license "BSD-style"
  :depends-on (split-sequence
	       org.kjerkreit.utils
	       org.kjerkreit.utils.ngram)
  :components ((:static-file "LICENSE")
	       (:module "store"
			:components
			((:file "package")
			 (:file "api"        :depends-on ("package"))
			 (:file "ht"         :depends-on ("api"))))
	       (:module "feeder"
			:components
			((:file "package")
			 (:file "processing" :depends-on ("package"))))
	       (:module "tests"
			:components
			((:file "package")
			 (:file "feeding"    :depends-on ("package")))
			:depends-on ("store"
				     "feeder"))))
