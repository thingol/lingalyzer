;;; -*- Lisp -*-

(in-package #:asdf)

(defsystem org.kjerkreit.lingalyzer
  :version "0.0.4"
  :depends-on (split-sequence
	       org.kjerkreit.utils
	       org.kjerkreit.ngram)
  :components ((:module "store"
			:components
			((:file "package")
			 (:file "store"      :depends-on ("package"))
			 (:file "store-ht"   :depends-on ("store"))))
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
