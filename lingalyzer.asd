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
	       (:module "feeder"
			:components
			((:file "package")
			 (:file "file" :depends-on ("package"))
			 (:file "dir"  :depends-on ("file"))))
	       (:module "preprocessing"
			:components
			((:file "package")
			 (:file "filter"   :depends-on ("package"))
			 (:file "doc-proc" :depends-on ("filter")))
			:depends-on ("store"
				     "feeder"))
	       (:module "store"
			:components
			((:file "package")
			 (:file "api"                :depends-on ("package"))
			 (:file "datatypes"          :depends-on ("package"))
			 (:file "store-internals"    :depends-on ("package"))
			 (:file "exported-functions" :depends-on ("api"
								  "store-internals"))
			 (:file "ht"                 :depends-on ("api"
							          "datatypes"))))
	       (:module "tests"
			:components
			((:file "package")
			 (:file "feeding"    :depends-on ("package")))
			:depends-on ("store"
				     "feeder"
				     "preprocessing"))))
