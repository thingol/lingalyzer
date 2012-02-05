;;; -*- Lisp -*-

(in-package #:asdf)

(defsystem org.kjerkreit.lingalyzer
  :version "0.0.5"
  :author "Marius Hårstad Kjerkreit"
  :license "BSD-style"
  :depends-on (:split-sequence
	       :org.kjerkreit.utils
	       :org.kjerkreit.utils.ngram)
  :components ((:static-file "LICENSE")
	       (:file "package")
	       (:module "feeder"
			:components
			((:file "package")
			 (:file "file" :depends-on ("package"))
			 (:file "dir"  :depends-on ("file")))
			:depends-on ("package"))
	       (:module "preprocessing"
			:components
			((:file "package")
			 (:file "filter"   :depends-on ("package"))
			 (:file "doc-proc" :depends-on ("filter")))
			:depends-on ("package"
				     "store"
				     "feeder"))
	       (:module "store"
			:components
			((:file "package")
			 (:file "generics" :depends-on ("package"))
			 (:file "store"    :depends-on ("package"))
			 (:file "db"       :depends-on ("generics"))
			 (:file "index"    :depends-on ("generics"))
			 (:file "api"      :depends-on ("generics"
							"store"
							"db"
							"index"))
			:depends-on ("package")))
	       (:module "tests"
			:components
			((:file "package")
			 (:file "feeding"    :depends-on ("package")))
			:depends-on ("package"
				     "store"
				     "feeder"
				     "preprocessing"))))
