;;; -*- Lisp -*-

(in-package #:asdf)

(defsystem org.kjerkreit.lingalyzer
  :version "0.0.3"
  :depends-on (split-sequence
	       sb-md5
	       org.kjerkreit.ngram)
  :components ((:file "defpackage")
	       (:file "lingalyzer-storage-ht"      :depends-on ("defpackage"))
	       ;;; (:file "lingalyzer-storage-clsql"   :depends-on ("defpackage")) ;;; soon to come
	       (:file "lingalyzer-storage"         :depends-on ("lingalyzer-storage-ht"))
	       (:file "lingalyzer-utils-functions" :depends-on ("defpackage"))
	       (:file "lingalyzer"                 :depends-on ("lingalyzer-storage"
								"lingalyzer-utils-structures"))))
