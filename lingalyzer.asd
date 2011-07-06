;;; -*- Lisp -*-

(defpackage #:lingalyzer-system
  (:use #:asdf #:cl))

(in-package #:lingalyzer-system)

(defsystem org.kjerkreit.lingalyzer
  :version "0.0.1"
  :components ((:file "defpackage")
	       (:file "lingalyzer-utils-structures" :depends-on ("defpackage"))
               (:file "lingalyzer-utils-functions" :depends-on ("defpackage"))
	       (:file "lingalyzer" :depends-on ("lingalyzer-utils-functions"
						"lingalyzer-utils-structures"))))
