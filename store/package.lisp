(in-package :org.kjerkreit.lingalyzer)

(defpackage #:org.kjerkreit.lingalyzer.store
  (:nicknames la-s :LA-S)
  (:use #:common-lisp
	#:cl-sqlite
	#:org.kjerkreit.ngram)
  (:export #:add
	   #:get
	   #:remove
	   #:update
	   #:search))