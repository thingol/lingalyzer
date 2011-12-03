(in-package :cl-user)

(defpackage #:org.kjerkreit.lingalyzer.stores
  (:nicknames la-s :LA-S)
  (:use #:common-lisp
	#:cl-fad
	#:cl-sql
	#:cl-sqlite
	#:org.kjerkreit.ngram)
  (:export #:ht
	   #:sql
	   #:add
	   #:get
	   #:remove
	   #:update
	   #:search))

(defpackage #:org.kjerkreit.lingalyzer.utils
  (:nicknames la-u :LA-U)
  (:use #:common-lisp
	#:split-sequence
	#:sb-md5)
  (:export #:))

(defpackage #:org.kjerkreit.lingalyzer
  (:nicknames la :LA)
  (:use #:common-lisp
	#:org.kjerkreit.lingalyzer.stores
	#:org.kjerkreit.lingalyzer.utils)
  (:export #:))