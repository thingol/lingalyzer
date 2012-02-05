(in-package :cl-user)

(defpackage #:org.kjerkreit.lingalyzer
  (:nicknames la :LA)
  (:use #:common-lisp
	#:org.kjerkreit.lingalyzer.store
	#:org.kjerkreit.lingalyzer.utils))