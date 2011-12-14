(in-package :org.kjerkreit.lingalyzer)

(defpackage #:org.kjerkreit.lingalyzer.feeder
  (:nicknames la-f :LA-f)
  (:use #:common-lisp
	#:split-sequence
	#:org.kjerkreit.utils)
  (:export #:process-doc))