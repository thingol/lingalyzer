(in-package :org.kjerkreit.lingalyzer)

(defpackage #:org.kjerkreit.lingalyzer.preprocessing
  (:nicknames la-f :LA-f)
  (:use #:cl
	#:split-sequence
	#:org.kjerkreit.utils)
  (:export #:process-doc))