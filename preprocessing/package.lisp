(in-package :org.kjerkreit.lingalyzer)

(defpackage #:org.kjerkreit.lingalyzer.preprocessing
  (:nicknames la-pp :LA-pp)
  (:use #:cl
	#:split-sequence
	#:org.kjerkreit.utils
	#:org.kjerkreit.lingalyzer.store
	#:org.kjerkreit.lingalyzer.feeder)
  (:export #:process-doc))