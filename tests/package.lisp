(in-package :org.kjerkreit.lingalyzer)

(defpackage #:org.kjerkreit.lingalyzer.tests
  (:nicknames la-t :LA-T)
  (:use #:cl
	#:org.kjerkreit.lingalyzer.store
	#:org.kjerkreit.lingalyzer.feeder
	#:org.kjerkreit.lingalyzer.preprocessing))
  