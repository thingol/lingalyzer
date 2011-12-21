(in-package :org.kjerkreit.lingalyzer)

(defpackage #:org.kjerkreit.lingalyzer.store
  (:nicknames la-s :LA-S)
  (:use #:cl
	#:org.kjerkreit.ngram)
  (:export #:add
	   #:close
	   #:drop
	   #:exists-p
	   #:gc
	   #:get
	   #:get-all
	   #:get-by
	   #:get-childless
	   #:get-orphans
	   #:increase-wf-count
	   #:indexed-p
	   #:new-store
	   #:open
	   #:remove
	   #:search
	   #:update))