(in-package :org.kjerkreit.lingalyzer)

(defpackage #:org.kjerkreit.lingalyzer.store
  (:nicknames la-s :LA-S)
  (:use #:cl
	#:org.kjerkreit.utils
	#:org.kjerkreit.utils.ngram)
  (:export #:add-entity
	   #:close-stpre
	   #:drop
	   #:exists-p
	   #:find-entities
	   #:gc
	   #:get
	   #:get-all
	   #:get-by
	   #:get-childless
	   #:get-orphans
	   #:increase-wf-count
	   #:indexed-p
	   #:new-store
	   #:open-store
	   #:remove-entity
	   #:update
	   ;;
	   #:ht-db
	   #:ht-index
	   ;;
	   #:agent
	   #:doc
	   #:doc-index
	   #:mdoc
	   #:word-form))

