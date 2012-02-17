(in-package :org.kjerkreit.lingalyzer)

(defpackage #:org.kjerkreit.lingalyzer.store
  (:nicknames ling-store :LING-STORE)
  (:use #:cl
	#:org.kjerkreit.utils
	#:org.kjerkreit.utils.ngram)
  (:export ;; Store
   
           #:close-store
	   #:create-store
	   #:delete-store
	   #:open-store

	   ;; DB - general

	   #:add-rec
	   #:exists-p	   
   	   #:get-rec-all
	   #:get-rec-by
	   #:get-rec-one
	   #:remove-rec
	   #:update-rec

	   ;; DB - specific
	   
	   #:update-wf-count	   

	   ;; Index
	   #:find-ents
	   #:indexed-p))


	   

