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
	   #:delete-rec
	   #:exists-p	   
	   #:get-rec-all
	   #:get-rec-by
	   #:get-rec-one
	   #:update-rec

	   ;; DB - specific
	   #:update-wf-count	   

	   ;; Index
	   #:find-rec
	   #:indexed-p
	   #:load-index
	   #:merge-inverse
	   #:save-index
	   
	   #:load-forward
	   #:load-inverse
	   #:load-fuzzy
	   
	   #:save-forward
	   #:save-inverse
	   #:save-fuzzy))
