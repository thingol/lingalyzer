(in-package :cl-user)

(defpackage #:org.kjerkreit.lingalyzer.utils
  (:nicknames la-u :LA-U)
  (:use #:common-lisp
	#:split-sequence
	#:sb-md5
	#:org.kjerkreit.ngram)
#|  (:import-from #:split-sequence
		#:split-sequence)
  (:import-from #:sb-md5
		#:md5sum-string
		#:md5sum-file)
  (:import-from #:org.kjerkreit.ngram
		#:gen-n-grams)|#
  (:export #:init-store
	   #:process-doc
	   #:agent-name
	   #:meta-doc-name
	   #:word-form-form))

(defpackage #:org.kjerkreit.lingalyzer
  (:nicknames la :LA)
  (:use #:common-lisp
	#:org.kjerkreit.ngram
	#:org.kjerkreit.lingalyzer.utils)
#|  (:import-from #:org.kjerkreit.ngram
		#:compare-n-grams
		#:gen-n-grams)
  (:import-from #:org.kjerkreit.lingalyzer.utils *)|#
  (:export #:add-doc
	   #:new
	   #:search-agents
	   #:search-docs
	   #:search-terms
	   #:search-index))