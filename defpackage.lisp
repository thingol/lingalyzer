(in-package :cl-user)


(defpackage #:org.kjerkreit.lingalyzer.utils
  (:nicknames "lingalyzer-utils")
  (:use #:common-lisp
	#:split-sequence
	#:sb-md5
	#:ngram)
  (:import-from #:split-sequence
		#:split-sequence)
  (:import-from #:sb-md5
		#:md5sum-string
		#:md5sum-file)
  (:import-from #:ngram
		#:compare-n-grams
		#:gen-n-grams)
  (:export #:init-store
	   #:add-doc
	   #:search-agents
	   #:search-docs
	   #:search-terms
	   #:search-index))

(defpackage #:org.kjerkreit.lingalyzer
  (:nicknames "lingalyzer")
  (:use #:common-lisp
	#:lingalyzer-utils)
  (:import-from #:lingalyzer-utils *))