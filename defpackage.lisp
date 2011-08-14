(in-package :cl-user)


(defpackage :org.kjerkreit.lingalyzer.utils
  (:nicknames "LL-UTILS")
  (:use :common-lisp
	:split-sequence
	:sb-md5
	:org.kjerkreit.ngram)
  (:import-from :org.kjerkreit.ngram
		:compare-n-grams
		:gen-n-grams)
  (:export :init-store
	   :add-doc
	   :search-agents
	   :search-docs
	   :search-terms))

;; TODO
;; remove doc/file
;; list all doc
;; list all authors
;; list all copyists
;; list all terms
;; output statistics
;;

(defpackage :org.kjerkreit.lingalyzer
  (:nicknames "LL")
  (:use :common-lisp
	:org.kjerkreit.lingalyzer.utils)
  (:import-from :org.kjerkreit.lingalyzer.utils *))