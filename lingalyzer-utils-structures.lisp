(in-package :org.kjerkreit.lingalyzer.utils)

(defstruct agent
  (name   "John Doe"  :type string :read-only t)
  (ngrams nil         :type list   :read-only t)
  (bday   "000-00-00" :type string)
  (dday   "000-00-00" :type string)
  (docs   nil         :type list))

(defstruct term
  (form   ""  :type string :read-only t)
  (ngrams nil :type list :read-only t)
  (count  0   :type integer))

(defstruct doc-container
  (name          "A tale" :type string :read-only t)
  (author        nil      :type )
  (genre         "-"      :type string)
  (versions      nil      :type list))

(defstruct doc-version
  (version       0        :type integer)
  (copied-by     nil      :type )
  (word-count    0        :type integer)
  (org-file      "-"      :type string)
  (org-file-hash "-"      :type string)
  (terms         nil      :type list))  