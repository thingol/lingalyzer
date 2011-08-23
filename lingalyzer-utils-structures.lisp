(in-package :org.kjerkreit.lingalyzer.utils)

(defstruct agent
  (name          "John Doe"  :type string :read-only t)
  (ngrams        '("$J" "Jo" "oh" "hn" "n " " D" "Do" "oe" "e$")
	                     :type list   :read-only t)
  (bday          "000-00-00" :type string)
  (dday          "000-00-00" :type string)
  (authored      nil         :type list)
  (copied        nil         :type list))

(defstruct term
  (form          ""          :type string :read-only t)
  (ngrams        nil         :type list   :read-only t)
  (count         0           :type integer))

(defstruct doc-container
  (name          "A tale"    :type string :read-only t)
  (author        nil         :type agent)
  (genre         "Unknown"   :type string)
  (versions      nil         :type list)))

(defstruct doc-version
  (version       -1          :type integer)
  (copied-by     nil         :type agent)
  (word-count    -1          :type integer)
  (org-file      "bogus"     :type string)
  (org-file-hash "bogus"     :type string)
  (word-forms    nil         :type list))