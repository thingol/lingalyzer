(in-package :org.kjerkreit.lingalyzer.utils)

(defstruct agent
  (name          "John Doe"  :type string :read-only t)
  (bday          "000-00-00" :type string)
  (dday          "000-00-00" :type string)
  (authored      nil         :type list)
  (copied        nil         :type list))

(defstruct word-form
  (form          ""          :type string :read-only t)
  (count         -1          :type integer))

(defstruct meta-doc
  (name          "A tale"    :type string :read-only t)
  (author        nil         :type agent)
  (genre         "Unknown"   :type string)
  (docs          nil         :type list))

(defstruct doc
  (meta-doc      nil         :type meta-doc)
  (copied-by     nil         :type agent)
  (word-count    -1          :type integer)
  (org-file      "bogus"     :type string)
  (org-file-hash "bogus"     :type string)
  (word-forms    nil         :type list))