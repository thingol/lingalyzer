(in-package :org.kjerkreit.lingalyzer.utils)

(defstruct author
  (name  "John Doe"  :type string :read-only t)
  (b-day "000-00-00" :type string :read-only t)
  (d-day "000-00-00" :type string :read-only t)
  (docs  nil         :type list))

(defstruct term
  (form  "" :type string)
  (count 0  :type integer))

(defstruct copyist
  (name     "John Doe"  :type string :read-only t)
  (b-day    "000-00-00" :type string :read-only t)
  (d-day    "000-00-00" :type string :read-only t)
  (docs     nil         :type list)
  (employer ""          :type string :read-only t))

(defstruct doc
  (name          "A tale" :type string)
  (name-hash     "-"      :type string)
  (author        nil      :type author)
  (genre         "-"      :type string)
  (version       0        :type integer)
  (copied-by     nil      :type copyist)
  (word-count    0        :type integer)
  (org-file      "-"      :type string)
  (org-file-hash "-"      :type string)
  (terms         nil      :type list))

(defstruct translator
  (name     "John Doe"  :type string :read-only t)
  (b-day    "000-00-00" :type string :read-only t)
  (d-day    "000-00-00" :type string :read-only t)
  (docs     nil         :type list)
  (employer "John Doe"  :type string :read-only t))
  