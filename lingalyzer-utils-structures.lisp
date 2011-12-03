(in-package :org.kjerkreit.lingalyzer.types)

(defstruct agent
  (name       "John Doe"     :type string)
  (bday       "000-00-00"    :type string)
  (dday       "000-00-00"    :type string)
  (authored   nil            :type list)
  (copied     nil            :type list))

(defstruct word-form
  (form       ""             :type string)
  (count      -1             :type integer))

(defstruct mdoc
  (name       "A tale"       :type string)
  (author     "key"          :type string)
  (genre      "Unknown"      :type string)
  (docs       nil            :type list))

(defstruct doc
  (mdoc       (make-array 1) :type array)
  (copied-by  "key"          :type string)
  (word-count -1             :type integer)
  (file       "bogus"        :type string)
  (file-hash  (make-array 1) :type array)
  (word-forms nil            :type array))