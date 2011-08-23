(in-package :org.kjerkreit.lingalyzer.utils)

(defclass agent ()
  ((name   :initarg  :name
	   :initform "John Doe"
	   :type     string
	   :reader   ag-name)
   (ngrams :initarg  :ngrams
	   :initform '("$J" "Jo" "oh" "hn" "n " " D" "Do" "oe" "e$")
	   :reader   ag-ngrams
	   :type     list)
   (bday   :initarg  :bday
	   :initform "000-00-00"
	   :type     string
	   :reader   ag-bday)
   (dday   :initarg  :dday
	   :initform "000-00-00"
	   :type     string
	   :reader   ag-dday)
   (docs   :type     list
	   :accessor ag-docs)))

(defclass word-form ()
  ((form   :initarg  :form
	   :type     string
	   :reader   wf-form)
   (ngrams :initarg  :ngrams
	   :type     list
	   :reader   wf-ngrams)
   (count  :initform 0
	   :type     integer
	   :accessor wf-count)))

(defclass doc-container ()
  ((name      :initarg  :name
	      :type     string
	      :reader   dc-name)
   (author    :initarg  :author
	      :type     agent
	      :reader   dc-author)
   (genre     :initarg  :genre
	      :type     string
	      :reader   dc-genre)
   (versiosns :type     (array doc-version)
	      :accessor dc-vers))

(defclass doc-version ()
  ((vid       :initarg :vid
	      :reader  dv-vid
	      :type    integer)
   (copied-by :initarg :copied-by
	      :type    agent)
   (wc        :initarg :wc
	      :reader  dv-wf
	      :type    integer)
   (file      :initarg :file
	      :reader  dv-file
	      :type    string)
   (file-hash :initarg :file-hash
	      :reader  file-hash
	      :type    string)
   (wf nil    :type    (array word-form))))