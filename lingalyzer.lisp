;;
;; defpackage et. al. in use until I can get a hang on asdf
;;
(load "/usr/local/src/lingalyzer/defpackage.lisp")
(load "/usr/local/src/lingalyzer/lingalyzer-utils-structures.lisp")
(load "/usr/local/src/lingalyzer/lingalyzer-utils-functions.lisp")

(in-package :org.kjerkreit.lingalyzer)

(if (< (length sb-ext:*posix-argv*) 2)
    (progn
      (format t "~%*** ERROR *** Missing input file!~%")
      (sb-ext:quit))
    t)


;; Used until init function is ready
(defconst +path+ (cadr sb-ext:*posix-argv*))

(defun init ()
  (defparameter *path0* "lingalyzer/test-data/de-bello-gallico-01-01-latin-library.txt")
  (defparameter *path1* "lingalyzer/test-data/de-bello-gallico-01-01-wikisource.txt")

  (defparameter *authors* (make-hash-table :test 'equal))
  (defparameter *copyist* (make-hash-table :test 'equal))
  (defparameter *docs* (make-hash-table :test 'equal))
  (defparameter *terms* (make-hash-table :test 'equal))
  (defparameter *translators* (make-hash-table :test 'equal)))





(defparameter *terms-list* '("gallia" "est" "omnis" "divisa" "in" "partes" "tres" "quarum" "unam"
 "incolunt" "belgae" "aliam" "aquitani" "tertiam" "qui" "ipsorum" "lingua"
 "celtae" "nostra" "galli" "appellantur" "hi" "omnes" "lingua" "institutis"
 "legibus" "inter" "se" "differunt" "gallos" "ab" "aquitanis" "garumna"
 "flumen" "a" "belgis" "matrona" "et" "sequana" "dividit" "horum" "omnium"
 "fortissimi" "sunt" "belgae" "propterea" "quod" "a" "cultu" "atque"
 "humanitate" "provinciae" "longissime" "absunt" "minimeque" "ad" "eos"
 "mercatores" "saepe" "commeant" "atque" "ea" "quae" "ad" "effeminandos"
 "animos" "pertinent" "important" "proximique" "sunt" "germanis" "qui" "trans"
 "rhenum" "incolunt" "quibuscum" "continenter" "bellum" "gerunt" "qua" "de"
 "causa" "helvetii" "quoque" "reliquos" "gallos" "virtute" "praecedunt" "quod"
 "fere" "cotidianis" "proeliis" "cum" "germanis" "contendunt" "cum" "aut"
 "suis" "finibus" "eos" "prohibent" "aut" "ipsi" "in" "eorum" "finibus"
 "bellum" "gerunt" "eorum" "una" "pars" "quam" "gallos" "obtinere" "dictum"
 "est" "initium" "capit" "a" "flumine" "rhodano" "continetur" "garumna"
 "flumine" "oceano" "finibus" "belgarum" "attingit" "etiam" "ab" "sequanis"
 "et" "helvetiis" "flumen" "rhenum" "vergit" "ad" "septentriones" "belgae" "ab"
 "extremis" "galliae" "finibus" "oriuntur" "pertinent" "ad" "inferiorem"
 "partem" "fluminis" "rheni" "spectant" "in" "septentrionem" "et" "orientem"
 "solem" "aquitania" "a" "garumna" "flumine" "ad" "pyrenaeos" "montes" "et"
 "eam" "partem" "oceani" "quae" "est" "ad" "hispaniam" "pertinet" "spectat"
 "inter" "occasum" "solis" "et" "septentriones"))