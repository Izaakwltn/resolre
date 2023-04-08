;;;; solfamifa-soldofa.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:resolre)

(setq *solfege-syllables* '("do" "re" "mi" "fa" "so" "la" "si" "ut"))

(defun solfege-p (x)
  (member x *solfege-syllables* :test #'string-equal))

(deftype solfege ()
  `(satisfies solfege-p)) 
