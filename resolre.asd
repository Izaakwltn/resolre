;;;; resolre.asd 
;;;;
;;;; Copyright Izaak Walton (C) 2023

(asdf:defsystem #:resolre
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A Brainfuck compiler using Solfege/Solresol syllables"
  :serial t
  :build-operation program-op
  :build-pathname "resolre"
  :entry-point "resolre::resolre"
  :components ((:module "src"
                :serial t
                :components ((:file "laresolre") ; package
                             (:file "larelasi")  ; cell list/array
                             (:file "resolremi") ; commands             
                             (:file "solrela-sisolla") ;lexical-analysis
                             (:file "mirefami") ; main
                             (:file "solsolredo") ; brainfuck conversions
                             (:file "soldosol"))) ; interpreter
	       (:module "examples"
                :serial t
		:components ((:file "examples")))))
