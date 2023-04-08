;;;; resolre.asd 
;;;;
;;;; Copyright Izaak Walton (C) 2023

(asdf:defsystem #:resolre
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A Brainfuck compiler using Solfege/Solresol syllables"
  :serial t
  :components ((:file "laresolre") ;package
               (:file "larelasi") ;list/catalog
               (:file "solfamifa-soldofa") ;solfege-type
               (:file "solrela-sisolla") ;lexical-analysis
               )) ;

                                       
