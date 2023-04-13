;;;; laresolre.lisp (package)
;;;;
;;;; Copyright Izaak Walton (C) 2023

(defpackage resolre
  (:documentation "A Brainfuck compiler using Solfege syllables")
  (:use #:cl)

  (:export #:run-commands
	   #:run-file)

  (:export #:run-hello-world
	   #:run-dumb-hello-world
	   #:run-multiply)
  
  ;;; solsolredo: resolre-brainfuck conversions
  (:export #:convert-string-to-bf
           #:convert-string-to-rsr
           #:convert-file-to-bf
           #:convert-file-to-rsr))
