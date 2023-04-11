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
	   #:run-multiply))
