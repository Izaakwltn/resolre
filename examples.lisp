;;;; examples.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package :resolre)

(defun run-example (filename) ;filename in examples folder
  (run-file (asdf:system-relative-pathname "resolre" (format nil "~a~a" "examples/" filename))))

(defun run-multiply ()
  (run-example "multiply.rsr"))

(defun run-dumb-hello-world ()
  (run-example "dumb-hello-world.rsr"))

(defun run-hello-world ()
  (run-example "hello-world.rsr"))
