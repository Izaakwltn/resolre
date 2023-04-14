;;;; examples.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package :resolre)

(defun run-example (filename) ;filename in examples folder
  (time (format nil "~%Example: ~a~%Output: ~a"
          filename
          (run-file
           (asdf:system-relative-pathname "resolre"
                                          (format nil "~a~a" "examples/" filename))))))

(defun run-print-7 ()
  (run-example "print-7.rsr"))

(defun run-dumb-hello-world ()
  (run-example "dumb-hello-world.rsr"))

(defun run-hello-world ()
  (run-example "hello-world.rsr"))

(defun run-addition ()
  (run-example "addition.rsr"))

(defun run-summoning ()
  (run-example "summoning.rsr"))
