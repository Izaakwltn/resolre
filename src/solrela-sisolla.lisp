;;;; solrela-sisolla.lisp (Lexical Analysis)
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:resolre)

;;; probably best with alexa, but for now, good old recursion

;;; parsing

(defvar *test1* "do re mi fa so la si ut")

(defvar *test2* "doremifasolasiut")

(defvar *test3* "dontremindmefacesolittlelattetositout")

(defun parse (input-string)
  "Parses out solfege syllables from a string"
  (loop :with parsed := nil

        :for i :from 2 :to (length input-string)
        :do (if (typep (subseq input-string (- i 2) i)
                          'solfege)
                (setq parsed
                      (cons (subseq input-string (- i 2) i)
                            parsed)))
                  
        :finally (return (reverse parsed))))

;(defvar *parsed-file* nil)

(defun parse-file (filename)
  (parse (uiop:read-file-string filename)))

;(defun hello-world ()
 ; (index-file (parse-file (asdf:system-relative-pathname "resolre" "dumb-hello-world.ssrd"))))

(defvar *symbol-table* '(("do" #'move-right)
                         ("re" #'move-left)
                         ("mi" #'incr)
                         ("fa" #'decr)
                         ("so" #'char-output)
                         ("la" #'char-input)
                         ;("si" #'loop-start) ; hardwired, unecessary
                         ("ut" #'loop-end)))

;;;; indexing commands (for loop purposes

(defvar *indexed-commands* nil)

(defun index-parsed (parsed-list)
  (loop :for i :in parsed-list
        :for j :from 0 :to (1- (length parsed-list))
        :collect (list j i)))

(defun index-file (filename)
  (setq *indexed-commands* (index-parsed (parse-file filename))))

(defun find-indexed (n)
  (find-if #'(lambda (x)
               (equal (first x) n))
               *indexed-commands*))


