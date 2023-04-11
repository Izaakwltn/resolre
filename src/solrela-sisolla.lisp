;;;; solrela-sisolla.lisp (Lexical Analysis)
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:resolre)

;;; Declaring the solfege type

(setq *solfege-syllables* '("do" "re" "mi" "fa" "so" "la" "si" "ut"))

(defun solfege-p (x)
  (member x *solfege-syllables* :test #'string-equal))

(deftype solfege ()
  `(satisfies solfege-p))

;;; Parsing Resolre commands

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

(defun parse-file (filepath)
  "Parses Resolre commands from a given file."
  (parse (uiop:read-file-string filepath)))

;;; Correlating Resolre commands with functions

(defvar *symbol-table* '(("do" #'move-right)
                         ("re" #'move-left)
                         ("mi" #'incr)
                         ("fa" #'decr)
                         ("so" #'char-output)
                         ("la" #'char-input)))
                         ;("si" #'loop-start) ; hardwired because these require an index number
                         ;("ut" #'loop-end)))

;;; Indexing commands (for loop purposes)
;;;
;;; Each Resolre command is given an index number

(defvar *indexed-commands* nil)

(defun index-parsed (parsed-list)
  "Takes a list of parsed Resolre commands, applies index value."
  (loop :for i :in parsed-list
        :for j :from 0 :to (1- (length parsed-list))
        :collect (list j i)))

(defun index-file (filepath)
  "Takes a path/file, populates *indexed-commands* with its indexed and parsed elements."
  (setq *indexed-commands* (index-parsed (parse-file filepath))))

(defun find-command (index)
  "Finds a command given its index."
  (find-if #'(lambda (x)
               (equal (first x) index))
               *indexed-commands*))


