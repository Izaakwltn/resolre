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

(defvar *parsed-file* nil)

(defun parse-file (filename)
  (setq *parsed-file* (parse (uiop:read-file-string filename))))

(defun hello-world ()
  (parse-file (asdf:system-relative-pathname "resolre" "dumb-hello-world.ssrd")))

(defvar *symbol-table* '(("do" #'move-right)
                         ("re" #'move-left)
                         ("mi" #'incr)
                         ("fa" #'decr)
                         ("so" #'char-output)
                         ("la" #'char-input)))
                         ;("si" #'jump-forward)
                                        ;("ut" #'jump-back)))

;;;; starting stupid

(defvar *print-buffer* "")

(defun run-file (filepath)
  (new-session)
  (setq *print-buffer* "")
  (loop :for s :in (parse-file filepath)
        :do (funcall (eval (second (assoc s *symbol-table* :test #'string-equal))))
            :finally (return (reverse *print-buffer*))))

(defun run-dumb-hello-world ()
  (run-file (asdf:system-relative-pathname "resolre" "dumb-hello-world.ssrd")))
;(defvar *parsed-index* 0)


