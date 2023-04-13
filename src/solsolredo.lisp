;;;; src/solsolredo.lisp (brainfuck)
;;;;
;;;; Copyright Izaak Walton (c) 2023

(in-package :resolre)

;;; functions for converting to and from brainfuck

;;; Since Resolre only has 7 commands, ommitting char-input, converting from resolre
;;; to brainfuck works all the time but converting the other only works without ','
(defvar *rsr-bf-table* '(("do" ">")
                         ("re" "<")
                         ("mi" "+")
                         ("fa" "-")
                         ("so" ".")
                         ("la" "[")
                         ("si" "]")))

;;; converting resolre to brainfuck

(defun convert-to-bf (command)
  (second (assoc command *rsr-bf-table* :test #'string-equal)))

(defun convert-commands-to-bf (commands-list)
  (mapcar #'convert-to-bf commands-list))

(defun convert-string-to-bf (rsr-string)
  (format nil "~{~a~}" (convert-commands-to-bf (parse rsr-string))))

(defun convert-file-to-bf (rsr-filepath bf-filepath)
  (let ((commands (convert-commands-to-bf (parse-file rsr-filepath))))
    (with-open-file (str bf-filepath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "~{~a~}" commands))))

;;; Converting from brainfuck to resolre

(defvar *brainfuck-characters* '(">" "<" "+" "-" "." "[" "]"))

(defun parse-bf (bf-string)
  (loop :with parsed := nil

        :for i :from 1 :to (length bf-string)
        :do (let ((c (subseq bf-string (1- i) i)))
              (cond ((member c *brainfuck-characters* :test #'string-equal)
                     (setq parsed (cons c parsed)))
                    ((string-equal c ",")
                     (return "ERROR- INCOMPATIBLE, NO RESOLRE CHARACTER INPUT"))))
        :finally (return (reverse parsed))))

(defun parse-bf-file (filepath)
  (parse-bf (uiop:read-file-string filepath)))

(defun convert-to-rsr (bf-command)
  (first (find-if #'(lambda (x)
                      (string-equal (second x) bf-command))
                  *rsr-bf-table*)))

                
(defun convert-commands-to-rsr (commands-list)
  (mapcar #'convert-to-rsr commands-list))

(defun convert-string-to-rsr (bf-string)
  (format nil "~{~A~}" (convert-commands-to-rsr (parse-bf bf-string))))
  
(defun convert-file-to-rsr (bf-filepath rsr-filepath)
  (let ((commands (convert-commands-to-rsr (parse-bf-file bf-filepath))))
    (with-open-file (str rsr-filepath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "~{~a~}" commands))))
