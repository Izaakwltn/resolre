;;;; mirefami.lisp (main)
;;;;
;;;; Copyright Izaak Walton (c) 2023

(in-package :resolre)

(defvar *print-buffer* "")

(defun new-session ()
  (setq *current-cell* (start-cell))
  (setq *indexed-commands* nil)
  (setq *print-buffer* ""))

(defun run-command (indexed-command)
  "Evaluates and runs Resolre command"
  (funcall (eval (second (assoc (second indexed-command) *symbol-table* :test #'string-equal)))))

(defun run-commands (indexed-commands) ; the recursive point where I nearly lost my mind
  "Cycles through a list of indexed Resolre commands." 
  (cond ((null indexed-commands)          
         *print-buffer*)                  
        ((string-equal (second (first indexed-commands)) "la")
         (loop-start (first (first indexed-commands))))
	((string-equal (second (first indexed-commands)) "si")
	 (loop-end (first (first indexed-commands))))
        (t (progn (run-command (first indexed-commands))
                  (run-commands (rest indexed-commands))))))

(defun run-file (filepath)
  (new-session)
  (index-file filepath)
  (run-commands *indexed-commands*))

  
