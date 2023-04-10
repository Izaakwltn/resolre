;;;; resolremi.lisp (commands)
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:resolre)


(defun move-right ()
  "Moves the pointer one cell to the right."
  (setq *current-cell* (if (cell-next *current-cell*)
                           (cell-next *current-cell*)
                           (new-cell *current-cell*
                                     (1+ (cell-index *current-cell*))
                                     0
                                     nil)))
  (setf (slot-value (cell-prev *current-cell*) 'next) *current-cell*))
  

(defun move-left ()
  "Moves the pointer one cell to the left."
  (setq *current-cell* (if (cell-prev *current-cell*)
                           (cell-prev *current-cell*)
                           *current-cell*))) ; ideally an error eventually probably

(defun incr ()
  "Increments the value of the current cell."
  (setf (slot-value *current-cell* 'value)
               (1+ (cell-value *current-cell*)))
  (if (cell-prev *current-cell*)
      (setf (slot-value (cell-prev *current-cell*) 'next)
            *current-cell*)))

(defun decr ()
  "Decrements the value of the current cell."
  (setf (slot-value *current-cell* 'value)
        (1- (cell-value *current-cell*)))
  (if (cell-prev *current-cell*)
      (setf (slot-value (cell-prev *current-cell*) 'next)
            *current-cell*)))

(defun char-output ()
  "Converts the numeric value of the current cell into a character."
  (setq *print-buffer* (format nil "~a~a"
                               *print-buffer*
                               (coerce (list (code-char (cell-value *current-cell*)))
                                            'string))))

(defun char-input (char) ; d
  "Takes character as input, assigns numeric code to current-cell"
  (setf (slot-value *current-cell* 'value)
        (char-code char)))

;;;; loop commands

(defvar *loopers* nil)

(defstruct looper start-index end-index)
                                        ; looper collects the index of the starting loop
                                        ; character ([ in brainfuck)
                                        ; as well as the index of the cell
                                        ; in which the command resides

(defun find-end-index (start-index)
  "Finds the si/] at the end of the loop"
  (first (find-if #'(lambda (x)
               (string-equal "ut" (second x)))
           (nthcdr start-index *indexed-commands*))))
          
(defun add-looper (start-index)
  "Add a new looper to the *loopers* list"
  (push (make-looper :start-index start-index
                     :end-index (find-end-index start-index))
        *loopers*))

(defun looper-nonexistent (index)
  (find-if #'(lambda (x)
               (equal index (looper-start-index x)))
           *loopers*))

(defun remove-looper ()
  (pop *loopers*))

(defun skip-loop (end-index)
  (run-commands (nthcdr (1+ end-index) *indexed-commands*)))

(defun loop-start (index)
  (cond ((looper-nonexistent index)
         (add-looper index))
        ((zerop (cell-value *current-cell*))
         (skip-loop (looper-end-index (pop *loopers*))))
        (t (run-commands (nthcdr (1+ index) *indexed-commands*)))))

(defun loop-end ()
  (loop-start (looper-start-index (first (last *loopers*)))))
         
;(defun loop-start (index)
 ; (cond ((zerop (cell-value *current-cell*))
  ;      (and (find-if-not #'(lambda (x)
   ;                   (equal index (looper-command-index x)))
    ;                    *loopers*)
     ;      (not (zerop (cell-value (add-looper index (cell-index *current-cell*)))
;
 ;                      (defun find-loop-end (index)
                         
;(defun loop-end (index)
 ; (if 
