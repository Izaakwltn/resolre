;;;; larelasi.lisp (list/catalog)
;;;;
;;;; Copyright Izaak Walton (c) 2023

(in-package #:resolre)

;;; brainfuck originally had an indexed array, originally initialized at 30k cells long
;;; Resolre works through a doubly-linked list, and makes new cells as necessary

;;; Cells (nodes)
(defstruct cell prev index value next)

(defmethod print-object ((cell cell) stream)
  (print-unreadable-object (cell stream :type t)
    (with-accessors ((prev cell-prev)
                     (index cell-index)
                     (value cell-value)
                     (next cell-next))
        cell
      (format stream "~%Index: ~a~%Value: ~a~%" index value))))

(defun new-cell (prev index value next)
  (make-cell :prev prev
             :index index
             :value value
             :next next))

;;; Initializing the array

(defun start-cell ()
  (new-cell nil 0 0 nil))

(defvar *current-cell* (start-cell))

(defun new-session ()
  "Resets the array."
  (setq *current-cell* (start-cell)))

;;; list/array operations (brainfuck operations)

(defun move-right ()
  "Moves the pointer one cell to the right."
  (setq *current-cell* (if (cell-next *current-cell*)
                           (cell-next *current-cell*)
                           (new-cell *current-cell*
                                     (1+ (cell-index *current-cell*))
                                     0
                                     nil))))

(defun move-left ()
  "Moves the pointer one cell to the left."
  (setq *current-cell* (if (cell-prev *current-cell*)
                           (cell-prev *current-cell*)
                           *current-cell*))) ; ideally an error eventually probably

(defun incr ()
  "Increments the value of the current cell."
  (setf (slot-value *current-cell* 'value)
        (1+ (cell-value *current-cell*))))

(defun decr ()
  "Decrements the value of the current cell."
  (setf (slot-value *current-cell* 'value)
        (1- (cell-value *current-cell*))))

(defun char-output ()
  "Converts the numeric value of the current cell into a character."
  (setq *print-buffer* (cons (code-char (cell-value *current-cell*))
                             *print-buffer*)))

(defun char-input (char) ; d
  "Takes character as input, assigns numeric code to current-cell"
  (setf (slot-value *current-cell* 'value)
        (char-code char)))

;(defun loop-start (
