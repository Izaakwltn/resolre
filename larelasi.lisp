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
