;;;; larelasi.lisp (list/catalog)
;;;;
;;;; Copyright Izaak Walton (c) 2023

(in-package #:resolre)


;;; Brainfuck originally had an indexed array, initialized at 30k cells long.
;;; Resolre works through a doubly-linked list, and makes new cells as necessary.

;;; One potential quirk is that at cell 0, if you attempt to "re"/move back one cell
;;; it will instead stay at cell 0. I may switch this for an error at some point,
;;; but for now, like a filmic mental institution, I've settled on a padded cell wall.


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
  "Makes a new cell."
  (make-cell :prev prev
             :index index
             :value value
             :next next))

;;; Initializing the array

(defun start-cell ()
  "A blank starting cell for the array."
  (new-cell nil 0 0 nil))

(defvar *current-cell* (start-cell))
