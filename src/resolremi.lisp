;;;; resolremi.lisp (commands)
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:resolre)

;;; Resolre commands

;;; "do"

(defun move-right ()
  "Moves the pointer/current-cell one cell to the right."
  (setq *current-cell* (if (cell-next *current-cell*)
                           (cell-next *current-cell*)
                           (new-cell *current-cell*
                                     (1+ (cell-index *current-cell*))
                                     0
                                     nil)))
  (setf (slot-value (cell-prev *current-cell*) 'next) *current-cell*))
  

;;; "re"

(defun move-left ()
  "Moves the pointer/current-cell one cell to the left."
  (setq *current-cell* (if (cell-prev *current-cell*)
                           (cell-prev *current-cell*)
                           *current-cell*))) ; ideally an error eventually probably,
					     ; for now the padded cell walls
;;; "mi"

(defun incr ()
  "Increments the value of the current cell."
  (setf (slot-value *current-cell* 'value)
               (1+ (cell-value *current-cell*)))
  (if (cell-prev *current-cell*)
      (setf (slot-value (cell-prev *current-cell*) 'next)
            *current-cell*)))

;;; "fa"

(defun decr ()
  "Decrements the value of the current cell."
  (setf (slot-value *current-cell* 'value)
        (1- (cell-value *current-cell*)))
  (if (cell-prev *current-cell*)
      (setf (slot-value (cell-prev *current-cell*) 'next)
            *current-cell*)))

;;; "so"

(defun char-output ()
  "Converts the numeric value of the current cell into a character."
  (setq *print-buffer* (format nil "~a~a"
                               *print-buffer*
                               (coerce (list (code-char (cell-value *current-cell*)))
                                            'string))))

;;; "la" - May reimplement, but for now, remove

;(defun char-input (char) ; d
 ; "Takes character as input, assigns numeric code to current-cell"
  ;(setf (slot-value *current-cell* 'value)
   ;     (char-code char)))

;;;; loop commands

;;; Finding the corresponding ut for a si (loop matching)

(defun find-la (index)
  "Finds the first 'la' after a given index"
  (first (find-if #'(lambda (x)
                         (string-equal "la" (second x)))
                  (nthcdr index *indexed-commands*))))

(defun find-si (index)
  "Finds the first 'si' after a given index"
  (first (find-if #'(lambda (x)
                         (string-equal "si" (second x)))
                  (nthcdr index *indexed-commands*))))

(defun la-before-si (index)
  "Checks whether you run into a 'la' before the next 'si'"
  (if (find-la index)
      (< (find-la index)
         (find-si index))
      nil))

(defun corresponding-si (la)
  "Finds the corresponding end index for a given start index. (si for la)"
  (if (la-before-si (1+ la))
      (corresponding-si (1+ (find-si la)))
      (find-si la)))

;;; Finding the corresponding si for an ut (loop beginning for a loop end)

(defun reverse-find-la (index)
  "Finds the last la before an index."
  (first (find-if #'(lambda (x)
		      (string-equal (second x) "la"))
		  (nthcdr (- (length *indexed-commands*)
			     index)
			  (reverse *indexed-commands*)))))

(defun reverse-find-si (index)
  "Finds the last si before an index."
  (first (find-if #'(lambda (x)
		      (string-equal (second x) "si"))
		  (nthcdr (- (length *indexed-commands*)
			     index)
			  (reverse *indexed-commands*)))))

(defun si-before-la (index)
  "Checks the indexed commands backwards to see whether you run into an ut before a si."
  (if (reverse-find-si index)
      (> (reverse-find-si index)
         (reverse-find-la index))
      nil))
  
(defun corresponding-la (si)
  "Finds the corresponding start index (si) for a given end (ut)"
  (if (si-before-la (1- si))
      (corresponding-la (1- (reverse-find-la si)))
      (reverse-find-la si)))

;;; Actual loop functions:

;;; "si"

(defun loop-start (index)
  "The beginning of a loop."
  (run-commands (nthcdr (1+ index) *indexed-commands*)))

;;; "ut"

(defun loop-end (index)
  "The end of a loop, if pointer is zero, keep calm and carry on."
  (if (zerop (cell-value *current-cell*))
      (run-commands (nthcdr (1+ index) *indexed-commands*))
      (loop-start (corresponding-la index))))
