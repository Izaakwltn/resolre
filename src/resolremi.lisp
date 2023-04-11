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

;;; "la"

(defun char-input (char) ; d
  "Takes character as input, assigns numeric code to current-cell"
  (setf (slot-value *current-cell* 'value)
        (char-code char)))

;;;; loop commands

;(defvar *loopers* nil)

;(defstruct looper start-index end-index)
                                        ; looper collects the index of the starting loop
                                        ; character ([ in brainfuck)
                                        ; as well as the index of the loop's end
                                        ; in which the command resides


;;; okay so, look for the next "ut"
;;; if you pass by a "si" before that "ut"
;;; look for the next next "ut", etc

;;; Finding the corresponding ut for a si (loop matching)

(defun find-si (index)
  "Finds the first 'si' after a given index"
  (first (find-if #'(lambda (x)
                         (string-equal "si" (second x)))
                  (nthcdr index *indexed-commands*))))

(defun find-ut (index)
  "Finds the first 'ut' after a given index"
  (first (find-if #'(lambda (x)
                         (string-equal "ut" (second x)))
                  (nthcdr index *indexed-commands*))))

(defun si-before-ut (index)
  "Checks whether you run into a 'si' before the next 'ut'"
  (if (find-si index)
      (< (find-si index)
         (find-ut index))
      nil))

(defun corresponding-ut (si)
  "Finds the corresponding end index for a given start index. (ut for si)"
  (if (si-before-ut (1+ si))
      (corresponding-ut (1+ (find-ut si)))
      (find-ut si)))

;;; Finding the corresponding si for an ut (loop beginning for a loop end)

(defun reverse-find-si (index)
  "Finds the last si before an index."
  (first (find-if #'(lambda (x)
		      (string-equal (second x) "si"))
		  (nthcdr (- (length *indexed-commands*)
			     index)
			  (reverse *indexed-commands*)))))

(defun reverse-find-ut (index)
  "Finds the last ut before an index."
  (first (find-if #'(lambda (x)
		      (string-equal (second x) "ut"))
		  (nthcdr (- (length *indexed-commands*)
			     index)
			  (reverse *indexed-commands*)))))

(defun ut-before-si (index)
  "Checks the indexed commands backwards to see whether you run into an ut before a si."
  (if (reverse-find-ut index)
      (> (reverse-find-ut index)
         (reverse-find-si index))
      nil))
  
(defun corresponding-si (ut)
  "Finds the corresponding start index (si) for a given end (ut)"
  (if (ut-before-si (1- ut))
      (corresponding-si (1- (reverse-find-si ut)))
      (reverse-find-si ut)))

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
      (loop-start (corresponding-si index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun add-looper (start-index)
 ; "Add a new looper to the *loopers* list"
  ;(push (make-looper :start-index start-index
   ;                  :end-index (corresponding-ut start-index))
    ;    *loopers*))

;(defun looper-exists (index)
 ; "Checks whether a looper exists given its index."
  ;(find-if #'(lambda (x)
   ;            (equal index (looper-start-index x)))
    ;       *loopers*))

;(defun remove-looper ()
 ; "Removes a looper from *loopers*"
  ;(pop *loopers*))

;(defun skip-loop ()
 ; (run-commands (nthcdr (1+ (looper-end-index (pop *loopers*))) *indexed-commands*)))

;(defun loop-start (index)
 ; (cond ((not (looper-exists index))
  ;       (progn (add-looper index)
   ;;             (run-commands (nthcdr (1+ index) *indexed-commands*))))
 ;       ((zerop (cell-value *current-cell*))
 ;        (skip-loop))
     ;   (t (run-commands (nthcdr (1+ index) *indexed-commands*)))))

;(defun loop-start (index)
 ; (cond ((zerop (cell-value *current-cell*))
  ;       (run-commands (nthcdr (1+ (corresponding-ut index)) *indexed-commands*)))
   ;     (t (progn (add-looper index)
    ;              (run-commands (nthcdr (1+ index) *indexed-commands*))))))

;(defun loop-end ()
 ; (if (zerop (cell-value *current-cell*))
  ;    (run-commands (nthcdr (1+ (looper-end-index (pop *loopers*))) *indexed-commands*))
   ;   (loop-start (looper-start-index (first *loopers*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; does a language even need loops
