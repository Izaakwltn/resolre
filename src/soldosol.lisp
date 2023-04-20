;;;; soldosol.lisp (interpreter)

(in-package :resolre)

;;; Maybe separate package eventually
;;; 

(defun print-current-cell ()
  (format nil "Cell-~a: ~a"
          (cell-index *current-cell*)
          (cell-value *current-cell*)))

(defun prompt-read (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defvar *commands-string* "")

(defun export-commands ()
  (write-line *commands-string*))

(defun prompt ()
  (let ((input (prompt-read (format nil "~a~%> " (print-current-cell)))))
    (cond ((or (string-equal input "exit")
               (string-equal input "quit"))
           (if (> (length *print-buffer*) 0)
               *print-buffer*
               (write-line "Fasidola! (Goodbye!)")))
          ((string-equal input "export")
           (progn (export-commands)
                  (prompt)))
          ((or (string-equal input "clear")
               (string-equal input "new-session"))
           (resolre))
          ((unfinished-loop-p (index-parsed (parse input)))
           (progn (incr-loop-indent)
                  (continue-loop input)))
          (t (progn (setq *indexed-commands* (index-parsed (parse input)))
                    (setq *commands-string* (format nil "~a ~a" *commands-string* input))
                    (run-commands *indexed-commands*)
                    (prompt))))))

(defun resolre ()
(new-session)
  (setq *commands-string* "")
  (setq *loop-indent* 0)
  (prompt))

;;; Handling loops
(defun interpreter-find-la (index indexed-commands)
  "Finds the first 'la' after a given index"
  (first (find-if #'(lambda (x)
                         (string-equal "la" (second x)))
                  (nthcdr index indexed-commands))))

(defun interpreter-find-si (index indexed-commands)
  "Finds the first 'si' after a given index"
  (first (find-if #'(lambda (x)
                         (string-equal "si" (second x)))
                  (nthcdr index indexed-commands))))

(defun interpreter-la-before-si (index indexed-commands)
  "Checks whether you run into a 'la' before the next 'si'"
  (if (and (interpreter-find-la index indexed-commands)
           (interpreter-find-si index indexed-commands))
      (< (interpreter-find-la index indexed-commands)
         (interpreter-find-si index indexed-commands))
      nil))

(defun interpreter-corresponding-si (la indexed-commands)
  "Finds the corresponding end index for a given start index. (si for la)"
  (if (interpreter-la-before-si (1+ la) indexed-commands)
      (interpreter-corresponding-si (1+ (interpreter-find-si la indexed-commands)) indexed-commands)
      (interpreter-find-si la indexed-commands)))

;(defun interpreter-la-before-si (index indexed-commands)
 ; "Checks whether you run into a 'la' before the next 'si'"
  ;(if (and (interpreter-find-la index indexed-commands)
   ;        (interpreter-find-si index indexed-commands))
    ;  (< (interpreter-find-la index indexed-commands)
     ;;    (interpreter-find-si index indexed-commands))
      ;nil))

(defun la-count (indexed-commands)
  (length (remove-if-not #'(lambda (x)
                             (string-equal (second x) "la"))
                         indexed-commands)))


(defun si-count (indexed-commands)
  (length (remove-if-not #'(lambda (x)
                             (string-equal (second x) "si"))
                         indexed-commands)))


(defun unfinished-loop-p (indexed-commands) ;might be useful for other errors
  "Checks whether a list of indexed commands have an unfinished loop."
  (if (remove-if-not #'(lambda (x)
                         (string-equal (second x) "la"))
                     indexed-commands)
      (loop :with bool := t
            :for c :in (mapcar #'first
                               (remove-if-not
                                #'(lambda (x)
                                    (string-equal (second x) "la"))
                                indexed-commands))
            :if (not (interpreter-corresponding-si c indexed-commands))
                :return t 
            :finally (return nil))
      nil))

;(defun another-unfinished-loop-p (indexed-commands)
 ; (if (remove-if-not #'(lambda (X)
  ;                       (string-equal (second x) "la"))
   ;                  (rest indexed-commands))
    ;  (loop :with bool := t
     ;       :for c :in (mapcar #'first
      ;                         (remove-if-not
       ;                         #'(lambda (x)
        ;                            (string-equal (second x) "la"))
         ;                       indexed-commands))
          ;  :if (not (interpreter-corresponding-si c indexed-commands))
 ;               :return t 
           ; :finally (return nil))))
;
;;; Currently doesn't support nested loops..... not optimal
(defvar *loop-indent* 0)

(defun loop-indent (n)
  (if (zerop n)
      " "
      (format nil "...~a" (loop-indent (1- n)))))

(defun incr-loop-indent ()
  (setq *loop-indent* (1+ *loop-indent*)))

(defun decr-loop-indent ()
  (setq *loop-indent* (1- *loop-indent*)))

(defun reset-loop-indent ()
  (setq *loop-indent* 0))

(defun continue-loop (prev-input)
  (let* ((new-input (prompt-read (loop-indent *loop-indent*)))
         (input (format nil "~a~a" prev-input new-input))
         (parsed (index-parsed (parse input))))
    (cond ((or (string-equal new-input "exit")
               (string-equal new-input "quit"))
           (write-line "Fasidola! (Goodbye!)"))
          ((unfinished-loop-p parsed)
           (if (> (- (la-count parsed)
                     (si-count parsed))
                  *loop-indent*)
               (progn (incr-loop-indent)
                      (continue-loop input))
               (continue-loop input)))
          ((string-equal new-input "si")
           (if (unfinished-loop-p parsed)
               (progn (decr-loop-indent) ; this isn't working for some reason
                      (continue-loop input))
               (progn (setq *indexed-commands* (index-parsed (parse input)))
                      (reset-loop-indent)
                      (setq *commands-string* (format nil "~a ~a" *commands-string* input))
                      (run-commands *indexed-commands*)
                      (prompt)))))))

