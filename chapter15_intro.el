;;; chapter15_intro.el --- Solutions to the exercises of chapter 15
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 15. How to draw the graph?
;;; (i) Let's try the `insert-rectangle' function:
(insert-rectangle '("*" "*" "*"))*
				 *
                                 *
;;; This seems to be convenient.
;;; Tip: we can do M-: and then the previous sexp, to get that:
;;; *
;;; *
;;; *

;;; (ii) The `max' function:
(max 1 5 6 2)				; return max of its arguments
;; Be careful: this does not work with a list:
;; (max '(1 5 6 2))
;; Instead we must use `apply' in combination to `max':
(apply 'max '(1 5 6 2))

;;; (iii) How to fill each column of the graph?
(defvar graph-symbol "*"
  "String used as symbol in graph, usually an asterisk.")

(defvar graph-blank " "
  "String used as blank in graph, usually a blank space.")

(defun column-of-graph (max-height column-height)
  "Return list of strings that is one column of the graph.
It will include MAX-HEIGHT characters in total.
In particular, COLUMN-HEIGHT of them will be graph-symbols."
  (let ((counter 1)
	(result ()))
    (while (<= counter max-height)
      ;; Add the right character to the list:
      (if (<= counter column-height)
	  (setq result (cons graph-symbol result))
	(setq result (cons graph-blank result)))
      ;; Increment counter:
      (setq counter (1+ counter)))
    ;; Return list
    result))

;;; Test of the function:
(column-of-graph 5 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 15.1. The `graph-body-print' function
(defun graph-body-print (numbers-list)
  "Print a bar graph of the NUMBERS-LIST.
The numbers-list consists of the Y-axis values."
  (let ((height (apply 'max numbers-list))
	(symbol-width (length graph-blank))
	(from-position nil))
    (while numbers-list			; while not empty
      (setq from-position (point))
      (insert-rectangle (column-of-graph height (car numbers-list)))
      (goto-char from-position)
      (forward-char symbol-width)
      ;; Draw graph column by column
      (sit-for 1)
      ;; Shorten the list
      (setq numbers-list (cdr numbers-list)))
    ;; Place point for the X-axis labels
    (forward-line height)
    (insert "\n")))

;; Test of the function:
(graph-body-print '(1 2 3 4 6 4 3 5 7 6 5 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 15.2. The `recursive-graph-body-print' function
(defun recursive-graph-body-print (numbers-list)
  "Print a bar graph of the NUMBERS-LIST.
The numbers-list consists of the Y-axis values."
  (let ((height (apply 'max numbers-list))
	(symbol-width (length graph-blank))
	(from-position nil))
    (recursive-graph-body-print-internal
     numbers-list
     height
     symbol-width)))

(defun recursive-graph-body-print-internal (numbers-list height symbol-width)
  "Print a bar graph.
Used within recursive-graph-body-print function."
  (when numbers-list 			; if it is not empty
    (setq from-position (point))
    (insert-rectangle
     (column-of-graph height (car numbers-list)))
    (goto-char from-position)
    (forward-char symbol-width)
    (sit-for 1)
    (recursive-graph-body-print-internal
     (cdr numbers-list) height symbol-width)))

;; Test of the function:
(recursive-graph-body-print '(1 2 3 4 6 4 3 5 7 6 5 2 3))

;;;;;;;;;;;;;;;;;;;;
;;; § 15.4. Exercise
;;; Write a line graph version of the graph printing functions.
;;; Here I will implement only the 'while loop' version.
(defun line-graph-version (numbers-list)
  "Print an horizontal bar graph of NUMBERS-LIST."
  (let ((graph-width (apply 'max numbers-list)))
    ;; Begin by a newline:
    (forward-line)
    ;; Plot each line:
    (while numbers-list			; while not empty
      (apply 'insert
	     (reverse (column-of-graph graph-width (car numbers-list))))
      ;; Goto next line:
      (insert "\n")
      (forward-line)
      ;; Wait for each row:
      (sit-for 1)
      ;; Make the list shorter:
      (setq numbers-list (cdr numbers-list)))))

;; Test of the function:
(line-graph-version '(1 2 3 4 6 4 3 5 7 6 5))
