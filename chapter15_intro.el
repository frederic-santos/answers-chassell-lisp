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
*
*
*

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
