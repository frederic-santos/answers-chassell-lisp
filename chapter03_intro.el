;;; chapter03_intro.el --- Solutions to the exercises of chapter 3
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;;
;;; § 3.12. Exercises
;; (i) Write a non-interactive function that doubles the value of its arg.
(defun double-value (arg)
   "Double value of numeric ARG."
   (message "The double of %d is %d"
	    arg (* 2 arg)))

;; Test: (double-value 5)

;; (ii) Make that function interactive:
(defun double-value-interactive (arg)
  "Double value of numeric ARG."
  (interactive "p") ; numeric prefix
  (message "The double of %d is %d"
	   arg (* 2 arg)))
;; execute C-u 5 M-x double-value-interactive
;; Note: if no prefix if supplied, its default value is 1:
;; M-x double-value-interactive

;; (iii) Write a function that tests whether the current value of
;; fill-column is greater than the argument passed to the function.
(defun test-fill-column (num)
  "Test whether NUM is greater or lower than `fill-column'."
  (interactive "p") ; numeric prefix
  (if (>= num fill-column)
      (message "%d is greater or equal than `fill-column'" num)
    (message "%d is lower than `fill-column'" num)))

;; Test: (test-fill-column 50)
;; Test: (test-fill-column 100)
;; Interactive test: C-u 50 M-x test-fill-column
