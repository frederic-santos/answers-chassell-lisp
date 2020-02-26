;;; chapter01_intro.el --- Solutions to the exercises of chapter 1
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;;
;;; § 1.11. Exercises

;;; Generate an error message by evaluating an appropriate symbol
;;; that is not within parenthses:
toto ; this is not an already-defined *variable*

;;; Generate an error message by evaluating an appropriate symbol
;;; that is between parenthses:
(toto) ; this is not an already-defined *function*

;;; Write an expression that prints a message in the echo area
;;; when evaluated:
(message "Hi there!")

;;; The next comments are not part of the exercises.
;;; They are given here for my own reference.

;;;;;;;;;;;;;;;;;;
;;; Creating lists
;; In Emacs Lisp, if we want to create a list where the elements won't
;; be evaluated, we can do:
'(rose violet daisy)
;; But sometimes we may want to build a list where each element is the
;; result of a given function, i.e. must be evaluated within the list.
;; We cand do this:
(list (point-min) (point-max))
;; This returns a list of two elements, respectively the values returned
;; by the evaluation of (point-min) and (point-max) instructions.
;; In fact, the first syntax with the single quote is just a shortcut
;; for the following instruction:
(quote (rose violet daisy))
