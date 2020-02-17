;;; chapter10_intro.el --- Solutions to the exercises of chapter 10
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;;
;;; § 10.3. Exercises
;;; Using nthcdr and car, construct a series of expressions to return
;;; the first, second, third and fourth elements of a list.
;; First I define a list:
(setq example '("one" "two" "three" "four" "five" "six" "seven"))
example					; check contents
;; Then I construct the "individual" expressions:
(car example)				; first element
(car (nthcdr 1 example))		; second element
(car (nthcdr 2 example))		; third element
(car (nthcdr 3 example))		; fourth element
;; I also can put them together with the "list" function:
(setq first-four-elements
      (list
       (car example)
       (car (nthcdr 1 example))
       (car (nthcdr 2 example))
       (car (nthcdr 3 example))))
first-four-elements			; okay!
