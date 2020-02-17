;;; chapter11_intro.el --- Solutions to the exercises of chapter 11
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;; The following sections are given for my own reference.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 11.1.2. An Example: print-elements-of-list
(setq animals '(gazelle girafe lion tigre))
(defun print-elements-of-list (liste)
  "Print elements of LISTE by using a while loop."
  (while liste
    (print (car liste))
    (setq liste (cdr liste))))
(print-elements-of-list animals)
;; N.B.: we cannot use the "message" function here, because
;; at each iteration, one message would erase the previous one.
