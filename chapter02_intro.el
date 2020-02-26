;;; chapter02_intro.el --- Solutions to the exercises of chapter 2
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;
;;; § 2.5. Exercise
;; Find a file with which you are working and move towards its middle.
;; Find its buffer name, file name, length, and your position in the file.

;; Evaluate manually those expressions after switching to a given buffer:
(buffer-name)
(buffer-file-name)
(buffer-size)
(point)

;; Bonus: how to move point right in the middle of a given buffer?
;; ('middle' is defined in terms of number of characters here)
(goto-char (/ (+ (point-min) (point-max)) 2))
