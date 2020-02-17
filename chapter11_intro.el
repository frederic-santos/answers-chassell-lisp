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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 11.1.3. A Loop with an Incrementing Counter
(defun count-pebbles (n)
  "Count necessary pebbles to build a triangle of N floors"
  (interactive "pNumber of floors: ")
  (let
      ;; VARLIST:
      ((counter 1)
       (result 0))
    ;; BODY:
    (while (<= counter n)
      (setq result (+ result counter))
      (setq counter (1+ counter)))
    (message "There are %s pebbles in total." result)))

;; Test of the function:
(count-pebbles 4)			; or C-u 4 M-x count-pebbles
(count-pebbles 7)			; or C-u 7 M-x count-pebbles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 11.1.4. Loop with a Decrementing Counter
(defun reverse-count-pebbles (n)
  "Count pebbles (still N floors) using a while loop and a decrementing counter."
  (interactive "pNumber of floors: ")
  (let
      ;; VARLIST:
      ((counter n)
       (result 0))
    ;; BODY:
    (while (>= counter 1)
      (setq result (+ result counter))
      (setq counter (- counter 1)))
    (message "There are %s pebbles in total." result)))

;; Test of the function:
(reverse-count-pebbles 4)
(reverse-count-pebbles 7)
