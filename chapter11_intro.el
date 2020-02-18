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
  (interactive "p")
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
  (interactive "nNumber of floors: ")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 11.2. dolist and dotimes
;;; Exercise: reverse a list using a while loop.
(defun reverse-list-with-while (liste)
  "Reverse the elements of LISTE using a while loop."
  (let
      ;; VARLIST:
      ((result ()))
    ;; BODY:
    (while liste
      (setq result (cons (car liste) result))
      (setq liste (cdr liste)))
    result))

;; Test of the function:
(setq animals '(loup renard poule))
(reverse-list-with-while animals)

;;; dolist:
(defun reverse-list-with-dolist (liste)
  "Reverse LISTE using dolist."
  (let
      ((result ()))
    (dolist (element liste result)
      (setq result (cons element result)))
    result))

;; Test of the function:
(setq animals '(loup renard poule))
(reverse-list-with-dolist animals)

;;; dotimes:
(defun decreasing-numbers (n)
  "Display the N first integers in decreasing order.
(N first integers range from 0 to N-1.)"
  (interactive "nNumber of integers: ")
  (let
      ;; VARLIST:
      ((result ()))
    ;; BODY:
    (dotimes (number n result)
      (setq result
	    (cons number result)))
  (message "%s" result)))

;; Test of the function:
(decreasing-numbers 5)

;;; Yet another version of count-pebbles:
(defun count-pebbles-with-dotimes (n)
  "Triangle with N floors using dotimes macro."
  (interactive "nNumber of floors:")     ; optional
  (let
      ;; VARLIST:
      ((result 0))
    ;; BODY:
    (dotimes (floor n result)
      (setq result (+ result (1+ floor)))) ; (1+ floor) because it counts starting from 0, not 1
    (message "There are %s pebbles in total." result)))

;; Test of the function:
(count-pebbles-with-dotimes 4)			; 10: ok!
(count-pebbles-with-dotimes 7) 			; 28: ok!
