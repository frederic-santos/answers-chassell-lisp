;;; chapter11_intro.el --- Solutions to the exercises of chapter 11
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;;
;;; § 11.4. Exercises
;;; (i) Write a function similar to triangle (a.k.a., `count-pebbles' in
;;; this file) in which each row has a value which is the square of the
;;; row number. Use a while loop.
(defun sum-of-squares (n)
  "Computes the sum of the (squared) first N integers."
  (interactive "nSum until n: ")
  (let
      ;; VARLIST:
      ((somme 0)
       (compteur 1))
    ;; BODY:
    (while (<= compteur n)
      (setq somme (+ (* compteur compteur) somme))
      (setq compteur (1+ compteur)))
    somme))

;; Test of the function:
(sum-of-squares 4)			; 30: ok!

;;; (ii) Write a function similar to triangle that multiplies instead of
;;; adds the values.
(defun prod-of-values (n)
  "Computes the product of the first N integers."
  (interactive "nProd until n: ")
  (let
      ;; VARLIST:
      ((res 1)
       (compteur 1))
    ;; BODY:
    (while (<= compteur n)
      (setq res (* res compteur))
      (setq compteur (1+ compteur)))
    res))

;; Test of the function:
(prod-of-values 5)			; 120: ok!

;;; (iii) Rewrite the two previous functions recursively.
(defun recursive-prod (n)
  "Use recursion to compute the product of the first N integers."
  (interactive "nProd until n: ")
  (if (<= n 1)
      1
    (* n (recursive-prod (- n 1)))))

;; Test of the function:
(recursive-prod 5)			; 120: ok!

(defun recursive-sum-of-squares (n)
  "Computes the sum of the (squared) first N integers."
  (interactive "nSum until n: ")
  (if (= n 0)
      0
    (+ (* n n) (recursive-sum-of-squares (- n 1)))))

(recursive-sum-of-squares 5)		; 55: ok!

;;; (iv) Rewrite the same functions using `cond'.

;;; (v) [I add this one] Rewrite `prod-of-values' using `dotimes'.
(defun prod-with-dotimes (n)
  "Computes the product of the first N integers using `dotimes'."
  (interactive "nProd until n: ")
  (let
      ;; VARLIST:
      ((result 1))
    ;; BODY:
    ;; reminder: k ranges from 0 (inclusive) to n (exclusive)
    ;; in a dotimes loop.
    (dotimes (k n result)
      (setq result (* result (1+ k))))
    result))

;; Test of the function:
(prod-with-dotimes 5)			; 120: ok!

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 11.3.3. Recursion with a list
(setq animals '(loup poule lapin renard))
(defun print-list-recursively (liste)
  "Print elements of LISTE using recursive programming."
  (when liste
    (print (car liste))
    (print-list-recursively (cdr liste))))
(print-list-recursively animals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 11.3.4. Recursion in place of a counter
(defun count-pebbles-recursively (n)
  "Count necessary pebbles to build a triangle with N floors.
Uses recursive programming."
  (interactive "nNumber of floors: ")
  (if (equal n 1)
      1
    (+ n (count-pebbles-recursively (- n 1)))))

;; Test of the function:
(count-pebbles-recursively 4)		; 10: ok
(count-pebbles-recursively 7)		; 28: ok

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 11.3.5. Recursion example using cond
(defun count-pebbles-cond (n)
  "Yet another version of count-pebbles."
  (interactive "nNumber of floors: ")
  (cond
   ((<= n 0) 0)
   ((= n 1) 1)
   ((> n 1) (+ n (count-pebbles-cond (1- n))))))

;; Test of the function:
(count-pebbles-cond 4)			; 10: ok
(count-pebbles-cond 7)			; 28: ok

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 11.3.6. Recursive patterns
;;; Example of pattern "every":
(defun square-each (numbers-list)
  "Return a list made of the squared elements of NUMBERS-LIST."
  (when numbers-list
    (cons
     (* (car numbers-list) (car numbers-list))
      (square-each (cdr numbers-list)))))

;; Test of the function:
(setq x '(1 2 3 4 5))
(square-each x)

;;; Example of pattern "accumulate":
(defun sum (x)
  "Compute the sum of the numeric elements of X."
  (if x
      (+ (car x) (sum (cdr x)))
    0))

;; Test of the function:
(setq x '(1 2 3 4 5))
(sum x)
