;;; chapter09_intro.el --- Solutions to the exercises of chapter 9
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;
;;; § 9.2. Exercice
;;; (i) Set flowers to violet and buttercup.
(setq flowers '(violet buttercup))
;;; (ii) Cons two more flowers on to this list and set this new
;;; list to "more-flowers".
(setq more-flowers (cons 'iris (cons 'poppy flowers)))
more-flowers				; (iris poppy violet buttercup)
;;; (iii) Set the CAR of flowers to a fish.
(setcar flowers 'tuno)
flowers
;;; (iv) What does the "more-flowers" list now contains ?
more-flowers				; (iris poppy tuno buttercup)
;;; Explanation:
;;; - setcar is destructive
;;; - more-flowers points (indirectly) to flowers. If flowers if modified,
;;; then more-flowers will be "dynamically" modified too.
;;; This is because more-flowers and flowers are pointers/addresses:
;;; see below.

;;; The next section is given here for my own reference.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; §9 How lists are implemented
;;; (i) Reminder:
;;; A cons consists of two pointers (car . cdr) to the places where the
;;; associated values are stored.
;;; (ii) Short story:
;;; "A list is implemented as either the atom nil or a cons cell whose
;;; cdr is a list."
;;; (iii) More comments:
;;; Basically, when we set a variable to a list with setq, this variable
;;; stores the adress of the first cons of the list.
;;; I.e., when we do (setq x '(1 2 3 4)), x is a pointer towards the
;;; first cons of '(1 2 3 4), which is a list stored at a given place
;;; in memory. Variables defined as lists are *pointers*, and lists themselves
;;; are series of pairs of pointers (conses).
;;; This is important! See code below.
(setq x '(1 2 3 4))		; define x
(setq y x)			; define y
(setcar y 9)			; modify CAR of y
y				; -> (9 2 3 4): y has changed (ok)
x				; -> (9 2 3 4): but x has changed too!
;;; What's happening?
;;; y points to the list referenced by symbol x, but this list exists only
;;; once in memory. x and y are the same object:
(eq x y)
;;; because x is a pointer towards a given constant, and y points towards this
;;; very same constant. x and y are pointers and point towards the same object:
;;; they are equal.
;;; Therefore, modifying x is also modifying y, and vice versa.
;;; This is list-specific, because lists are pointers. This is different for
;;; simpler values like characters:
(setq a "a")				; define a
(setq b a)				; define b
(setq b "b")				; modify value of b
b					; b has changed
a					; a is unchanged

;;; See also:
;;; https://emacs.stackexchange.com/questions/55475/whats-really-behind-an-assignment-in-emacs-lisp/
