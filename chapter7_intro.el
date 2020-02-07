;;; chapter7_intro.el --- Solutions to the exercises of chapter 7
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;
;;; § 7.7. Exercise.
;; Construct a list of four birds by evaluating several expressions
;; with cons:
(setq animals (cons 'corbeau
		    (cons 'choucas '(pie geai))))
;; Find out what happens when you cons a list onto itself:
(cons animals animals)
;; Replace the first element of the list of four birds with a
;; fish. Replace the rest of that list with a list of other fish.
(setcar animals 'truite)
animals
(setcdr animals '(ablette goujon carpe perche))
animals

;;; The next sections are given here for my own reference.

;;;;;;;;;;;;;;;;;;;;;;
;;; § 7.1. car and cdr
;; Define a list:
(setq mylist '(rose violet daisy buttercup))
;; Its car is its first element:
(car mylist)
;; Its cdr is the rest of its contents:
(cdr mylist)

;;;;;;;;;;;;;;;
;;; § 7.2. cons
;; cons builds a list from a CAR and a CDR:
(cons 'pine '(fir oak maple)) ; cf. help of cons
(cons 'buttercup ())
(cons 'buttercup nil) ; the same thing
;; If it has one single element, the CDR component should be
;; written as a list all the same:
(cons 'buttercup '(daisy))
;; § 7.2.1. Length of a list:
(length mylist)
(length '(buttercup))
(length 'buttercup) ; error: this is not a list!
(length nil) ; nil = empty list

;;;;;;;;;;;;;;;;;
;;; § 7.3. nthcdr
;; Returns nested CDRs, as would do those commands:
(cdr '(a b c d))
(cdr (cdr '(a b c d)))
;; Same result with nthcdr:
(nthcdr 2 '(a b c d))
;; One more recursive level:
(nthcdr 3 '(a b c d))

;;;;;;;;;;;;;;
;;; § 7.4. nth
;; Returns the n-th element of a list:
(nth 3 '(a b c d))
;; /!\ The first index is 0, not 1!
;; nth may be seen as a way of returning the CAR of the
;; result from nthcdr.

;;;;;;;;;;;;;;;;;
;;; § 7.5. setcar
;; setcar changes the value of the CAR of a given list:
mylist ; recall this list defined above
(setcar mylist 'flowers)
mylist ; mylist has *changed*  (setcar is "destructive")

;;;;;;;;;;;;;;;;
;; § 7.6. setcdr
;; This is the same thing as setcar, but for the CDR part:
mylist
(setcdr mylist '(ploup plip plop))
mylist
;; The CDR can even be of a different length:
(setcdr mylist '(iris))
mylist
