;;; chapter06_intro.el --- Solutions to the exercises of chapter 6
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;
;;; § 6.3. Exercise.
;; Write a function that will display the first 60 characters of the
;; current buffer, even if you have narrowed the buffer to its latter half
;; so that the first line is inaccessible.
;; Restore point, mark, and narrowing.
(defun first-sixty-chars ()
  "Display the first 60 characters of the current buffer (even if)
they are in a narrowed part."
  (interactive)
  (save-restriction
    (widen)
    (message "%s"
	     (buffer-substring-no-properties 1 61))))

;; Test this function:
(first-sixty-chars)

;;; The next section (in french) is for my own reference only.

;;;;;;;;;;;;;;;;;;;;
;;; § 6.2. what-line
;; [fr]
;; Dans un premier temps on pourrait se contenter de ceci :
(defun simple-what-line ()
  "Simplified version of what-line.
Unlike the 'true' what-line, this version only returns
the current line number of point in the (widened) buffer."
  (interactive) ; no argument
  ;; First widen the buffer, and then restore it in its
  ;; original state at the end of the function:
  (save-restriction
    ;; Widen the buffer if narrowed:
    (widen)
    ;; Get line number in widened buffer:
    (message "Line %d"
	     (+ 1 (count-lines 1 (point))))))

;; Mais on constate que la fonction ne marche pas bien.
;; L'exécuter avec le point au début d'une ligne, puis au milieu de la
;; même ligne. Commentaire ?
;; Explication : cf. l'aide de count-lines qui dit qu'il faut se situer
;; au début d'une ligne pour que ça marche. On est donc obligé d'ajouter
;; une instruction qui déplace le point au début d'une ligne.
;; [/fr]

(defun simple-what-line ()
  "Simplified version of what-line.
Unlike the 'true' what-line, this version only returns
the current line number of point in the (widened) buffer."
  (interactive) ; no argument
  ;; First widen the buffer, and then restore it in its
  ;; original state at the end of the function:
  (save-restriction
    ;; Widen the buffer if narrowed:
    (widen)
    ;; Move point to beginning of line:
    (save-excursion
      (beginning-of-line)
      ;; Get line number in widened buffer:
      (message "Line %d"
	       (+ 1 (count-lines 1 (point)))))))

