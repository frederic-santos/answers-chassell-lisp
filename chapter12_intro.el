;;; chapter12_intro.el --- Solutions to the exercises of chapter 12
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;;
;;; § 12.6. Exercises
;;; (i) Write a function to search for a regular expression that
;;; matches two or more blank lines in sequence.
(defun search-blank-lines ()
  "Search for two or more blank lines in sequence."
  (interactive)
  (re-search-forward "\\(^\n\\)\\{2,\\}"))

;;; Begin test of the function



;;; End test of the function
;;; Commentary: I'm not sure of this one.
;;; Note that "in a Lisp program, each '\' must be doubled".
;;; (See GNU Emacs manual, § 12.6, p. 104)

;;; (ii) Write a function to search for duplicated words.
(defun dup-words ()
  "Simplified research for duplicated words."
  (interactive)
  (push-mark)				; so as to come back easily
  (if (re-search-forward "\\(\\w+[\t\n ]+\\)\\1")
      (message "This is a duplicate.")
    (message "No duplicates found in this buffer.")))

;;; Test of duplicated duplicated words.

;;; The following sections are given for my own reference.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 12.3. forward-sentence
(defun perso-forward-sentence (&optional arg)
  "Simplified version of `forward-sentence'.
With prefix ARG, repeat. If negative, go backward."
  (interactive "p")
  ;; If arg is not supplied, set it to 1:
  (or arg (setq arg 1))
  ;; Begin body:
  (let
      ;; VARLIST:
      ((opoint (point)) 		; value of point before search
       (sentence-end (sentence-end)))
    ;; 1. Case with positive prefix:
    (while (> arg 0)
      (let
	  ;; Varlist:
	  ((par-end (save-excursion
		      (end-of-paragraph-text)
		      (point))))	; store the place where the current paragraph ends
	(if (re-search-forward sentence-end par-end t)
	    (skip-chars-backward " \t\n")
	  (goto-char par-end)))
      ;; Decrementing counter of while loop:
      (setq arg (1- arg)))
    (constrain-to-field nil opoint t)))

;; This a test paragraph.  A test paragraph!  Another one?
;; N.B.: Put a double space after each sentence.
