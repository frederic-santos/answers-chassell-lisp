;;; chapter8_intro.el --- Solutions to the exercises of chapter 8
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;
;;; § 8.7. Exercices
;;; (i) Write an interactive function that searches for a string.
;;; If the search finds the string, leave point after it, and
;;; display a message that says "Found!".
(defun perso-search (chaine)
  "Searches for CHAINE and leaves point after it (if found)."
  (interactive "sSearch: ")			; ask for a string
  (let
      ;; VARLIST:
      ((current-point (point)))
    ;; BODY:
    (search-forward chaine)
    (unless (equal current-point (point))
      (message "Found!"))))

;;; Another solution (simpler):
(defun perso-search (chaine)
  "Searches for CHAINE and leaves point after it (if found)."
  (interactive "sSearch: ")			; ask for a string
  (when (search-forward chaine nil t nil)
    (message "Found!")))
;;; In this variant, there is no error if the string is not found,
;;; and the "when" condition moves point for us as a side-effect.

;; (ii) Write a function that prints the third element of the kill
;; ring in the echo area (if any); if the kill ring does not contain
;; a third element, print an appropriate message. 
(defun third-killed-element ()
  "Prints the third element of the kill ring."
  (interactive)
  (if (>= (length kill-ring) 2)
      (message "%s" (nth 2 kill-ring))
    (message "The kill is not that long.")))

;;; The next sections are given here for my own reference.

;;;;;;;;;;;;;;;;;;;;;;
;;; § 8.1. zap-to-char
(defun perso-zap-to-char (arg char)
  "Kill up to and including the ARG-th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in current buffer.
Goes backward if ARG<0.
Error if CHAR is not found."
  ;; Get both a numeric prefix ARG (default: 1 if not supplied)
  ;; and a character CHAR:
  (interactive "p\ncZap to char: ")
  ;; Body:
  (kill-region
   (point) ; beginning of region to kill
   (progn
     (search-forward (char-to-string char) nil nil arg)
     (point)) ; end reg.
   ))

;; A comment (which also may be used as a test paragraph):
;; This might not be the case when R. Chassell wrote his book,
;; but the help and `search-forward' says that this function
;; "returns [the value of] point".
;; Consequently, the `progn' special form was maybe not mandatory
;; here (and in all cases, the function works without it).

;;;;;;;;;;;;;;;;;;;;;
;; § 8.2. kill-region
;; Instructions:
;; - trigger an error if mark is not set
;; - check whether the string exists (i.e., start != end): if it
;;   does not exist, do nothing.
;; - if the previous command was already a `kill-region',
;;   use `kill-append' to add the region to the last string
;;   saved in the kill ring
;; - if the previous command was not a `kill-region', use
;;  `kill-new' to add a new element in the kill ring
;; - if the buffer or region is read-only, use `copy-region-as-kill'
(defun simplified-kill-region (beg end)
  "Kill text between point (BEG) and mark (END).
This deletes the text from the buffer and saves it in the kill ring.
If buffer is read-only, barf and copies the region."
  (interactive (list (point) (mark)))
  ;; First trigger an error if the mark is not set:
  (unless end ; it is nil if mark is not set (cf. help of `mark')
    (error "The mark is not set!"))
  ;; BODY of function:
  (condition-case
      nil
      ;; Bodyform (executed if all goes well):
      (let 
	  ;; VARLIST, define string to kill:
	  ((string (buffer-substring beg end)))
	;; BODY:
	(when string ; if string is nil (empty), do nothing.
	  (if (eq last-command 'kill-region)
	      (kill-append string)
	    (kill-new string))))
    ;; Error-handler:
    (buffer-read-only ; the if part
     (barf-if-buffer-read-only)
     (copy-region-as-kill beg end) ; the then part
     (message "Read only text has been copied."))))
