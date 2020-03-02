;;; chapter13_intro.el --- Solutions to the exercises of chapter 13
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 13.1. The `count-words-example' function
(defun count-words-example (beg end)
  "Count words within the region defined by BEG and END.
This version has (intentionally) a small bug in particular cases."
  (interactive "r")
  (let
      ;; VARLIST:
      ((counter 0))
    ;; BODY:
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(re-search-forward "\\w+\\W*")
	(setq counter (1+ counter))))
    (cond
     ((= counter 0)
      (message "There are no words here."))
     ((= counter 1)
      (message "There is one word in the region."))
     ((> counter 1)
      (message "There are %d words in the region." counter)))))

;;; This is an example text with 8 words.
;;; One.
;;;               
;;;        One two three.

(defun count-words-example (beg end)
  "Count words within the region defined by BEG and END.
Complete version without bugs."
  (interactive "r")
  (let
      ;; VARLIST:
      ((counter 0))
    ;; BODY:
    (save-excursion
      (goto-char beg)
      (while
	  (and (< (point) end) (re-search-forward "\\w+\\W*" end t))
	(setq counter (1+ counter))))
    (cond
     ((= counter 0)
      (message "There are no words here."))
     ((= counter 1)
      (message "There is one word in the region."))
     ((> counter 1)
      (message "There are %d words in the region." counter)))))

;;; This is an example text with 8 words.
;;; One.
;;;               
