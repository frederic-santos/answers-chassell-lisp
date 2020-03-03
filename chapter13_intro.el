;;; chapter13_intro.el --- Solutions to the exercises of chapter 13
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;
;;; § 13.3. Exercise
;;; (i) Using a `while' loop, write a function to count the number of
;;; punctuations marks in a region (period, comma, semicolon, colon,
;;; exclamation mark, question mark).
(defun count-punct (beg end)
  "Count punctuation marks between BEG and END."
  (interactive "r")
  (let
      ;; VARLIST:
      ((counter 0))
    ;; BODY:
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
		  (re-search-forward "[.,;:!?]" end t))
	(setq counter (1+ counter)))
      (message "Total number: %d." counter))))

;;; A good (?) example text, with... wait, seven punct marks in total!
;;; A text with no marks at all
;;; (ii) Do the same recursively.

(defun count-punct (beg end)
  "Count punctuation marks between BEG and END."
  (interactive "r")
  (message "Counting...")
  (save-excursion
    (goto-char beg)
    (let
	;; VARLIST:
	((counter (count-punct-recursively end)))
      ;; BODY:
      (message "Total number: %d." counter))))

(defun count-punct-recursively (end)
  "Count punctuation marks from point to END."
  (if (and (< (point) end)
	   (re-search-forward "[.,;:!?]" end t))
      (1+ (count-punct-recursively end))
    0))

;;; The following sections are given for my own reference.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 13.2. Count words recursively
(defun count-words-example (beg end)
  "Count words within the region between BEG and END."
  (interactive "r")
  ;; 1. Set up appropriate conditions:
  (message "Counting words...")
  (save-excursion
    (goto-char beg)
  ;; 2. Count the words:
    (let
	;; VARLIST:
	((counter (recursive-count-words end)))
      ;; BODY:
      ;; 3. Send a message to the user:
      (cond
       ((= counter 0)
	(message "There are no words here."))
       ((= counter 1)
	(message "There is one word in the region."))
       ((> counter 1)
	(message "There are %d words in the region." counter))))))

(defun recursive-count-words (region-end)
  "Count words from point to REGION-END."
  ;; No need to make the function interactive here.
  ;; Do-again-test:
  (if (and (< (point) end)
	   (re-search-forward "\\w+\\W*" region-end t))
      ;; then:
      (1+ (recursive-count-words region-end))
    ;; else:
    0))

;;; This is an example text with 8 words.
;;; One.
;;;            
