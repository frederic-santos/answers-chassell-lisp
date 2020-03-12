;;; chapter14_intro.el --- Solutions to the exercises of chapter 14
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 14.3 The `count-words-in-defun' function
;; This function does really do the job, but is not interactive:
(defun count-words-in-defun ()
  "Count words and symbols within a defun."
  ;; Move point at the beginning of the defun:
  (beginning-of-defun)
  ;; Local variables:
  (let
      ;; VARLIST:
      ((count 0)
       (end (save-excursion
	      (end-of-defun)
	      (point))))
    ;; BODY:
    ;; Count words and symbols:
    (while (and
	    (< (point) end)
	    (re-search-forward
	     "\\(\\w\\|\\s_\\)+[^ \t\n]*[ \t\n]" end t))
      (setq count (1+ count)))
    ;; Return the number of words:
    count))

;; Here is a wrapper to call the function interactively:
(defun count-words-defun ()
  "Number of words and symbols within a defun."
  (interactive)
  (message "Counting words and symbols...")
  (let
      ;; VARLIST:
      ((result (count-words-in-defun)))
    ;; BODY: return results with correct messages.
    (cond
     ((= result 0) (message "No words here."))
     ((= result 1) (message "One word here."))
     ((> result 1) (message "%d words here." result)))))


;; Let's bind this function to `C-c =' :
(global-set-key "\C-c=" 'count-words-defun)

;; Test: the following defun should have 10 counted words.
(defun multiply-by-seven (number)
  "Multiply NUMBER by seven."
  (* 7 number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 14.6. The `lengths-list-file' function
;; A first draft:
(defun draft-lengths-list-file (filename)
  "Return a list with the number of symbols of each defun in FILENAME."
  ;; First display a welcoming message:
  (message "Counting words in file %s" filename)
  ;; Count words in each defun:
  (save-excursion
    (set-buffer (find-file-noselect filename))
    (widen)				; just a precaution
    (goto-char (point-min))
    (let
	;; VARLIST:
	((counts ())
	 (end (point-max)))
      ;; BODY:
      (while (and (< (point) end)
		  (re-search-forward "^(defun)" nil t))
	(setq counts (cons (count-words-defun) counts)))
      ;; Return the result:
      counts)))

;; Comment: the previous version does not kill the buffer at the end.
;; Since this function will be used on many files, this a problem.
;; The problem is fixed in this version:

(defun lengths-list-file (filename)
  "Return a list with the number of symbols of each defun in FILENAME."
  ;; First display a welcoming message:
  (message "Counting words in file %s" filename)
  ;; Count words in each defun:
  (save-excursion
    (let
	;; VARLIST:
	((buffer (find-file-noselect filename))
	 (counts ())
	 (end (point-max)))
      ;; BODY:
      (set-buffer buffer)
      (widen)				; just a precaution
      (goto-char (point-min))
      (while (and (< (point) end)
		  (re-search-forward "^(defun" nil t))
	(setq counts (cons (count-words-in-defun) counts)))
      ;; Kill the buffer, which is now useless:
      (kill-buffer buffer)
      ;; Return the result:
      counts)))

;; Test the function:
(lengths-list-file "./chapter13_intro.el") ; Yay, it works !

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 14.7. Count words in defuns in different files
(defun lengths-list-many-files (list-of-files)
  "Return a list of lengths of defuns in LIST-OF-FILES."
  (let
      ;; Initialize the list which will be returned:
      ((lengths-list ()))
    ;; Explore each file of the list and count the words in its defuns:
    (while list-of-files		; while it's non-empty
      (setq lengths-list
	    (append lengths-list
	     (lengths-list-file (car list-of-files))))
      ;; Make list shorter (so that the while loop can stop):
      (setq list-of-files (cdr list-of-files)))
    ;; Return result:
    lengths-list))

;; Test of the function:
(lengths-list-file "chapter12_intro.el")
(lengths-list-file "chapter13_intro.el")
(lengths-list-many-files '("chapter12_intro.el" "chapter13_intro.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 14.8. Recursively count words in different files
(defun recursive-lengths-list-many-files (list-of-files)
  "Return a list of lengths of defuns in LIST-OF-FILES."
  (if list-of-files			; if not empty
      (append				; add (to one single list)
       (lengths-list-file (car list-of-files))
       (recursive-lengths-list-many-files (cdr list-of-files)))))

(recursive-lengths-list-many-files '("chapter12_intro.el" "chapter13_intro.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 14.9.2. Making a list of files
(defun files-in-below-directory (directory)
  "List the .el files in DIRECTORY and its sub-directories."
  (interactive "DChoose directory: ")
  (let
      ;; Initialize variables:
      (result				; bound to nil
       (files (directory-files-and-attributes directory t)))
    ;; Explore list of files:
    (while files			; while not empty
      (cond
       ;; If it is "." or ".." (i.e., ends by "."), ignore it:
       ((equal "."
	       (substring (car (car files)) -1))
	())
       ;; Otherwise, if the file ends by ".el", add it to the list:
       ((equal ".el" (substring (car (car files)) -3))
	(setq result (cons (car (car files)) result)))
       ;; Otherwise, if it is a folder, explore it:
       ((eq t (car (cdr (car files))))
	(setq result
	      (append
	       (files-in-below-directory
		(car (car files)))
	       result))))
      ;; Shorten the list:
      (setq files (cdr files)))
    ;; Return result:
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 14.9.3 Counting function definitions
(defvar top-of-ranges
  (number-sequence 10 200 10)
  "List specifiying ranges for `defuns-per-range'.")

(defun defuns-per-range (sorted-lengths top-of-ranges)
  "SORTED-LENGTHS defuns in each TOP-OF-RANGES range."
  (let ((top-of-range (car top-of-ranges)) ; upper bound
	(number-within-range 0)
	(defuns-per-range-list ()))

    ;; Outer loop: explore ranges (upper bounds)
    (while top-of-ranges 		; while not empty

      ;; Inner loop: explore defun lengths
      (while (and
	      (car sorted-lengths) 	; not nil, i.e. list not empty
	      (< (car sorted-lengths) top-of-range))

	;; Count number of defuns within current range
	(setq number-within-range (1+ number-within-range))
	(setq sorted-lengths (cdr sorted-lengths)))

	;; Exit inner loop
	(setq defuns-per-range-list
	      (cons number-within-range defuns-per-range-list))
	(setq number-within-range 0)	; reset count to 0

	;; Move to next range
	(setq top-of-ranges (cdr top-of-ranges))
	(setq top-of-range (car top-of-ranges)))

    ;; Exit outer loop.
    ;; Count number of defuns > largest top-of-range value
    (setq defuns-per-range-list
	  (cons
	   (length sorted-lengths)
	   defuns-per-range-list))

    ;; Return the result (sorted)
    (nreverse defuns-per-range-list)))

;;; Test of the function:
(setq sorted-lengths
      (sort
       (lengths-list-many-files
	(files-in-below-directory
	 "~/PACEA_MyCore/Autoformation/Exercism/emacs-lisp/"))
       '<))
(defuns-per-range sorted-lengths top-of-ranges)
