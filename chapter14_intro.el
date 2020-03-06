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
