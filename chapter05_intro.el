;;; chapter05_intro.el --- Solutions to the exercises of chapter 5
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;
;;; § 5.5. Exercise
;; This was really a good exercise, because I understood the
;; difference between (interactive "p") and "P".
;; I first wrote the following (incorrect) version:
(defun test-fill-column (&optional width)
  "Test whether fill-column is > or < than WIDTH."
  ;; NB: if WIDTH was not provided, it is bounded to nil.
  (interactive "p")
  ;; Set the value that will be compared to fill-column:
  (if width
      (setq arg width)
    (setq arg 56))
  ;; Perform comparison:
  (if (>= fill-column arg)
      (message "fill-column is bigger than %d" arg)
    (message "fill-column is smaller than %d" arg)))

;; If I do M-x test-fill-column, I get the following answer:
;; "fill-column is bigger than 1"
;;
;; What's the problem?
;; It is explained in the manual. If you use interactive "P" and
;; the argument is not supplied, its default value is nil.
;; But if you use interative "p" and the argument is not supplied,
;; its default value is 1, it's not nil.
;; The complete explanation is that interactve "p" consists in
;; running the function prefix-numeric-value on the prefix. But if
;; the prefix is not supplied, it is nil, and precisely,
;; (prefix-numeric-value nil) is equal to 1 (by default).
;;
;; See also:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html
;; https://emacs.stackexchange.com/questions/13886/what-is-a-raw-prefix-argument-capital-p-in-interactive
;;
;; So the correct answer is:

(defun test-fill-column (&optional width)
  "Test whether fill-column is > or < than WIDTH."
  ;; NB: if WIDTH was not provided, it is bounded to nil.
  (interactive "P")
  ;; Set the value that will be compared to fill-column:
  (if width
      (setq arg (prefix-numeric-value width))
    (setq arg 56))
  ;; Perform comparison:
  (if (>= fill-column arg)
      (message "fill-column is bigger than %d" arg)
    (message "fill-column is smaller than %d" arg)))

;; Now it works! Try M-x test-fill-column, or:
(test-fill-column)
(test-fill-column 20)
(test-fill-column 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some other sections are studied below.

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 5.1. copy-to-buffer
;;; My own version:
(defun perso-copy-to-buffer (buffer start end)
  "Replace all the text in BUFFER by the region
between START and END."
  ;; Three args: the buffer itself, and the limits
  ;; (start, end) of the region.
  (interactive
   (list (read-buffer
	  "Copy to buffer: "
	  (other-buffer (current-buffer) t))
	 (region-beginning)
	 (region-end)))
  ;; Create local variables:
  (let
      ;; VARLIST:
      ((oldbuf (current-buffer)))
    ;; BODY:
    (save-excursion
      (set-buffer buffer) ; focus to other buffer
      (erase-buffer) ; erase all its content
      (insert-buffer-substring oldbuf
			       start
			       end))))

;;; The original version from R. Chassell:
(defun correction-copy-to-buffer (buffer start end)
    "Replace all the text in BUFFER by the region
between START and END."
    ;; 'B' for buffer (may still not exist)
    ;; 'r' for region (start and end of region)
    (interactive "BCopy to buffer :\nr")
    (let
	;; VARLIST:
	((oldbuf (current-buffer)))
      ;; BODY:
      (with-current-buffer buffer
	;; With the new buffer:
	(erase-buffer)
	(insert-buffer-substring oldbuf
				 start
				 end))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 5.2. insert-buffer
(defun perso-insert-buffer (buffer)
  "Insert after point the content of another BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  ;; 'b' for buffer that *already* exists
  ;; "*" makes sure that it is not read-only
  (interactive "*bInsert content of this buffer: ")
  ;; Begin body.
  ;; If BUFFER is a name, turn it into the buffer itself.
  (if (not (bufferp buffer))
      (setq buffer (get-buffer buffer)))
  ;; Perform "insertion":
  (let
      ;; VARLIST:
      ((start nil)
       (end nil))
    ;; BODY:
    (save-excursion
      (save-excursion
	(set-buffer buffer)
	(setq start (point-min)
	      end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point))
      )
    (push-mark newmark)
    )
  )

;; A more modern version:
(defun perso-insert-buffer (buffer)
  "Insert after point the content of another BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  (interactive "*bInsert content of this buffer: ")
  (push-mark
   (save-excursion
     (insert-buffer-substring buffer)
     (setq mark (point))
     )
   mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; § 5.3. beginning-of-buffer
;;; Reminder: write once again the simplified version.
(defun simplified-beginning-of-buffer ()
  "Version simplifiée de M-< (rappel).
Met le point au début du buffer.
Met une marque à l'endroit actuel."
  (interactive)
  (push-mark (point))
  (goto-char (point-min)))
