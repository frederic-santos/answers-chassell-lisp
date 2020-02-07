;;; chapter4_intro.el --- Solutions to the exercises of chapter 4
;;; from "An Introduction to Programming in Emacs Lisp",
;;; by R. Chassell.

;;; Author: Frédéric Santos (2020)
;;; URL: https://gitlab.com/f.santos/answers-chassell-lisp

;;;;;;;;;;;;;;;;;;;;;
;;; § 4.6. Exercises.
;;; Write your own simplified-end-of-buffer function definition;
;;; then test it to see whether it works.
(defun simplified-end-of-buffer ()
  "Save mark and set point at the end of the buffer."
  (interactive) ; no argument since the function does not have one
  (push-mark (point) t nil)
  (goto-char (point-max)))

;;; Use if and get-buffer to write a function that prints a
;;; message telling you whether a buffer exists.
(defun test-buffer-exists (namebuf)
  "Test whether a specified buffer NAMEBUF exists."
  (interactive "B") ; cf. help of "interactive"
  (if (equal (get-buffer namebuf) nil)
      (message "The buffer %s does not exist." namebuf)
    (message "The buffer %s exists." namebuf)))

;;; Another variant:
(defun test-buffer-exists (namebuf)
  "Test whether a specified buffer NAMEBUF exists."
  (interactive (list (read-buffer
		      "Buffer name: "
		      (other-buffer (current-buffer) t))))
  (if (equal (get-buffer namebuf) nil)
      (message "The buffer %s does not exist." namebuf)
    (message "The buffer %s exists." namebuf)))
;; [fr]
;; Attention :
;; l'aide de interactive dit : "If the argument is not a string, it
;; is evaluated to get a list of arguments to pass to the command."
;; Il faut donc ajouter le "list" dans "interactive", même pour
;; un seul argument.
;; En effet, évaluer ceci :
(list "a" "b" "c")
;; On constate que c'est ce qu'on veut (créer une liste d'arguments).
;; Ne pas confondre avec :
("a" "b" "c")
;; qui provoque (évidemment) une erreur.
;; [/fr]

;;; Other exercises:
;;; Rewrote the function mark-whole-buffer
(defun perso-mark-whole-buffer ()
  "Mark whole buffer, put point at beginning and mark at the end."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))

;;; Rewrote the (much more complicated) function append-to-buffer
(defun perso-append-to-buffer (buffer start end)
  "Append to specified BUFFER the text of the region between START
and END.
It is inserted into that buffer before its point."
  ; Make the function interactive:
  (interactive ; how to retrieve each argument? Make a list.
   (list (read-buffer
	  "Append to the buffer: "
	  (current-buffer)
	  t)
	 (region-beginning)
	 (region-end)))
  ; Define & initialize the variables:
  (let
      ;; VARLIST:
      ((oldbuf (current-buffer)))
    ;; BODY (of let):
    (save-excursion
      (set-buffer buffer)
      (barf-if-buffer-read-only)
      (insert-buffer-substring oldbuf start end)
      )))
