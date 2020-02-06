;;;;;;;;;;;;;;
;;; CHAPITRE 4
;;; § 4.6. Exercices.
;;; Réécrire la fonction liée à M-> en plus simple :
(defun simplified-end-of-buffer ()
  "Save mark and set point at the end of the buffer."
  (interactive) ; pas d'argument car la fonction n'en a pas.
  (push-mark (point) t nil)
  (goto-char (point-max)))

;;; Tester l'existence d'un buffer donné :
(defun test-buffer-exists (namebuf)
  "Test whether a specified buffer NAMEBUF exists."
  (interactive "B") ; cf. l'aide de "interactive"
  (if (equal (get-buffer namebuf) nil)
      (message "The buffer %s does not exist." namebuf)
    (message "The buffer %s exists." namebuf)))

;;; Variante plus manuelle :
(defun test-buffer-exists (namebuf)
  "Test whether a specified buffer NAMEBUF exists."
  (interactive (list (read-buffer
		      "Buffer name: "
		      (other-buffer (current-buffer) t))))
  (if (equal (get-buffer namebuf) nil)
      (message "The buffer %s does not exist." namebuf)
    (message "The buffer %s exists." namebuf)))
;;; Attention :
;;; l'aide de interactive dit : "If the argument is not a string, it
;;; is evaluated to get a list of arguments to pass to the command."
;;; Il faut donc ajouter le "list" dans "interactive", même pour
;;; un seul argument.
;;; En effet, évaluer ceci :
(list "a" "b" "c")
;;; On constate que c'est ce qu'on veut (créer une liste d'arguments).
;;; Ne pas confondre avec :
("a" "b" "c")
;;; qui provoque (évidemment) une erreur.

;;; Réécrire la fonction mark-whole-buffer
(defun perso-mark-whole-buffer ()
  "Mark whole buffer, put point at beginning and mark at the end."
  (interactive)
  (push-mark (point)) ; juste au cas où, pour pouvoir y revenir + tard
  (push-mark (point-max) nil t)
  (goto-char (point-min)))

;;; Plus compliqué : réécrire la fonction append-to-buffer
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
      ;; begin VARLIST of let:
      ((oldbuf (current-buffer)))
    ;; begin BODY of let expression:
    (save-excursion
      (set-buffer buffer)
      (barf-if-buffer-read-only)
      (insert-buffer-substring oldbuf start end)
      )
    ) ; end of let
  ) ; end of function
