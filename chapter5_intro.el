;;;;;;;;;;;;;;
;;; CHAPITRE 5

;;; § 5.1. copy-to-buffer
;;; Une version personnelle :
(defun perso-copy-to-buffer (buffer start end)
  "Replace all the text in BUFFER by the region
between START and END."
  ;; Trois arguments : le buffer et les bornes de la région :
  (interactive
   (list (read-buffer
	  "Copy to buffer: "
	  (other-buffer (current-buffer) t))
	 (region-beginning)
	 (region-end)))
  ;; Travailler localement :
  (let
      ;; VARLIST du let :
      ((oldbuf (current-buffer)))
    ;; BODY du let :
    (save-excursion
      (set-buffer buffer) ; changer de buffer
      (erase-buffer) ; effacer tout son contenu
      (insert-buffer-substring oldbuf
			       start
			       end) ; ajout texte
      )
    )
  )

;;; La version du manuel :
(defun correction-copy-to-buffer (buffer start end)
    "Replace all the text in BUFFER by the region
between START and END."
    ;; 'B' prend le buffer,
    ;; 'r' prend les bornes de la région.
    (interactive "BCopy to buffer :\nr")
    (let
	;; VARLIST du let :
	((oldbuf (current-buffer)))
      ;; BODY du let :
      (with-current-buffer buffer
	;; Avec le nouveau buffer :
	(erase-buffer)
	(insert-buffer-substring oldbuf
				 start
				 end))
      ) ; fin du let
    )

;;; § 5.2. insert-buffer
(defun perso-insert-buffer (buffer)
  "Insert after point the content of another BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  ;; 'b' prend un buffer déjà existant,
  ;; "*" assure qu'il n'est pas en lect. seule
  (interactive "*bInsert content of this buffer: ")
  ;; Begin body.
  ;; If BUFFER is a name, turn it into the buffer itself.
  (if (not (bufferp buffer))
      (setq buffer (get-buffer buffer)))
  ;; Perform "insertion"
  (let
      ;; VARLIST du let :
      ((start nil)
       (end nil))
    ;; BODY du let :
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

;; ou en version moderne :
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
   mark)  
  )

;;; § 5.3. beginning-of-buffer
;;; Rappel de la version simplifiée (exo) :
(defun simplified-beginning-of-buffer ()
  "Version simplifiée de M-< (rappel).
Met le point au début du buffer.
Met une marque à l'endroit actuel."
  (interactive)
  (push-mark (point))
  (goto-char (point-min))
  )

;;; § 5.5. Exercice
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

(test-fill-column)
(test-fill-column 20)
(test-fill-column 100)
