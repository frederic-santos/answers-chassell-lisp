;;;;;;;;;;;;;;
;;; CHAPITRE 6

;;; § 6.2. what-line
;; Dans un premier temps on pourrait se contenter de ceci :
(defun simple-what-line ()
  "Simplified version of what-line.
Unlike the 'true' what-line, this version only returns
the current line number of point in the (widened) buffer."
  (interactive) ; no argument
  ;; First widen the buffer, and then restore it in its
  ;; original state at the end of the function:
  (save-restriction
    ;; Widen the buffer if narrowed:
    (widen)
    ;; Get line number in widened buffer:
    (message "Line %d"
	     (+ 1 (count-lines 1 (point))))
    ))

;; Mais on constate que la fonction ne marche pas bien.
;; L'exécuter avec le point au début d'une ligne, puis au milieu de la
;; même ligne. Commentaire ?
;; Explication : cf. l'aide de count-lines qui dit qu'il faut se situer
;; au début d'une ligne pour que ça marche. On est donc obligé d'ajouter
;; une instruction qui déplace le point au début d'une ligne.

(defun simple-what-line ()
  "Simplified version of what-line.
Unlike the 'true' what-line, this version only returns
the current line number of point in the (widened) buffer."
  (interactive) ; no argument
  ;; First widen the buffer, and then restore it in its
  ;; original state at the end of the function:
  (save-restriction
    ;; Widen the buffer if narrowed:
    (widen)
    ;; Move point to beginning of line:
    (save-excursion
      (beginning-of-line)
      ;; Get line number in widened buffer:
      (message "Line %d"
	       (+ 1 (count-lines 1 (point))))
      )))

;;; § 6.3. Exercice.
(defun first-sixty-chars ()
  "Display the first 60 characters of the current buffer (even if)
they are in a narrowed part."
  (interactive)
  (save-restriction
    (widen)
    (message "%s"
	     (buffer-substring-no-properties 1 61)
    )
    ))

(first-sixty-chars)
