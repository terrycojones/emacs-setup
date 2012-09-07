
(add-hook 'dired-load-hook
	  (function
	   (lambda ()
	     
	     (load "dired-x")
	     
	     ;; dired-x global variables.
	     (setq
	      dired-omit-files (concat dired-omit-files "\\|^CVS$\\|^\\..+$")
	      dired-local-variables-file nil
	      dired-enable-local-variables nil
	      dired-guess-shell-gnutar "tar"
	      dired-patch-unclean-extensions (list ".rej")
	      dired-x-hands-off-my-keys nil
	      )
	     
	     (dired-x-bind-find-file)
	     
	     (define-keys
	       dired-mode-map
	       (list
		"\M-e"    'dired-mark-extension
		"\M-E"    'dired-flag-extension
		"\C-o"    'dired-omit-toggle
		"j"       'dired-jump
		[mouse-2] 'dired-mouse-find-file
		))
	     
	     )))

(add-hook 'dired-mode-hook
	  (function
	   (lambda ()
	     ;; dired-x buffer-local variables.
	     (setq dired-omit-files-p t)
	     )))

; (defun dired-mouse-find-file (event)
;   "In dired, visit the file or directory name you click on."
;   (interactive "e")
;   (let (file)
;     (save-excursion
;       (set-buffer (window-buffer (posn-window (event-end event))))
;       (save-excursion
; 	(goto-char (posn-point (event-end event)))
; 	(setq file (dired-get-filename))))
;     (select-window (posn-window (event-end event)))
;     (let ((file-name (file-name-sans-versions file t)))
;       (if (file-exists-p file-name)
; 	  (find-file file-name)
; 	(if (file-symlink-p file-name)
; 	    (error "File is a symlink to a nonexistent target")
; 	  (error "File no longer exists; type `g' to update Dired buffer"))))))

(defun my-fast-dired-x-find-file ()
  "Edit file whose filename is near point."
  (interactive)
  (find-file (expand-file-name (dired-filename-at-point))))

(defun my-dired-mouse-find-file (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (mouse-drag-region event)
  (my-fast-dired-x-find-file))


; (defun my-right-button (event)
;   "Visit the file or directory name you click on, and if there isn't one, call hideshow to try to hide code."
;   (interactive "e")
;   (mouse-drag-region event)
;   (let
;       ((f (my-dired-filename-at-point))

;     (if f
; 	(find-file f)

;       (hs-mouse-toggle-hiding event)))))

; (defun my-dired-filename-at-point ()

;   ;; This hacked by Terry from dired-x.el 9/26/98
;   ;; To remove backward motion before looking for file name
;   ;; and not call error when no file is found.

;   (let ((filename-chars ".a-zA-Z0-9---_/:$+")
;         (bol (save-excursion (beginning-of-line) (point)))
;         (eol (save-excursion (end-of-line) (point)))
;         start end filename)

;     (save-excursion
;       (if (string-match (concat "[" filename-chars "]")
;                         (char-to-string (following-char)))
;           (progn
;             (skip-chars-backward filename-chars)
;             (setq start (point))
;             (if (string-match "[/~]" (char-to-string (preceding-char)))
;                 (setq start (1- start)))
;             (skip-chars-forward filename-chars)
; 	    (expand-file-name (buffer-substring start (point))))

;         nil))))


(provide 'my-dired)
