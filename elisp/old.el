(let ((pep8-buffer-name "*pep8*")
      (pep8-error-buffer-name "*pep8-puller")
      (project-dir "/home/travish/Projects/project")
      (find-command "find %s -name '*.py' -exec pep8 {} \\;"))
  (shell-command
   (format find-command project-dir)
   pep8-buffer-name
   pep8-error-buffer-name)
  (switch-to-buffer pep8-buffer-name)
  (compilation-mode))

;; This doesn't get u with an umlaut yet...
(defun iso-to-tex-buffer ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward "á" nil t)
      (replace-match "\\'{a}" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "é" nil t)
      (replace-match "\\'{e}" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "í" nil t)
      (replace-match "\\'{\\i}" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "ó" nil t)
      (replace-match "\\'{o}" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "ú" nil t)
      (replace-match "\\'{u}" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "ö" nil t)
      (replace-match "\\\"{o}" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "ñ" nil t)
      (replace-match "\\~{n}" nil t))))

(defun iso-to-text-buffer ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward "¢" nil t)
      (replace-match "o" nil t))
    
    (while (search-forward "á" nil t)
      (replace-match "a" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "é" nil t)
      (replace-match "e" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "í" nil t)
      (replace-match "i" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "ó" nil t)
      (replace-match "o" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "ú" nil t)
      (replace-match "u" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "ö" nil t)
      (replace-match "o" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "ñ" nil t)
      (replace-match "n" nil t))))

;; This doesn't get u with an umlaut yet...
(defun tex-to-iso-buffer ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward "\\'{a}" nil t)
      (replace-match "á" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "\\'{e}" nil t)
      (replace-match "é" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "\\'{\\i}" nil t)
      (replace-match "í" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "\\'{o}" nil t)
      (replace-match "ó" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "\\'{u}" nil t)
      (replace-match "ú" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "\\\"{o}" nil t)
      (replace-match "ö" nil t))
    
    (beginning-of-buffer)
    (while (search-forward "\\~{n}" nil t)
      (replace-match "ñ" nil t))))


(defun switch-to-frame-showing-buffer (name)
  
  "Switch to the frame showing buffer whose name is `name'. If
no frame is showing the buffer, switch this frame to display
that buffer. If the buffer does not exist, ding."
  
  (interactive "bBuffer name: ")
  (let
      ((buf (buffer-from-buffer-name name)))

    (if buf
	(progn
	  (let
	      ((frames (frame-list))
	       (frame))

	    (setq frame
		  (catch 'f
		    (while frames
		      (let
			  ((buffer
			    (buffer-name
			     (car (frame-parameter (car frames) 'buffer-list)))))

			(message (format "buffer is %s" buffer))
			(if (equal buffer name)
			    (throw 'f (car frames)))
			(setq frames (cdr frames))
			)
		      )
		    (throw 'f nil)
		    )
		  )
	    
	    
	    (if frame
		(select-frame frame)
	      (switch-to-buffer buf))))
	       
      (message (format "No such buffer: `%s'." name))
      (ding))))


(defun shell-meta-dot ()
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc)))
    (goto-char pmark)
    (insert "\n")
    (comint-send-string proc "\M-.") 
    (set-marker pmark (point))
    (let ((pt (point))) ; wait for 1 line
      ;; This extra newline prevents the user's pending input from spoofing us.
      (insert "\n") (backward-char 1)
      (while (not (looking-at ".+"))
	(accept-process-output proc)
	(goto-char pt)))
    (goto-char pmark)
    (delete-char 1) ; remove the extra newline
    ))

(defun sign-teclata ()
  (interactive)
  (insert "Terry Jones (terry@teclata.es).")
  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun sign-verbose-teclata ()
  (interactive)
  (insert "Terry Jones
Teclata S.L.                                  Tel.: +34 3 3151521
Via Laietana 21, entlo. A                     Fax:  +34 3 2684212
08003 Barcelona                               E-mail: terry@teclata.es
SPAIN                                         URL: http://www.teclata.es/terry")
  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun teclata-word ()
  (interactive)
    (insert-before-last-white-maybe "http://www.teclata.es")
    (if (not (looking-at " \\|\t\\|\n")) (insert "/")))

(defun teclata-english ()
  (interactive)
    (insert-before-last-white-maybe "http://www.teclata.es/teclata/english")
    (if (not (looking-at " \\|\t\\|\n")) (insert "/")))

(defun teclata-spanish ()
  (interactive)
    (insert-before-last-white-maybe "http://www.teclata.es/teclata/spanish")
    (if (not (looking-at " \\|\t\\|\n")) (insert "/")))



   "c"     '(lambda ()
	      (interactive)
	      (if
		  (and
		   (boundp 'vc-mode)
		   vc-mode)
		  
		  (vc-toggle-read-only)
	      (call-interactively 'kill-region)))



(let
    ((chars "`~1!2@3#4$5%6^7&8*9(0)-_=+[{]}\\|;:'\",<.>/?aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ")
     length
     i)

  (setq i 0
	length (length chars))
  
  (while (< i length)
    
    (define-key my-fix-keymap (substring chars i (1+ i))
      `(lambda ()
	 (interactive)
	 (fix-last ,(substring chars i (1+ i)))))
    
    (setq i (1+ i))))
