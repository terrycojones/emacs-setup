(require 'my-shell)

(setq *fluiddb-credentials* '("terrycojones" . "2soft2look4find"))

(defun insert-lastpage (who)
  (interactive "sInsert the last URL of which user? ")
  (let*
      ((user (or who (car *fluiddb-credentials*)))
       (result (fluiddb-query-objects-tag-values (format "has %s/lastpage" user) '("fluiddb/about"))))
    (insert (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (car (cdr result)))))))))))))))

(defun clear-lastpage (who)
  (interactive "sClear the last URL of which user? ")
  (let
      ((tag (format "%s/lastpage" (or who (car *fluiddb-credentials*)))))
    (fluiddb-delete-objects-tag-values (format "has %s" tag) (list tag))))

(defun set-lastpage (who url)
  (interactive "sSet the last URL of which user? \nsURL: ")
  (let*
      ((user (if (equal who "") (car *fluiddb-credentials*) who))
       (tag (format "%s/lastpage" user)))
    (clear-lastpage user)
    (fluiddb-set-object-about-tag-value url tag nil)))

(defun insert-clipboard (who)
  (interactive "sInsert the clipboard of which user? ")
  (let*
      ((user (if (equal who "") (car *fluiddb-credentials*) who))
       (tag (format "%s/clipboard" user))
       (result (fluiddb-query-objects-tag-values (format "has %s" tag) (list tag)))
       (object (cdr (car (cdr (car (car (cdr result))))))))
    (if object
        (insert (cdr (car (cdr (car (cdr (car object)))))))
      (message "Remote clipboard is empty."))))

(defun clear-clipboard (who)
  (interactive "sClear the clipboard of which user? ")
  (let
      ((tag (format "%s/clipboard" (if (equal who "") (car *fluiddb-credentials*) who))))
    (fluiddb-delete-objects-tag-values (format "has %s" tag) (list tag))))

(defun set-clipboard (who)
  (interactive "sSet the region as the clipboard of which user? ")
  (let*
      ((user (if (equal who "") (car *fluiddb-credentials*) who))
       (tag (format "%s/clipboard" user)))
    (clear-clipboard user)
    (fluiddb-set-object-about-tag-value user tag (buffer-substring (region-beginning) (region-end)))))

(defun insert-last-shell-pwd nil
  (interactive)
  (beginning-of-line)
  (minibuffer-avoid-prompt nil nil)
  (kill-line)
  (insert-file-contents "~/.pwdlog")
  (end-of-line)
  (kill-line)
  (insert "/"))

(add-hook 'minibuffer-setup-hook
          '(lambda () (interactive) 
             (local-set-key [f8] 'insert-last-shell-pwd)))

(defun start-or-end-kbd-macro (&optional args)
  (interactive "P")
  (if defining-kbd-macro
      (end-kbd-macro args)
    (start-kbd-macro args)))

(defun goto-matching-paren-or-insert (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
	((looking-at "[])}]") (forward-char) (backward-sexp 1))
	((and (equal 'm4-mode major-mode) (looking-at "`")) (forward-sexp 1) (backward-char))
	((and (equal 'm4-mode major-mode) (looking-at "'")) (forward-char) (backward-sexp 1))
	(t (self-insert-command (or arg 1)))))

(defun add-hook-if-x (hook-name hook-code)
  (interactive "P")
  (and running-x (add-hook hook-name hook-code)))

(defun add-hook-if-color-x (hook-name hook-code)
  (interactive "P")
  (and color-x (add-hook hook-name hook-code)))

(defun end-of-next-line ()
  (interactive)
  (next-line 1)
  (end-of-line))
  
(defun space-around-char ()
  (interactive)
  (insert " ")
  (forward-char)
  (insert " "))

(defun plus-to-end ()
  (interactive)
  (replace-regexp "^" "+ "))

(defun plus-buffer ()
  (interactive)
  (goto-char (point-min))
  (plus-to-end))

(defun plus-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark t))
      (goto-char (point-min))
      (plus-to-end))))

(defun pipe-to-end ()
  (interactive)
  (replace-regexp "^" "| "))

(defun pipe-buffer ()
  (interactive)
  (goto-char (point-min))
  (pipe-to-end))

(defun pipe-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark t))
      (goto-char (point-min))
      (pipe-to-end))))

(defun gt-to-end ()
  (interactive)
  (replace-regexp "^" "> "))

(defun gt-buffer ()
  (interactive)
  (goto-char (point-min))
  (gt-to-end))

(defun gt-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark t))
      (goto-char (point-min))
      (gt-to-end))))

(defun br-to-end ()
  (interactive)
  (replace-regexp "$" " <br>"))

(defun br-buffer ()
  (interactive)
  (goto-char (point-min))
  (br-to-end))

(defun br-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark t))
      (goto-char (point-min))
      (br-to-end))))

(defun quote-to-end ()
  (interactive)
  (let
      ((start))
    (beginning-of-line)
    (delete-horizontal-space)
    (setq start (point))

    (while
		(= 0 (forward-line 1))
      (delete-horizontal-space))

    (goto-char start)

    (while
		(not (= (point) (point-max)))
      (fill-paragraph nil)
      (forward-paragraph))

    (goto-char start)
    (gt-to-end)

    (goto-char (point-max))
    (beginning-of-line)

    (while
		(looking-at "^|[ 	]*$")
      (kill-line)
      (forward-line -1))

    (goto-char start)))

(defun quote-buffer ()
  (interactive)
  (goto-char (point-min))
  (quote-to-end))

(defun quote-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark t))
      (goto-char (point-min))
      (quote-to-end))))

(defun kill-this-buffer ()
  (interactive)
  (if (or
       (string= (substring (buffer-name) 0 1) "*")
       (and (> (length (buffer-name)) 7)
	    (string= (substring (buffer-name) 0 8) "mail to "))
       (and (> (length (buffer-name)) 8)
	    (string= (substring (buffer-name) 0 9) "reply to ")))
      (progn
	(ding)
	(message "This function cannot kill buffers with names like this. Use C-x k explicitly."))
    (kill-buffer nil)))

(defun previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun define-keys (map defns)
  (interactive "P")
  (if defns
      (progn
	(define-key map (car defns) (cadr defns))
	(define-keys map (cddr defns)))))

(defun visit-my-emacs-startup-file ()
  (interactive)
  (find-file
   (concat
    "~/emacs/startup/" (int-to-string emacs-major-version) "." (int-to-string emacs-minor-version) ".el")))

(defun underline-line ()
  (interactive)
  (underline-line-arg ?-))

(defun underline-line-arg (ch)

  "Underline the current line. Uses the character argument to do the
  underlining. If the following line \(if any\) in the buffer looks like
  an underlining line \(i.e., it is composed solely of \(optional\)
  leading whitespace followed by instances of the underlining
  character\), it is deleted. This allows you to re-underline an
  already underlined line if its content changes.  If the line to be
  underlined starts with white space, this is not underlined, but is
  inserted into the beginning of the underlining line."

  (interactive "cUnderline using which char? ")
  (save-excursion
    (let ((white "")
	  beg
	  count)
      
      (beginning-of-line)
      (setq beg (point))
      
      (if (looking-at "\\([ \t]+\\)[^ \t\n]")
	    (setq white (buffer-substring (match-beginning 1) (match-end 1))
		  beg (+ beg (- (match-end 1) (match-beginning 1)))))
	  
      (end-of-line)
      (setq count (- (point) beg))

      ;; Kill the existing underline, if it looks like one exists.
      (if (eq (forward-line 1) 0)
	  (progn
	    (beginning-of-line)
	    (if (looking-at (format "^[ \t]*%c+$" ch))
	      (kill-line 1))
	    (forward-line -1)
	    (end-of-line)))
	
      (insert "\n" white)
      (insert-char ch count))))


(defun delete-white-after (&optional newlines-not-white)
  (interactive "P")
  (let
      ((what-to-eat (if newlines-not-white " \\|\t" " \\|\t\\|\n")))
    (while (looking-at what-to-eat)
      (delete-char 1))))

(defun delete-white-around (&optional newlines-not-white)
  (interactive "P")
  (back-to-first-white newlines-not-white)
  (delete-white-after newlines-not-white))

(defun back-to-first-white (&optional newlines-not-white)
  ;; Moves back to the first white space, if you are in whitespace.
  (interactive "P")
  (let
      (moved
       (what-to-skip (if newlines-not-white " \\|\t" " \\|\t\\|\n")))

    (if (looking-at what-to-skip)
	(progn
	  (setq moved t)
	  (forward-char -1)))
    
    (while (looking-at what-to-skip)
      (forward-char -1))

    (if moved (forward-char 1))))

(defun insert-before-last-white-maybe (s)
  ;; Moves back to the first white space, if you are in whitespace.
  (interactive "P")
  (let
      (moved)

    (while
	(and
	 (> (point) 1)
	 (not (looking-at " \\|\t\\|\n")))
      (progn
	(setq moved t)
	(forward-char -1)))

    (if moved (forward-char 1))

    (if (not (looking-at s))
	(insert s))))

(defun http-word ()
  "Puts \"http://\" before the current word, if it's not there already."
  (interactive)
  (save-excursion
    (if (and (> (point) 1) (looking-at "\n")) (forward-char -1))
    (insert-before-last-white-maybe "http://")))

(defun terry-url ()
  (interactive)
    (insert-before-last-white-maybe "http://jon.es")
    (if (not (looking-at " \\|\t\\|\n")) (insert "/")))

(defun terry-ftp ()
  (interactive)
    (insert-before-last-white-maybe "ftp://jon.es/pub")
    (if (not (looking-at " \\|\t\\|\n")) (insert "/")))

(defun terry-resume ()
  (interactive)
    (insert-before-last-white-maybe "http://jon.es/personal/resume.html"))

(defun terry-mobile ()
  (interactive)
    (insert-before-last-white-maybe "+34 654906866"))

(defun terry-bookmarks ()
  (interactive)
    (insert-before-last-white-maybe "http://jon.es/bookmarks/public/")
    (if (not (looking-at " \\|\t\\|\n")) (insert "/")))

(defun terry-leona ()
  (interactive)
    (insert-before-last-white-maybe "http://jon.es/barna/leona.html"))

(defun terry-scams ()
  (interactive)
    (insert-before-last-white-maybe "http://jon.es/barna/scams.html"))

(defun terry-smugmug ()
  (interactive)
    (insert-before-last-white-maybe "http://terrycojones.smugmug.com"))

(defun terry-fluidinfo ()
  (interactive)
    (insert-before-last-white-maybe "http://fluidinfo.com/terry"))

(defun terry-twitter ()
  (interactive)
    (insert-before-last-white-maybe "http://twitter.com/terrycojones"))

(defun fluidinfo ()
  (interactive)
    (insert-before-last-white-maybe "http://fluidinfo.com"))

(defun sign ()
  (interactive)
  (insert "Terry Jones (tc.jones@jon.es).")
  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun sign-zoo ()
  (interactive)
  (insert "
Dr. Terry Jones
Department of Zoology
University of Cambridge
Downing St
Cambridge, CB2 3EJ
United Kingdom")
  (if (looking-at "\n")
      (forward-char 1)
	(insert "\n")))


(defun sign-zoo-verbose ()
  (interactive)
  (insert "
Dr. Terry Jones                            E-mail: tcj25@cam.ac.uk
Department of Zoology                      Tel: +34 654906866
University of Cambridge
Downing St
Cambridge, CB2 3EJ
United Kingdom")
  (if (looking-at "\n")
      (forward-char 1)
	(insert "\n")))

(defun sign-betaworks ()
  (interactive)
  (insert "Terry Jones
416 W. 13th St #203
New York, NY 10014-1178
USA")
  (if (looking-at "\n")
      (forward-char 1)
	(insert "\n")))

(defun sign-borne ()
  (interactive)
  (insert "Terry Jones
Paseo del Borne 19, 3, 1
08003 Barcelona
SPAIN")

  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun sign-borne-verbose ()
  (interactive)
  (insert "Terry Jones                                E-mail: tc.jones@jon.es
Paseo del Borne 19, 3, 1                   Tel: +34 932681684
08003 Barcelona                            GSM: +34 654906866
SPAIN                                      URL: http://jon.es
")

  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun sign-sombrerers ()
  (interactive)
  (insert "Terry Jones
Sombrerers 23, 2, 2
08003 Barcelona
SPAIN")

  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun sign-sombrerers-verbose ()
  (interactive)
  (insert "Terry Jones                                E-mail: terry@jon.es
Sombrerers 23, 2, 2                        Tel: +34 932681684
08003 Barcelona                            GSM: +34 654906866
SPAIN                                      URL: http://jon.es
")

  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun sign-upf ()
  (interactive)
  (insert "Terry Jones
Departament de Tecnologia
Passeig de Circumval·lació, 8
Universitat Pompeu Fabra
08003 Barcelona
SPAIN")

  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun rt ()
  (interactive)
  (insert "Regards,\nTerry\n"))

(defun rtj ()
  (interactive)
  (insert "Regards,\nTerry Jones\n"))

(defun double-space-sentences-to-end ()
  (interactive)
  (save-excursion
    (while (re-search-forward "\\([^. 	].\\)\\. \\([^ ]\\)" nil t)
      (replace-match "\\1.  \\2" nil nil))))

(defun double-space-sentences-buffer ()
  (interactive)
  (goto-char (point-min))
  (double-space-sentences-to-end))

(defun double-space-sentences-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark t))
      (goto-char (point-min))
      (double-space-sentences-to-end))))

(defun replace-string-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark t))
      (goto-char (point-min))
      (call-interactively 'replace-string))))

(defun replace-regexp-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark t))
      (goto-char (point-min))
      (call-interactively 'replace-regexp))))

(defun create-my-frames ()
  (interactive)
  (dired (expand-file-name "~"))
  (make-frame)
  (vm)
  (new-shell-frame)
  (new-shell-frame))

(defun find-file-sans-backup (file)
  (interactive "fFind without backups : ")
  (find-file file)
  (make-variable-buffer-local 'backup-inhibited)
  (setq backup-inhibited t))
	
(defun indent-to-with-spaces (n)
  (interactive "NIndent with spaces to which column? : ")
  (let
      ((need (- n (current-column))))

    (while (> need 0)
      (insert " ")
      (setq need (- need 1 )))))
      
(defun finish-kbd-macro-and-exec (&optional n)
  (interactive "P")
  (if defining-kbd-macro
      (end-kbd-macro))
  (call-last-kbd-macro n))
  
(defun toggle-case-fold-search ()
    (interactive)
    (setq case-fold-search (not case-fold-search))
    (message "%s %s %s" "Case will be" (if case-fold-search "IGNORED" "CONSIDERED") "in searches."))

;;;
;;; A function to yank in the last visited URL. Repeat usage (done fast enough)
;;; gets successively earlier URLs.
;;;
(setq last-url-insert-time 0)
(setq last-url-n 0)
(setq last-url-point 0)

(defun insert-last-url (inc)
  (interactive "p")

  (let
      ((now (cadr (current-time))))

    (if (> (- now last-url-insert-time) 5)
	;; Start afresh.
	(progn
	  (setq
	   last-url-n 0
	   last-url-point (point)))
	  
      ;; Otherwise, start up where we left off - zap out the last insert.
      (kill-region last-url-point (point)))

    (setq last-url-insert-time now)

    (if (> (+ last-url-n inc) 0)
	(progn
	  (setq last-url-n (+ last-url-n inc))
	  (call-process "last-urls.py" nil '(t nil) nil (number-to-string last-url-n)))
      (message "No further URLs available."))))

(defun insert-last-urls (n)
  (interactive "p")
  (if (> n 0)
      (call-process "last-urls.py" nil '(t nil) nil (number-to-string n))))


(setq my-fixable-chars "`~1!2@3#4$5%6^7&8*9(0)-_=+[{]}\\|;:'\",<.>/?aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ")


(defun my-old-fix-last (from)
  (interactive "s")
  (save-excursion
    (let
		((old-case case-fold-search)
		 pos)

	  (setq case-fold-search nil)
      (if (setq pos (string-match (regexp-quote from) my-fixable-chars))
		  (progn
			(message (format "pos before = %d" pos))
			(setq pos (+ pos (if (zerop (% pos 2)) 1 -1)))
			(message (format "pos after  = %d" pos))
			(if (search-backward from (point-min) t)
				(progn
				  (message (format "here too, pos = %d, replacing '%s' with '%s'" pos from (substring my-fixable-chars pos (1+ pos))))
				  (replace-match (substring my-fixable-chars pos (1+ pos)) t t)))))
	  (setq case-fold-search old-case))))

(defun my-fix-last (from)
  (interactive "s")
  (save-excursion
    (let*
		((old-case case-fold-search)
		 case-fold-search
		 (pos (string-match (regexp-quote from) my-fixable-chars)))
      (when pos 
		(setq pos (+ pos (if (zerop (% pos 2)) 1 -1)))
		(and
		 (search-backward from (point-min) t)
		 (replace-match (substring my-fixable-chars pos (1+ pos)) t t)))
		(setq case-fold-search old-case))))

(defun my-delete-last (what)
  (interactive "s")
  (save-excursion
    (and
	 (search-backward what (point-min) t)
	 (replace-match ""))))

(defun new-frame-slowly ()
  (sleep-for 1)
  (make-frame))

(defun count-matches-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark t))
      (goto-char (point-min))
      (call-interactively 'count-matches))))

(defun find-file-at-end-if-unvisited (file)
  "Go to the file if it's currently being visited, else visit and go to end."
  (let*
      ((buf (buffer-visiting-file file)))
    (if buf
	(switch-to-buffer buf)
      (find-file file)
      (end-of-buffer))))

(defun email-from-tc ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry <tc@jones.tc>"))
  (setq
    mail-default-reply-to            "tc@jones.tc"
    user-mail-address                "tc@jones.tc"
    mail-from-style                  'angles
    mail-envelope-from               "Terry <tc@jones.tc>"))

(defun email-from-tc.jones ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry Jones <tc.jones@jones.tc>"))
  (setq
    mail-default-reply-to            "tc.jones@jones.tc"
    user-mail-address                "tc.jones@jones.tc"
    mail-from-style                  'angles
    mail-envelope-from               "Terry Jones <tc.jones@jones.tc>"))

(defun email-from-jon.es ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry Jones <terry@jon.es>"))
  (setq
    mail-default-reply-to            "terry@jon.es"
    user-mail-address                "terry@jon.es"
    mail-from-style                  'angles
    mail-envelope-from               "Terry Jones <terry@jon.es>"))

(defun email-from-gmail ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry Jones <terry.jones@gmail.com>"))
  (setq
    mail-default-reply-to            "terry.jones@gmail.com"
    user-mail-address                "terry.jones@gmail.com"
    mail-from-style                  'angles
    mail-envelope-from               "Terry Jones <terry.jones@gmail.com>"))

(defun email-from-sfi ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry Jones <terry@santafe.edu>"))
  (setq
    mail-default-reply-to            "terry@santafe.edu"
    user-mail-address                "terry@santafe.edu"
    mail-from-style                  'angles
    mail-envelope-from               "Terry Jones <terry@santafe.edu>"))

(defun email-from-zoo ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry Jones <tcj25@cam.ac.uk>"))
  (setq
    mail-default-reply-to            "tcj25@cam.ac.uk"
    user-mail-address                "tcj25@cam.ac.uk"
    mail-from-style                  'angles
    mail-envelope-from               "Terry Jones <tcj25@cam.ac.uk>"))

(defun email-from-upf ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry Jones <terry.jones@upf.edu>"))
  (setq
    mail-default-reply-to            "terry.jones@upf.edu"
    user-mail-address                "terry.jones@upf.edu"
    mail-from-style                  'angles
    mail-envelope-from               "Terry Jones <terry.jones@upf.edu>"))

(defun email-from-eatoni ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry Jones <terry@eatoni.com>"))
  (setq
    mail-default-reply-to            "terry@eatoni.com"
    user-mail-address                "terry@eatoni"
    mail-from-style                  'angles
    mail-envelope-from               "Terry Jones <terry@eatoni.com>"))

(defun email-from-ana-eatoni ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Ana Mosterin <ana@eatoni.com>"))
  (setq
    mail-default-reply-to            "ana@eatoni.com"
    user-mail-address                "ana@eatoni"
    mail-from-style                  'angles
    mail-envelope-from               "Ana Mosterin <ana@eatoni.com>"))

(defun email-from-fluidinfo ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry Jones <terry@fluidinfo.com>"))
  (setq
    mail-default-reply-to            "terry@fluidinfo.com"
    user-mail-address                "terry@fluidinfo.com"
    mail-from-style                  'angles
    mail-envelope-from               "Terry Jones <terry@fluidinfo.com>"))

(defun re-tab-buffer ()
  (interactive)
  (save-excursion
	(save-restriction
	  (widen)
	  (setq tab-width 4)
	  (indent-region 0 (point-max) nil)
	  (untabify 0 (point-max))
	  (tabify 0 (point-max))
	  (delete-trailing-whitespace))))

(defun delete-enclosing-function ()
  (interactive)
  (save-excursion
   (forward-sexp 1)
   (backward-sexp 1)
   (let ((beg (point)))
	 (up-list 1)
	 (backward-sexp 1)
	 (delete-region beg (point)))
   (forward-sexp 1)
   (let ((beg (point)))
	 (up-list 1)
	 (delete-region beg (point)))
   (backward-sexp 1)))

(defun set-frame-transparency (alpha type)
  (modify-frame-parameters (selected-frame) `((,type . ,alpha))))

(defun set-frame-active-transparency (alpha)
  (interactive "p")
  (set-frame-transparency alpha 'active-alpha))

(defun set-frame-inactive-transparency (alpha)
  (interactive "p")
  (set-frame-transparency alpha 'inactive-alpha))

(defun my-today ()
  (map 'list 'string-to-int (split-string (format-time-string "%Y %m %d"))))

(defun my-days-in-month (month year)
  (case month
    ;; 30 days have September, April, June and November...
    ((9 4 6 11) 30)
    ((2) (if (date-leap-year-p year) 29 28))
    (t 31)))

(defun my-previous-day (ymd)
  (let
      ((year (car ymd))
       (month (cadr ymd))
       (day (1- (caddr ymd))))
    (if (eq day 0)
        (progn
          (setq month (1- month))
          (if (eq month 0)
              (setq month 12
                    year (1- year)))
          (setq day (my-days-in-month month year))))
    (list year month day)))

(defun my-next-day (ymd)
  (let
      ((year (car ymd))
       (month (cadr ymd))
       (day (1+ (caddr ymd))))
    (if (> day (my-days-in-month month year))
        (progn
          (setq day 1
                month (1+ month))
          (if (eq month 13)
              (setq month 1
                    year (1+ year)))))
    (list year month day)))

(defun my-yyyymmdd-string (ymd)
  (format "%d%02d%02d" (car ymd) (cadr ymd) (caddr ymd)))

(provide 'my-misc)
