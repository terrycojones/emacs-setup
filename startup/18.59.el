;;; -*-Emacs-Lisp-*-

(defun enter-abbrev-mode () (abbrev-mode 1))

(defun my-gnus-kill-mode-string ()
  (kill-local-variable 'global-mode-string))

(defun msg-line-column ()
  "Print line and column numbers"
  (interactive)
  (let ( (col (current-column)) )
    (save-restriction
      (widen)
      (save-excursion
	(beginning-of-line)
	(message "Line: %d Column: %d" (1+ (count-lines 1 (point))) col)))))

(defun startup ()

  (setq load-path (append load-path (list (expand-file-name "~/s/elisp"))))
  (setq display-time-day-and-date t)
  (display-time)

  (or global-mode-string (setq global-mode-string '("")))
  (setq global-mode-string
	(append global-mode-string 
		(list " " (substring (system-name) 0 (string-match "\\." (system-name))) "  ")))
  
  ;; supercite stuff
  (autoload 'sc-cite-original "supercite"  "Hookified Supercite 2.3" t)
  (autoload 'sc-cite          "supercite"  "Interactive Supercite 2.3" t)
  (setq mail-yank-hooks 'sc-cite-original)
  (setq sc-citation-leader    "")
  (setq sc-citation-delimiter ">")
  (setq sc-citation-separator " ")
  (setq sc-nested-citation-p nil)
  (setq sc-downcase-p nil)
  (setq sc-preferred-header-style 4)
  (setq sc-auto-fill-region-p t)
  (setq sc-fixup-whitespace-p t)
  
  (autoload 'arts-find-tag "tag-stack" "" t)
  (autoload 'visit-tags-table "tag-stack" "" t)
  
  (setq vm-flush-interval t)
  (setq vm-primary-inbox (expand-file-name "~/mail/INBOX"))
  (setq vm-folder-directory (expand-file-name "~/mail/"))
  (setq vm-crash-box (expand-file-name "~/mail/crash-box"))
  (setq vm-inhibit-startup-message t)
  (setq vm-delete-after-saving t)
  (setq vm-move-after-deleting t)
  (setq vm-search-using-regexps t)
  (setq vm-gargle-uucp t)
  (setq vm-startup-with-summary 1)
  (setq vm-mutable-windows t)
  (setq vm-preview-lines nil)
  (setq vm-summary-format "%3n %a %-17.17F (%-15.15f) %3m %2d %3l/%-5c \"%s\"\C-j")
  (setq vm-visible-headers '("^From" "^From " "^Subject" "^Date" "^To"))
  (setq vm-in-reply-to-format "Your message at %h on %w, %d %m %y")
  (setq vm-mail-window-percentage 60)
  (setq vm-included-text-prefix "+ ")
  (setq vm-auto-folder-alist 
	'(("From"
	   ("terry@cluster" . (expand-file-name "~/mail/cluster"))
	   ("alife-request@iuvax.cs.indiana.edu" . (expand-file-name "~/alife"))
	   ;; match id in <>
	   ("<\\([^>@%]+\\)" . (buffer-substring (match-beginning 1) (match-end 1)))
	   ;; match id with @
	   ("\\([^@%]+\\)[@%]" . (buffer-substring (match-beginning 1) (match-end 1)))
	   ;; match id with machine!id@wherever.
	   ;; ("[^!]+!\\([^@%]+\\)[@%]" . (buffer-substring (match-beginning 1) (match-end 1)))
	   ;; match first word
	   ("\\(\\w+\\) " . (buffer-substring (match-beginning 1) (match-end 1))))))

  ;; This must be done before loading dm-c-terry, otherwise it will clobber the abbrevs defined there.
  (define-abbrev-table 'global-abbrev-table '
    (("rememeber" "remember" nil 1)
     ("teh" "the" nil 0)
     ("ahve" "have" nil 0)
     ("interseting" "interesting" nil 0)
     ("thasnk" "thanks" nil 1)
     ("taht" "that" nil 1)
     ("doen" "done" nil 0)))

  (load "terry")
  (load "ebuff-menu")
  (load "c-fill")
  (load "vm")
  (load "dm-c-terry")
  (setq auto-dmacro-alist '(("\\.c$" . dot-c) ("\\.h$" . dot-h)))

  (if (and (not (boundp 'ebuttons-running)) (equal window-system 'x))
      (progn
	(load "ebuttons")
	(ebuttons)))
  
  ;; Miscellaneous stuff.

  (setq-default case-fold-search nil)
  (setq pop-up-windows t)
  (setq blink-matching-paren-distance 20000)
  (setq default-major-mode 'text-mode)
  (setq text-mode-hook 'turn-on-auto-fill)
  (setq enable-recursive-minibuffers t)
  (put 'eval-expression 'disabled nil)
  (setq inhibit-startup-message t)
  (setq find-file-hooks '(enter-abbrev-mode))
  (setq find-file-not-found-hooks '(enter-abbrev-mode))
  (setq compile-command "make")
  (setq mail-archive-file-name (expand-file-name "~/mail/OUT"))
  (setq mark-ring-max 64)
  (setq kill-ring-max 64)
  (setq fill-column 200)
  (setq lpr-switches (list (concat "-P" (getenv "PRINTER"))))
  
  (require 'ehelp)
  ;; (require 'crypt)
  
  (define-key global-map "\C-x\C-b"   'electric-buffer-list)
  (define-key global-map "\C-h"       'ehelp-command)
  (define-key global-map "\C-xt"      'ebuttons)
  (define-key global-map "\C-x\m"     'vm-mail)
  (define-key global-map "\C-c="      'msg-line-column)
  (define-key global-map "\C-cG"      'goto-line)
  (define-key global-map "\C-c-"      '(lambda () (interactive) (recenter -1)))
  (define-key global-map "\C-c\C-m"   '(lambda () (interactive) (recenter 0)))
  
  ;; c mode stuff.
  (modify-syntax-entry   ?#       "w" c-mode-syntax-table)
  (modify-syntax-entry   ?_       "w" c-mode-syntax-table)
  (define-key c-mode-map "\C-m"       'newline-and-indent)
  (define-key c-mode-map "\C-c\C-c"   'compile)
  (define-key c-mode-map "\C-cc"      'c-comment-line)
  (define-key c-mode-map "\C-ce"      'c-func-to-extern)
  (define-key c-mode-map "\C-c\C-k"   'kill-compilation)
  (define-key c-mode-map "\C-c\C-m"   '(lambda () (interactive) (recenter 0)))
  (define-key c-mode-map "\C-cu"      'c-uncomment-line)
  (setq c-indent-level                4)
  (setq c-continued-statement-offset  4)
  (setq c-brace-offset                0)
  (setq c-continued-brace-offset      -4)
  (setq c-brace-imaginary-offset      0)
  (setq c-argdecl-indent              0)
  (setq c-label-offset                0)
  
  ;; useful X functions 
  ;; These give us a semi-xterm'ish cut and paste. Left button sets point
  ;; right button selects from point to mouse position, middle button
  ;; pastes at current mouse position. Shift right is like right, but it
  ;; also deletes the text being selected (for moves instead of copies)
  ;;
  (if (equal window-system 'x)
      (progn
	(require 'x-mouse)
	(define-key mouse-map x-button-left 'x-mouse-set-point)
	(define-key mouse-map x-button-left-up 'x-mouse-ignore)
	(define-key mouse-map x-button-middle 'x-paste-text)
	(define-key mouse-map x-button-middle-up 'x-mouse-ignore)
	(define-key mouse-map x-button-right 'x-cut-text)
	(define-key mouse-map x-button-right-up 'x-mouse-ignore)
	(define-key mouse-map x-button-s-left 'x-mouse-set-point)
	(define-key mouse-map x-button-s-middle 'x-paste-text)
	(define-key mouse-map x-button-s-right 'x-cut-and-wipe-text)
	))
  
  ;; Some definitions for GNUS
  (setq gnus-large-newsgroup 100)
  (setq gnus-Group-mode-hook 'my-gnus-kill-mode-string)
  (setq gnus-use-long-file-name t)
  (setq gnus-Subject-mode-hook 'my-gnus-kill-mode-string)
  (setq gnus-Article-mode-hook 'my-gnus-kill-mode-string)
  (setq gnus-nntp-server "news.santafe.edu")
  (setq gnus-your-domain "santafe.edu")
  (setq gnus-your-organization "Santa Fe Institute")
  (setq gnus-article-save-directory (expand-file-name "~/things"))
  (setq gnus-nntp-service 119)
  (setq gnus-use-cross-reference t)
  (setq gnus-default-distribution "world")
  (setq gnus-default-article-saver 'gnus-Subject-save-in-mail)
  (autoload 'gnus "gnus-hide-quote" "Read network news." t)
  (autoload 'gnus-post-news "gnuspost" "Post something to the net." t)
  
  (setq gnus-window-configuration
	'((SelectNewsgroup (0 1 0))
	  (ExitNewsgroup   (4 0 5))
	  (SelectArticle   (0 4 5))
	  (ExpandSubject   (0 1 0))))
  
  (put 'server-buffer-clients 'preserved t)
  (put 'narrow-to-region 'disabled nil)
  (setq target-buffer (get-buffer "*scratch*"))
)

;; call the startup function defined above.
(startup)
