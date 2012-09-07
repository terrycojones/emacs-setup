;;; -*-Emacs-Lisp-*-

(defvar i-use-viper-mode       nil         "True if we are using viper mode.")
(defvar i-use-cc-mode          t           "True if we are using cc mode.")
(defvar i-use-ding-gnus        t           "True if we are using ding Gnus.")
(defvar i-want-auto-raise      nil         "If true frames will auto raise.")
(defvar i-want-modeline-info   nil         "If true the date etc. appears in the modeline.")
(defvar i-want-line-numbers    t           "If true show the line number in the modeline.")
(defvar running-x              (equal window-system 'x) "True if we have X.")
(defvar color-x                (and running-x (x-display-color-p)) "True if we have X & color.")

(if color-x
    (progn
      (defvar i-use-hilit      nil "True if we are using hilit19.")
      (defvar i-use-font-lock  t   "True if we are using font lock.")))
      
(defun startup ()
  (interactive)

  ;;; MY ELISP DIR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (setq load-path (append load-path (list (expand-file-name "~/emacs/elisp"))))
  
  ;;; REQUIRES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require 'my-autoloads)
  (require 'my-misc)
  (require 'ehelp)
  (require 'crypt)

  ;;; MODELINE THINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if i-want-modeline-info
      (progn	
	(display-time)
	(or global-mode-string
	    (setq global-mode-string '("")))

	(setq
	 display-time-day-and-date t
	 global-mode-string (append
			     global-mode-string 
			     (list
			      " "
			      (substring
			       (system-name) 0 (string-match "\\." (system-name)))
			      "  ")))))
  
  ;;; SUPERCITE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'sc-cite-original "supercite" "Hookified Supercite 2.3" t)
  (autoload 'sc-cite "supercite" "Interactive Supercite 2.3" t)
  (add-hook 'mail-citation-hook 'sc-cite-original)
  (eval-after-load "supercite" '(require 'my-supercite))

  ;;; VM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'vm "vm" "VM mail reader" t)
  (autoload 'vm-mail "vm" "VM mail reader" t)

  ;;; ABBREVIATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; This must be done before loading dmacro.
  ;; otherwise it will clobber the abbrevs defined there.
  
  (define-abbrev-table 'global-abbrev-table
    '(("rememeber"   "remember"    nil 0)
      ("teh"         "the"         nil 0)
      ("fcat"        "fact"        nil 0)
      ("ahve"        "have"        nil 0)
      ("interseting" "interesting" nil 0)
      ("intial"      "initial"     nil 0)
      ("thasnk"      "thanks"      nil 0)
      ("taht"        "that"        nil 0)
      ("doen"        "done"        nil 0)))

  ;;; TEX/LATEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require 'tex-site)
  (and color-x
       i-use-font-lock
       (add-hook 'LaTeX-mode-hook 'turn-on-font-lock))
  (add-hook 'LaTeX-mode-hook 'my-latex-hook)
  (eval-after-load "tex" '(require 'my-latex))

  ;;; EDIFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
  (autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)

  ;;; PERL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'perl-mode "perl-mode")
  (and color-x
       i-use-font-lock
       (add-hook 'perl-mode-hook 'turn-on-font-lock))
  (add-hook
   'perl-mode-hook
   '(lambda ()
      (define-key perl-mode-map "\C-m" 'newline-and-indent)
      (define-key perl-mode-map "%"    'goto-matching-paren-or-insert)))

  ;;; HTML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq html-helper-new-buffer-template
	'("include(`/u/terry/html/m4/macros')\n"
	  "\n"
	  "m4_html_start(`')\n"
	  "\n"
	  "m4_html_end\n")
	
	html-helper-use-expert-menu t
	html-helper-do-write-file-hooks nil)
  
  (and color-x
       i-use-font-lock
       (add-hook 'html-helper-mode-hook 'turn-on-font-lock))
  
  ;;; DMACRO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require 'dmacro)
  (dmacro-load "~/emacs/elisp/my-dmacros")
  (setq
   auto-dmacro-alist (cons '("\\.h$" . dot-h) auto-dmacro-alist)
   auto-dmacro-alist (cons '("\\.c$" . dot-c) auto-dmacro-alist))

  ;;; TAGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (autoload 'tag-continue "tags" "Tag stack things" t)
  (autoload 'my-find-tag  "tags" "Tag stack things" t)
  (autoload 'tag-pop      "tags" "Tag stack things" t)

  (add-hook 'find-tag-hook 'my-find-tag-hook)
  
  (global-set-keys
   (list "\M-[" 'tag-continue
	 "\M-." 'my-find-tag
	 "\M-'" 'tag-pop))

  ;;; KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (and i-use-viper-mode
       (global-set-key "\C-z" 'vip-change-state-to-vi))
     
  (global-set-keys
   (list "\M-`"     'back-to-first-white
	 "\C-x\C-b" 'electric-buffer-list
	 "\C-h"     'ehelp-command
	 "\C-x\m"   'vm-mail
	 "\M-g"     'kill-white-after
	 "\M-/"     'hippie-expand
	 "\M-i"     'insert-buffer
	 [S-return] 'newline
	 [C-return] 'newline
	 [S-delete] 'backward-delete-char-untabify
	 [C-delete] 'backward-delete-char-untabify))

  (cond
   (running-x
    (global-set-keys
     (list [f3]     'save-buffer
	   [f4]     '(lambda () (interactive) (point-to-register ?x))
	   [f5]     '(lambda () (interactive) (jump-to-register ?x))
	   [f6]     'goto-line
	   [f7]     'undo
	   [f8]     '(lambda () (interactive) (recenter -1))
	   [f9]     'electric-buffer-list
	   [f10]    '(lambda () (interactive) (recenter 0))
	   [f11]    'set-mark-command                                   ; stop
	   [f12]    'previous-buffer                                    ; again
	   [f13]    'save-buffer                                        ; prop
	   [f14]    'undo                                               ; undo
	   [f15]    'delete-other-windows                               ; front
	   [f16]    'kill-ring-save                                     ; copy
	   [f17]    'TeX-recenter-output-buffer                         ; open
	   [f18]    (if (and color-x i-use-hilit) 'hilit-yank 'yank)    ; paste    
	   [f19]    'find-file                                          ; find
	   [pause]  'find-file                                          ; find
	   [f20]    'kill-region                                        ; cut
	   [f21]    'beginning-of-buffer                                ; pause
	   [f22]    'kill-this-buffer                                   ; PrSc
	   [f23]    'scroll-other-window-down                           ; scroll/lock/break
	   [f24]    'end-of-buffer                                      ; =
	   [f25]    'LaTeX-fill-paragraph                               ; /
	   [f26]    'scroll-other-window                                ; *
	   [f27]    '(lambda () (interactive) (move-to-window-line 0))  ; home
	   [f29]    'scroll-down                                        ; pgup
	   [f30]    'backward-char                                      ; left
	   [f31]    'move-to-window-line                                ; middle
	   [f32]    'forward-char                                       ; right
	   [f33]    '(lambda () (interactive) (move-to-window-line -1)) ; end
	   [f35]    'scroll-up                                          ; pgdn
	   [KP_3]   'scroll-up                                          ; pgdn
	   [insert] 'recenter)))                                        ; insert

   (t
    (global-set-keys
     (list "\C-c-" '(lambda () (interactive) (recenter -1))
	   "\C-c0" '(lambda () (interactive) (recenter 0))))))
  

  ;;; IMENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if running-x
      (progn
	(require 'imenu)
	(setq
	 imenu-sort-function 'imenu--sort-by-name
	 imenu-max-items 35)))

  ;;; TCL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'tcl-mode  "tcl-mode" "Sort-of minor mode for editing TCL code" t)
  
  (setq
   tcl-mode-hook
   '(lambda ()
      (setq
       tcl-indent 2
       indent-line-function 'tcl-indent-line
       gin-left-hang-indent-re "\\([-*#]\\|([a-zA-Z0-9])\\|[a-zA-Z0-9]\\.?:]?\\)"
       gin-retain-indent-re "[a-zA-Z]*#?>+\\( >\\)*[    ]*\\|[  ]*"
       comment-start "# "
       comment-start-skip "#+ "
       comment-indent-hook 'tcl-calculate-indent)
	   
      (local-set-key "\r" 'reindent-then-newline-and-indent)
      (local-set-key "%"  'goto-matching-paren-or-insert)
	   
      (if running-x
	  (progn
	    (abbrev-mode 1)
	    (defun tcl-proc-bold ()
	      (set-face-font 'bold (face-font 'default))
	      (condition-case nil
		  (make-face-bold 'bold)
		(error nil))
	      (overlay-put (make-overlay (- (point) 4) (point)) 'face 'bold))))))

  ;;; X WINDOWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if running-x
      (progn
        ;; (require 'paren)
	(transient-mark-mode t)
	(setq x-pointer-shape x-pointer-spider)
	(set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
	(if
	 i-want-auto-raise
	 (setq default-frame-alist (append default-frame-alist '((auto-raise . 1)))))

	(if color-x
	    (progn

	      (setq
	       default-frame-alist (append
				    default-frame-alist
				    '((background-color . "steelblue")
				      (foreground-color . "white")
				      (menu-background . "steelblue")
				      (cursor-color . "red")
				      (mouse-color . "black"))))
				      
	      (set-face-background 'highlight "hotpink")
	      (set-face-background 'region    "deepskyblue")
	      (set-face-background 'modeline  "cyan") 
	      (set-face-foreground 'modeline  "black")

	      (cond
	       (i-use-hilit
		(require 'hilit19)
		(eval-after-load "hilit19" '(require 'my-hilit)))

	       (i-use-font-lock
		(autoload 'turn-on-lazy-lock "lazy-lock" "Turn on Lazy Lock mode.")
		(autoload 'turn-on-defer-lock "defer-lock" "Turn on Defer Lock mode.")
		(add-hook
		 'font-lock-mode-hook
		 '(lambda ()
		    (turn-on-lazy-lock)
		    (turn-on-defer-lock)
		    (require 'font-lock-menu)))
		(eval-after-load "font-lock" '(require 'my-font-lock))))))))


  ;;; MISCELLANEOUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if i-want-auto-raise (auto-raise-mode t))

  (setq
   gc-cons-threshold             (* 1024 1024)
   kill-whole-line               t
   target-buffer                 (get-buffer "*scratch*")
   auto-save-timeout             15
   mark-even-if-inactive         t
   next-line-add-newlines        nil
   pop-up-windows                t
   blink-matching-paren-distance 20000
   default-major-mode            'text-mode
   enable-recursive-minibuffers  t
   inhibit-startup-message       t
   compile-command               "make"
   mail-archive-file-name        (expand-file-name "~/mail/OUT")
   mark-ring-max                 64
   kill-ring-max                 64
   fill-column                   200
   lpr-switches                  (list (concat "-P" (getenv "PRINTER"))))

  (put 'eval-expression 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'server-buffer-clients 'preserved t)

  ;;; MAIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (and color-x
       i-use-font-lock
       (add-hook 'mail-setup-hook 'turn-on-font-lock))

  (add-hook
   'mail-setup-hook
   '(lambda ()
      (mail-abbrevs-setup)
      (gin-mode t)))

  (add-hook
   'mail-setup-hook
   '(lambda ()
      (forward-line 2)
      (insert "Reply-To: terry@santafe.edu\n")
      (forward-line -3)
      (end-of-line)))

  ;;; FILE FINDING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (setq
   find-file-visit-truename      t
   find-file-existing-other-name t)
  
  (add-hook
   'find-file-hooks
   '(lambda ()
      (abbrev-mode t)
      (and i-want-line-numbers (line-number-mode t))
      (gin-mode t)))

  (add-hook
   'find-file-not-found-hooks
   '(lambda ()
      (abbrev-mode t)
      (and i-want-line-numbers (line-number-mode t))
      (gin-mode t)))
  
  ;;; FILE SUFFIXES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq
   auto-mode-alist
   (append
    '(("\\.C$"     . c++-mode)
      ("\\.cc$"    . c++-mode)
      ("\\.H$"     . c++-mode)
      ("\\.hh$"    . c++-mode)
      ("\\.c$"     . c-mode)
      ("\\.h$"     . c-mode)
      ("\\.m$"     . objc-mode)
      ("\\.html$"  . html-helper-mode)
      ("\\.htm4l$" . html-helper-mode)
      ("\\.htm$"   . html-helper-mode)
      ("\\.t$"     . tcl-mode))
    auto-mode-alist))

  ;;; C, CC AND OBJECTIVE C  MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (cond
   (i-use-cc-mode
	 
    (fmakunbound 'c-mode)
    (makunbound  'c-mode-map)
    (fmakunbound 'c++-mode)
    (makunbound  'c++-mode-map)
    (makunbound  'c-style-alist)

    (autoload 'c-mode    "cc-mode"  "C Editing Mode" t)
    (autoload 'c++-mode  "cc-mode"  "C++ Editing Mode" t)
    (autoload 'objc-mode "cc-mode"  "Objective-C Editing Mode" t)

    (and color-x
	 i-use-font-lock
	 (add-hook 'c-mode-common-hook 'turn-on-font-lock))
    (add-hook-if-x 'c-mode-common-hook '(lambda () (imenu-add-to-menubar "Functions")))
    
    (add-hook
     'c-mode-common-hook
     '(lambda ()
	(alter-c-mode-bindings)
	(setq
	 c-basic-offset 4
	 c-inhibit-startup-warnings-p nil)
	(c-set-offset 'case-label           '+)
	(c-set-offset 'label                 0)
	(c-set-offset 'statement-case-intro '+)
	(c-set-offset 'knr-argdecl-intro     0)
	(c-set-offset 'statement-cont       'c-lineup-math))))

   (t
	
    (setq
     c-indent-level                4
     c-continued-statement-offset  4
     c-brace-offset                0
     c-continued-brace-offset      -4
     c-brace-imaginary-offset      0
     c-argdecl-indent              0
     c-label-offset                0)))

  ;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (and color-x
       i-use-font-lock
       (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock))
  
  (define-keys
    emacs-lisp-mode-map
    (list "\C-m"     'newline-and-indent
	  [S-return] 'newline-and-indent
	  [C-return] 'newline-and-indent
	  "%"        'goto-matching-paren-or-insert))

  ;;; GNUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if i-use-ding-gnus
      (setq
       load-path (append
		  (list
		   "/network/software/packages/emacs/share/emacs/site-lisp/ding-gnus")
		  load-path)))

  (eval-after-load "gnus" '(require 'my-gnus))

  (add-hook 'news-reply-header-hook   '(lambda () (gin-mode t)))
  (add-hook 'gnus-group-mode-hook     'my-gnus-kill-mode-string)
  (add-hook 'gnus-article-mode-hook   'my-gnus-kill-mode-string)

  ;;; LISPDIR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'format-lisp-code-directory    "lispdir"       nil t)
  (autoload 'lisp-dir-apropos              "lispdir"       nil t)
  (autoload 'lisp-dir-retrieve             "lispdir"       nil t)
  (autoload 'lisp-dir-verify               "lispdir"       nil t)
)

;; call the startup function defined above.
(startup)
