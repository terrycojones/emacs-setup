;; -*-Emacs-Lisp-*-
(defvar my-primary-buffer-name "*scratch*" "The buffer switched to by default.")

(defconst i-use-viper-mode               nil "True if we are using viper mode.")
(defconst i-use-w3                       nil "True if we are using w3 mode.")
(defconst i-use-tm                       t   "Use tm to do mime things.")
(defconst i-want-modeline-time           nil "Show the time in the modeline.")
(defconst i-want-modeline-time-and-date  nil "Show the time and date in the modeline.")
(defconst i-want-modeline-host-name      nil "Show the host name in the modeline.")
(defconst i-want-modeline-line-numbers   t   "Show the line number in the modeline.")
(defconst i-want-modeline-column-numbers t   "Show the column number in the modeline.")
(defconst i-want-menus                   t   "Display a menu bar in X.")
(defconst i-want-scroll-bars             nil "Display a scroll bar in X.")
(defconst i-want-menus-in-text-mode      nil "Display a menu bar in text mode.")

(defconst running-x                      (equal window-system 'x) "True if we have X.")

(defconst color-x                        (and
					  running-x
					  (x-display-color-p)) "True if we have X & color.")

(if color-x
    (progn
      (defconst i-use-hilit     nil "True if we are using hilit19.")
      (defconst i-use-font-lock t   "True if we are using font lock.")))
      
(defun startup ()
  (interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MY ELISP DIR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (setq load-path (append load-path (list
				     (expand-file-name "~/emacs/elisp"))))

  (if i-use-w3
      (setq load-path (append load-path (list
					 "/usr/local/gnu/emacs/share/emacs/site-lisp/w3"
					 "/usr/local/gnu/emacs/share/emacs/site-lisp/url"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INFO DIRS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (setq
   Info-directory-list
   (list "/usr/local/gnu/emacs/site-info"
	 "/usr/local/gnu/emacs/info"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REQUIRES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require 'my-autoloads)
  (require 'my-misc)
  (require 'my-unscroll)
  (require 'my-buffers)
  (require 'my-keys)
  ;; (require 'ehelp)
  (require 'crypt)
  ;; (require 'mouse-sel)
  ;; (load "accel" t t)
  (require 'id-select)

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MIME SHIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if i-use-tm
   (progn
     (load "mime-setup")

     (call-after-loaded
      'tm-view
      (function
       (lambda ()
	 (require 'tm-pgp))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODELINE THINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (or global-mode-string
      (setq global-mode-string '("")))

  (and i-want-modeline-time-and-date
       (setq display-time-day-and-date t)
       (setq i-want-modeline-time t))
     
  (and i-want-modeline-time
       (display-time))

  (and i-want-modeline-host-name
       (setq
	global-mode-string
	(append
	 global-mode-string 
	 (list
	  " "
	  (substring
	   (system-name) 0 (string-match "\\." (system-name)))
	  "  "))))

  (and (not i-want-modeline-line-numbers)
       (line-number-mode nil))

  (and i-want-modeline-column-numbers
       (column-number-mode t))

  (if running-x
      (progn
	(and (not i-want-menus)
	     (menu-bar-mode nil))
	(and (not i-want-scroll-bars)
	     (scroll-bar-mode nil)))
    (and (not i-want-menus-in-text-mode)
	 (menu-bar-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SUPERCITE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'sc-cite-original "supercite" "Hookified Supercite 2.3" t)
  (autoload 'sc-cite "supercite" "Interactive Supercite 2.3" t)
  (add-hook 'mail-citation-hook 'sc-cite-original)
  (eval-after-load "supercite" '(require 'my-supercite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CYCLE BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; (add-hook 'cycle-buffer-load-hook
	    ;; '(lambda ()
	       ;; (setq
		;; cycle-buffer-reset-after 2
		;; cycle-buffer-show        1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'vm "vm" "VM mail reader" t)
  (autoload 'vm-mail "vm" "VM mail reader" t)

  ;; mailcrypt things.
  (add-hook 'vm-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-mail-mode-hook 'mc-install-write-mode)
  
  ;; (add-hook 'vm-select-message-hook 'rmime-format)
  ;; (autoload 'rmime-format "rmime" "" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ABBREVIATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; This must be done before loading dmacro.
  ;; otherwise it will clobber the abbrevs defined there.
  
  (define-abbrev-table 'global-abbrev-table
    '(("rememeber"   "remember"    nil 0)
      ("teh"         "the"         nil 0)
      ("enetered"    "entered"     nil 0)
      ("eneter"      "enter"       nil 0)
      ("fcat"        "fact"        nil 0)
      ("ahve"        "have"        nil 0)
      ("interseting" "interesting" nil 0)
      ("intial"      "initial"     nil 0)
      ("thasnk"      "thanks"      nil 0)
      ("taht"        "that"        nil 0)
      ("doen"        "done"        nil 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEX/LATEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; (require 'tex-site)
  ;; (and color-x
  ;;      i-use-font-lock
  ;;      (add-hook 'LaTeX-mode-hook 'turn-on-font-lock))
  ;; (add-hook 'LaTeX-mode-hook 'my-latex-hook)
  ;; (eval-after-load "tex" '(require 'my-latex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EDIFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
  (autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PERL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'perl-mode "perl-mode")
  (and color-x
       i-use-font-lock
       (add-hook 'perl-mode-hook 'turn-on-font-lock))
  (add-hook
   'perl-mode-hook
   '(lambda ()
      (define-key perl-mode-map "\C-m" 'newline-and-indent)
      (define-key perl-mode-map "%"    'goto-matching-paren-or-insert)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (and color-x
       i-use-font-lock
       (add-hook
	'm4-mode-hook
	'(lambda ()
	   (turn-on-font-lock)
	   (require 'my-html))))


  (setq
   html-helper-new-buffer-template nil
   html-helper-use-expert-menu t
   html-helper-do-write-file-hooks nil)
  
  (and color-x
       i-use-font-lock
       (add-hook 'html-helper-mode-hook 'turn-on-font-lock))

  (add-hook 'html-helper-mode-hook
	    (lambda ()
	      (require 'my-html)
	      (setq html-helper-basic-offset 1)
	      
	      ;; Undo what html-mode does to ?' because it's important in htm4l
	      (modify-syntax-entry ?` "('" html-helper-mode-syntax-table)
	      (modify-syntax-entry ?' "(`" html-helper-mode-syntax-table)

	      (define-keys
		html-helper-mode-map
		(list 
		 "\C-c'" "&`#'39;"
		 "\C-c`" "&`#'96;")))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DMACRO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require 'dmacro)
  (dmacro-load "~/emacs/elisp/my-dmacros")
  (setq
   auto-dmacro-alist (cons '("\\.h$" . dot-h) auto-dmacro-alist)
   auto-dmacro-alist (cons '("\\.c$" . dot-c) auto-dmacro-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (autoload 'tag-continue "tags" "Tag stack things" t)
  (autoload 'my-find-tag  "tags" "Tag stack things" t)
  (autoload 'tag-pop      "tags" "Tag stack things" t)

  (add-hook 'find-tag-hook 'my-find-tag-hook)
  
  (define-keys
    global-map
    (list "\M-[" 'tag-continue
	  "\M-." 'my-find-tag
	  "\M-'" 'tag-pop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq my-keymap (make-keymap "terry"))

  (define-keys
    global-map
    (list
     [f11] my-keymap
     [f15] my-keymap
     [f17] my-keymap
     [f19] my-keymap
     ))
  

  (define-keys
    my-keymap
    (list
     "0"     'delete-window
     "1"     'delete-other-windows
     "2"     'split-window-vertically
     "a"     'iso-accents-mode
     "b"     'electric-buffer-list
     "c"     'kill-region
     "e"     'end-of-buffer
     "f"     'find-file
     "g"     'goto-line
     "h"     'mark-whole-buffer
     "i"     'insert-buffer
     "k"     'kill-this-buffer
     "l"     'eval-last-sexp
     "m"     'start-or-end-kbd-macro
     "n"     'make-frame-command
     "o"     'other-window
     "p"     'previous-buffer
     "r"     'rename-buffer
     "q"     'fill-paragraph
     "r"     'gnus
     "s"     'save-buffer
     "t"     'beginning-of-buffer
     "u"     'undo
     "v"     'vm-mail
     "w"     'kill-white-after
     "y"     'yank
     "/"     'hippie-expand
     "+"     'plus-to-eof
     ">"     'gt-to-eof
     "."     'call-last-kbd-macro
     ","     'set-mark-command
     "`"     'font-lock-mode
     "/"     '(lambda () (interactive) (set-mark-command 1))
     "-"     '(lambda () (interactive) (recenter -1))
     "="     '(lambda () (interactive) (recenter 0))
     "B"     '(lambda ()
		(interactive)
		(find-file (expand-file-name "~/.bashrc"))
		(end-of-buffer))
     "C"     '(lambda () (interactive) (switch-to-buffer "*compilation*"))
     "E"     'visit-my-emacs-startup-file
     "M"     '(lambda () (interactive) (switch-to-buffer "*Messages*"))
     "P"     '(lambda ()
		(interactive)
		(find-file (expand-file-name "~/.phonebook"))
		(end-of-buffer))
     "S"     '(lambda () (interactive) (switch-to-buffer "*scratch*"))
     ))
  
  (and i-use-viper-mode
       (global-set-key "\C-z" 'vip-change-state-to-vi))
     
  (define-keys
    global-map
    (list "\M-`"      'set-my-primary-buffer-name
	  "\C-x\C-b"  'electric-buffer-list
	  "\C-x\m"    'vm-mail
	  "\M-g"      'kill-white-after
	  "\M-/"      'hippie-expand
	  "\M-i"      'insert-buffer
	  ))

  (cond
   (running-x
    (define-keys
      global-map
      (list [f3]     'save-buffer
	    [f4]     '(lambda () (interactive) (point-to-register ?x))
	    [f5]     '(lambda () (interactive) (jump-to-register ?x))
	    [f6]     'goto-line
	    [f7]     'start-or-end-kbd-macro
	    [f8]     '(lambda () (interactive) (recenter -1))
	    [f9]     'electric-buffer-list
	    [f10]    '(lambda () (interactive) (recenter 0))
	    ;; [SunF36] 'iso-accents-mode	; stop
	    ;; [f11]    'iso-accents-mode	; stop
	    [f12]    'previous-buffer	; again
	    [SunF37] 'previous-buffer	; F12 on a sun5 keybd
	    [f13]    'save-buffer	; prop
					; [KP_1]   'save-buffer
	    [f14]    'undo		; undo
	    ;; [f15]    'delete-other-windows ; front
	    [f16]    'kill-ring-save	; copy
	    ;; [f17]    'TeX-recenter-output-buffer ; open
	    ;; [f18]    (if (and color-x i-use-hilit) 'hilit-yank 'yank) ; paste    
	    ;; [f19]    'find-file		; find
	    [f20]    'kill-region	; cut
	    [f21]    'find-file		; pause
	    [f22]    'kill-this-buffer	; PrSc
	    [f23]    'unscroll ; scroll/lock/break
	    [f24]    'end-of-buffer	; =
	    ;; [f25]    'LaTeX-fill-paragraph ; /
	    [f26]    'scroll-other-window ; *
	    [f27]    '(lambda () (interactive) (move-to-window-line 0)) ; home
	    [f29]    'scroll-down	; pgup
	    [f30]    'backward-char	; left
	    [f31]    'move-to-window-line ; middle
	    [f32]    'forward-char	; right
	    [f33]    '(lambda () (interactive) (move-to-window-line -1)) ; end
	    [f35]    'scroll-up		; pgdn
	    [print]  'kill-this-buffer	; print
	    [pause]  'find-file		; pause
	    [SunPowerSwitch] 'set-mark-command
	    [insert] 'recenter)))	; insert

   (t
    (define-keys
      global-map
      (list "\C-c-" '(lambda () (interactive) (recenter -1))
	    "\C-c0" '(lambda () (interactive) (recenter 0))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if running-x
      (progn
	(require 'imenu)
	(setq
	 imenu-sort-function 'imenu--sort-by-name
	 imenu-max-items 35)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TCL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
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

  (and color-x
       i-use-font-lock
       (add-hook 'tcl-mode-hook 'turn-on-font-lock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HIPPIE EXPAND ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq hippie-expand-try-functions-list
	'(try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-all-abbrevs
	  try-expand-line
	  try-expand-list
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-dabbrev-from-kill
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; X WINDOWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if running-x
      (progn
	;; (require 'paren)
	(transient-mark-mode t)
	(setq
	 x-pointer-shape x-pointer-hand1)
	;; (setq default-frame-alist (append default-frame-alist '((auto-raise . 1))))
	(set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))

	(if color-x
	    (progn

	      ;; (setq
	      ;; default-frame-alist (append
	      ;; default-frame-alist
	      ;; '((background-color . "cornflowerblue")
	      ;; (foreground-color . "white")
	      ;; (menu-background . "steelblue")
	      ;; (cursor-color . "red")
	      ;; (mouse-color . "black"))))
				      
	      (set-face-background 'highlight "hotpink")
	      (copy-face 'highlight 'isearch)
	      (set-face-background 'isearch   "yellowgreen")
	      (set-face-background 'region    "deepskyblue")
	      (set-face-background 'modeline  "cyan") 
	      (set-face-foreground 'modeline  "black")

	      (cond
	       (i-use-hilit
		(require 'hilit19)
		(eval-after-load "hilit19" '(require 'my-hilit)))

	       (i-use-font-lock
		(autoload 'turn-on-lazy-lock "lazy-lock" "Turn on Lazy Lock mode.")
		(add-hook
		 'font-lock-mode-hook
		 '(lambda ()
		    (turn-on-lazy-lock)
		    (require 'font-lock-menu)))
		(eval-after-load "font-lock" '(require 'my-font-lock))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISCELLANEOUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (auto-raise-mode nil)

  (setq
   mc-passwd-timeout             99999999
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
   mail-default-reply-to         "terry@teclata.es"
   mark-ring-max                 64
   kill-ring-max                 64
   fill-column                   200
   lpr-switches                  (list (concat "-P" (getenv "PRINTER"))))

  (put 'eval-expression 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'server-buffer-clients 'preserved t)

  ;;;;;;;;;;;;;;;;;; LANGUAGE & ACCENTS ;;;;;;;;;;;;;;

  (standard-display-european t)
  (require 'iso-acc)

  (setq iso-languages
	(append
	 iso-languages
	 '(("catspa" ; A superset of Catalan and Spanish.
	    (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
		(?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363) (?u . ?\372)
		(?\  . ?'))
	    (?` (?A . ?\300) (?E . ?\310) (?O . ?\322)
		(?a . ?\340) (?e . ?\350) (?o . ?\362)
		(?\  . ?`))
	    (?\" (?I . ?\317) (?U . ?\334) (?i . ?\357) (?u . ?\374) (?\  . ?\"))
	    (?\~ (?C . ?\307) (?N . ?\321) (?c . ?\347) (?n . ?\361)
		 (?> . ?\273) (?< . ?\253) (?! . ?\241) (?? . ?\277)
		 (?\  . ?\~))))))

  (iso-accents-customize "catspa")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (and color-x
       i-use-font-lock
       (add-hook 'mail-setup-hook 'turn-on-font-lock))

  (add-hook
   'mail-setup-hook
   '(lambda ()
      (mail-abbrevs-setup)
      ;; (iso-accents-mode)
      (gin-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FILE FINDING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (setq
   find-file-visit-truename      t
   find-file-existing-other-name t)
  
  (add-hook
   'find-file-hooks
   '(lambda ()
      (abbrev-mode t)
      (gin-mode t)))

  (add-hook
   'find-file-not-found-hooks
   '(lambda ()
      (abbrev-mode t)
      (gin-mode t)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FILE SUFFIXES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq
   auto-mode-alist
   (append
    '(("\\.C$"         . c++-mode)
      ("\\.cc$"        . c++-mode)
      ("\\.c\.m4$"     . c++-mode)
      ("\\.h\.m4$"     . c++-mode)
      ("\\.H$"         . c++-mode)
      ("\\.hh$"        . c++-mode)
      ("\\.l$"         . c-mode)
      ("\\.m$"         . objc-mode)
      ("\\.html$"      . html-helper-mode)
      ("\\.htm4l$"     . html-helper-mode)
      ("\\.html\\.m4$" . html-helper-mode)
      ("\\.perl$"      . perl-mode)
      ("\\.perl\.m4$"  . perl-mode)
      ("\\.cgi$"       . perl-mode)
      ("\\.cgi\.m4$"   . perl-mode)
      ("\\.ph$"        . perl-mode)
      ("\\.t$"         . tcl-mode)
      ("\\.m4$"        . m4-mode))
    auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;; C, CC AND OBJECTIVE C MODES ;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (c-set-offset 'statement-cont       'c-lineup-math)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (and color-x
       i-use-font-lock
       (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock))
  
  (define-keys
    emacs-lisp-mode-map
    (list "\C-m"     'newline-and-indent
	  [S-return] 'newline-and-indent
	  [C-return] 'newline-and-indent
	  "%"        'goto-matching-paren-or-insert))

  (define-keys
    lisp-interaction-mode-map
    (list "\C-m"     'newline-and-indent
	  [S-return] 'newline-and-indent
	  [C-return] 'newline-and-indent
	  "%"        'goto-matching-paren-or-insert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GNUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require 'my-gnus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LISPDIR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'format-lisp-code-directory    "lispdir"       nil t)
  (autoload 'lisp-dir-apropos              "lispdir"       nil t)
  (autoload 'lisp-dir-retrieve             "lispdir"       nil t)
  (autoload 'lisp-dir-verify               "lispdir"       nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq
   vc-default-back-end 'CVS
   vc-initial-comment  nil
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ISEARCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-keys
    isearch-mode-map
    (list "\C-m" 'isearch-repeat-forward
	  ))

;;; CLOSE
  )

;; call the startup function defined above.
(startup)