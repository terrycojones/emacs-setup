;; -*-Emacs-Lisp-*-

(defconst my-location                    (or (getenv "LOCATION") "unknown") "Where are we?")
(defconst my-host                        (or (getenv "HOST") "unknown") "Where are we?")
(defconst my-site-lisp-directory         (expand-file-name "~/emacs/site-lisp"))
(defconst my-keyboard                    'pc "Used by emacs/elisp/my-keys.el")
(defconst i-use-bbdb                     nil "True if I use BBDB.")
(defconst i-use-gnuserv                  nil "True if I use gnuserv.")
(defconst i-use-tramp                    t   "True if I use tramp.")
(defconst i-use-diary                    nil "True if I use diary mode.")
(defconst i-use-viper-mode               nil "True if I use viper mode.")
(defconst i-want-modeline-time           t   "Show the time in the modeline.")
(defconst i-want-modeline-time-and-date  t   "Show the time and date in the modeline.")
(defconst i-want-modeline-host-name      nil "Show the host name in the modeline.")
(defconst i-want-modeline-line-numbers   t   "Show the line number in the modeline.")
(defconst i-want-modeline-column-numbers t   "Show the column number in the modeline.")
(defconst i-want-menus                   t   "Display a menu bar in X.")
(defconst i-want-scroll-bars             nil "Display a scroll bar in X.")
(defconst i-want-menus-in-text-mode      nil "Display a menu bar in text mode.")
(defconst i-use-shell-mode               nil "True if I use shell mode. Duh.")
(defconst i-use-dired                    t   "True if I use dired. Duh.")
(defconst i-use-tags                     nil "True if I use tags.")
(defconst i-use-tcl                      nil "True if I use TCL.")
(defconst i-use-hideshow                 nil "True if I use hideshow.")
(defconst i-use-speedbar                 nil "True if I use speedbar.")
(defconst i-use-tex                      nil "True if I use TeX.")
(defconst i-use-oe                       nil "True if I use my Object Exchange.")
(defconst i-use-flyspell                 t   "True if I use my flyspell mode.")
(defconst i-use-cmu-scheme               nil "True if I use CMU Scheme.")
(defconst i-use-w3                       nil "True if I use w3.")
(defconst i-use-w3m                      nil   "True if I use w3m.")
(defconst i-use-semi                     nil "True if I use semi.")
(defconst i-use-franz                    t "True if I use Franz lisp.")
(defconst i-use-ess                      nil "True if I use ESS (emacs interface to R).")

(defconst running-x                      (equal window-system 'x) "True if we have X.")

(defconst color-x                        (and
										  running-x
										  (x-display-color-p)) "True if I have X & color.")

(defconst i-use-font-lock                color-x "True if I use font lock.")

(when running-x
  (tool-bar-mode 0)
  (defconst my-initial-width 100 "Initial width for new frames.")
  (defconst my-initial-height 48 "Initial height for new frames.")
  (defconst my-default-width 100 "Default width for new frames.")
  (defconst my-default-height 48 "Default height for new frames."))



; (defmacro if-true (var)
;   `(and (boundp ',var) ,var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOAD PATH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 load-path (append
	    load-path
	    (list
		 (expand-file-name "~/emacs/elisp")
		 my-site-lisp-directory
		 (concat my-site-lisp-directory "/vm")
		 (concat my-site-lisp-directory "/bbdb")
		 (concat my-site-lisp-directory "/ess")
		 (expand-file-name "~/local/emacs/share/emacs/site-lisp/w3m"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INFO DIRS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
  Info-directory-list
  (cond
   ((or
     (string-equal my-location "borne")
     (string-equal my-location "eatoni")) (list (expand-file-name "~/l/info")
												(expand-file-name "~/local/emacs/info")
												"/usr/share/info"))
   ((string-equal my-location "bornemac") (list (expand-file-name "~/l/info")
												(expand-file-name "~/local/emacs/info")
												"/sw/share/info"
												"/usr/share/info"))
   ((string-equal my-location "hci") (list "/usr/local/gnu/emacs/site-info"
										   "/usr/local/gnu/emacs/info"))
   (t (list "/usr/share/info"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REQUIRES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-keys)
;; (require 'my-mac-keys)
(require 'my-autoloads)
(require 'my-misc)
;; (require 'my-unscroll)
(require 'my-buffers)
;; (require 'ehelp)
(require 'crypt+pgp-pub)
;; (require 'mouse-sel)
;; (load "accel" t t)
(require 'id-select)
;; (require 'my-filecache)
(require 'find-func)
(require 'my-gnus)
;; (require 'mailabbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MOUSE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mouse-wheel-mode t)
(setq mouse-wheel-scroll-amount '(5 . 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODELINE THINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(or
 global-mode-string
 (setq global-mode-string '("")))

(when
    i-want-modeline-time-and-date
  (setq display-time-day-and-date t)
  (setq i-want-modeline-time t))

(when
    i-want-modeline-time
  (setq calendar-time-display-form '(24-hours ":" minutes))
  (display-time))

(when
    i-want-modeline-host-name
  (setq
   global-mode-string
   (append
    global-mode-string 
    (list
     " "
     (substring
      (system-name) 0 (string-match "\\." (system-name)))
     "  "))))

(when
    (not i-want-modeline-line-numbers)
  (line-number-mode nil))

(when
    i-want-modeline-column-numbers
  (column-number-mode t))

(if running-x
    (progn
      (and
       (not i-want-menus)
       (menu-bar-mode -1))
      (and
       (not i-want-scroll-bars)
       (scroll-bar-mode -1)))

  (when
      (not i-want-menus-in-text-mode)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SPEEDBAR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when i-use-speedbar
  (require 'speedbar)
  (speedbar-add-supported-extension ".m4")
  (speedbar-add-supported-extension ".y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'vm "vm" "VM mail reader" t)
(autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
(autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
(autoload 'vm-mail "vm" "Send a mail message using VM." t)

;; mailcrypt things.
(add-hook 'vm-mode-hook 'mc-install-read-mode)
(add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
(add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
(add-hook 'vm-mail-mode-hook 'mc-install-write-mode)

(and
 (boundp 'i-use-flyspell)
 i-use-flyspell
 (add-hook 'vm-mail-mode-hook '(lambda () (flyspell-mode t))))


(load-library "mailcrypt")
(mc-setversion "gpg")

;; (add-hook 'vm-select-message-hook 'rmime-format)
;; (autoload 'rmime-format "rmime" "" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ABBREVIATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This must be done before loading dmacro.
;; otherwise it will clobber the abbrevs defined there.

(define-abbrev-table 'global-abbrev-table
  '(("rememeber"   "remember"    nil 0)
    ("teh"         "the"         nil 0)
    ("enetered"    "entered"     nil 0)
    ("appraoch"    "approach"    nil 0)
    ("indiacte"    "indicate"    nil 0)
    ("indiacted"   "indicated"   nil 0)
    ("langauge"    "language"    nil 0)
    ("langauges"   "languages"   nil 0)
    ("eneter"      "enter"       nil 0)
    ("fcat"        "fact"        nil 0)
    ("desno"       "denso"       nil 0)
    ("ahve"        "have"        nil 0)
    ("interseting" "interesting" nil 0)
    ("interset"    "interest"    nil 0)
    ("serach"      "search"      nil 0)
    ("intial"      "initial"     nil 0)
    ("thasnk"      "thanks"      nil 0)
    ("taht"        "that"        nil 0)
    ("doen"        "done"        nil 0)))

;; Franz

(and
 (boundp 'i-use-franz)
 i-use-franz
 (require 'my-franz))

;; ESS

(and
 (boundp 'i-use-ess)
 i-use-ess
 (require 'ess-site)
 (require 'my-ess))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FONTLOCK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(and
 (boundp 'i-use-font-lock)
 i-use-font-lock
 (progn
   (require 'my-font-lock)
   (global-font-lock-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; W3M ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(and
  (boundp 'i-use-w3m)
  i-use-w3m
  (require 'w3m)
  (setq w3m-use-cookies t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEX/LATEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when i-use-tex
  (require 'tex-site)
  (require 'font-latex)
  (add-hook 'LaTeX-mode-hook 'my-latex-hook)
  (and
   (boundp 'i-use-flyspell)
   i-use-flyspell
   (add-hook 'LaTeX-mode-hook '(lambda () (flyspell-mode t))))
  (eval-after-load "tex" '(require 'my-latex))
  (autoload 'turn-on-bib-cite "bib-cite")
  (add-hook 'LaTeX-mode-hook 'turn-on-bib-cite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EDIFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HIDESHOW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(and
 (boundp 'i-use-hideshow)
 i-use-hideshow
 (setq hs-show-hidden-short-form nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAKE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'makefile-mode-hook
 '(lambda ()
    (define-keys
      makefile-mode-map
      (list
       "\C-c\C-c" 'compile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PERL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'perl-mode-hook
 '(lambda ()
    (and i-use-hideshow 'hs-minor-mode)
    (define-keys
      perl-mode-map
      (list
       "\C-c\C-c" 'compile
       "\C-m"     'newline-and-indent
       "%"        'goto-matching-paren-or-insert))))

(add-hook
 'cperl-mode-hook
 '(lambda ()
    (setq cperl-indent-level 4)
    (define-keys
      cperl-mode-map
      (list
       "\C-c\C-c" 'compile
       "\C-m"     'newline-and-indent
       "%"        'goto-matching-paren-or-insert))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCHEME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(and
 (boundp 'i-use-cmu-scheme)
 i-use-cmu-scheme
 (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme" t)
 (setq
  scheme-program-name "stk"))

(add-hook
 'scheme-mode-hook
 '(lambda ()
    (define-keys
      scheme-mode-map
      (list
       "\C-c\C-c" 'compile
       "\C-m"     'newline-and-indent
       "%"        'goto-matching-paren-or-insert))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(and color-x
     (add-hook
      'm4-mode-hook
      '(lambda ()
	 (define-keys
	   m4-mode-map
	   (list
	    "%"        'goto-matching-paren-or-insert
	    "\C-c\C-c" 'compile))))
     (require 'my-html))

(setq
 html-helper-new-buffer-template nil
 html-helper-use-expert-menu t
 html-helper-do-write-file-hooks nil)

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
	       "\C-c\C-c" 'compile
	       "\C-c'"    "`'__SQ`'")))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DMACRO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dmacro)
(dmacro-load "~/emacs/elisp/my-dmacros")
(setq
 auto-dmacro-alist (cons '("\\.pl$" . perlmain) auto-dmacro-alist)
 auto-dmacro-alist (cons '("\\.h$" . dot-h) auto-dmacro-alist)
 auto-dmacro-alist (cons '("\\.c$" . dot-c) auto-dmacro-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 todo-file-do   (expand-file-name "~/TODO")
 todo-file-done (expand-file-name "~/TODO.done")
 todo-file-top  (expand-file-name "~/TODO.top")
 todo-insert-threshold 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when i-use-tags
  (require 'my-tags))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SHELL && COMINT ;;;;;;;;;;;;;;;;;;;;;;;;;;

(when i-use-shell-mode
  (require 'my-shell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIRED ;;;;;;;;;;;;;;;;;;;;;;;;;;

(when i-use-dired
  (require 'my-dired))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when running-x
  (require 'imenu)
  (setq
   imenu-sort-function 'imenu--sort-by-name
   imenu-max-items 35))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TCL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when i-use-tcl
  (require 'my-tcl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HIPPIE EXPAND ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-line
	try-expand-dabbrev-all-buffers
	try-expand-all-abbrevs
	try-expand-list
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-dabbrev-from-kill
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; X WINDOWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when running-x
  ;; (require 'paren)
  (transient-mark-mode t)

  (cond
   ((string-match my-host "mallet")
    (setq
     my-initial-height 40
     my-default-height 40
     my-initial-width 125
     my-default-width 125))
   ((string-match my-host "terry")
    (setq
     my-initial-height 49
     my-default-height 49
     my-initial-width 101
     my-default-width 101))
   ((string-match my-host "tapdance")
    (setq
     my-initial-height 63
     my-default-height 63
     my-initial-width 100
     my-default-width 100)))
    
  (setq
   x-pointer-shape x-pointer-hand1

   initial-frame-alist (append
						(assq-delete-all 'height
										 (assq-delete-all 'width  initial-frame-alist))
						
						`((width  . ,my-initial-width)
						  (height . ,my-initial-height)))

   default-frame-alist (append
						(assq-delete-all 'height
										 (assq-delete-all 'width  default-frame-alist))
						
						`((width  . ,my-default-width)
						  (height . ,my-default-height))))

  
  (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
  
  (when color-x
    
    ;; (setq
    ;; default-frame-alist (append
    ;; default-frame-alist
    ;; '((background-color . "cornflowerblue")
    ;; (foreground-color . "white")
    ;; (menu-background . "steelblue")
    ;; (cursor-color . "red")
    ;; (mouse-color . "black"))))
    
    ;; (set-face-background 'highlight "hotpink")
    ;; (copy-face 'highlight 'isearch) 
    ;; (set-face-background 'isearch   "yellowgreen")
    ;; (set-face-background 'region    "deepskyblue")
    ;; (set-face-background 'modeline  "cyan") 
    ;; (set-face-foreground 'modeline  "black")
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISCELLANEOUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (auto-raise-mode nil)

(set-default 'tab-width 4)

(setq
 backward-delete-char-untabify-method nil
 browse-url-browser-function (quote browse-url-generic)
 browse-url-generic-args          '("-newpage")
 browse-url-new-window-p nil
 browse-url-generic-program "opera"
 browse-url-of-file-hook (quote (browse-url-generic-reload))
 ;; url-temporary-directory "~/tmp"
 ;;browse-url-netscape-program      "opera"
 ;;browse-url-browser-function      '(("^mailto:" . browse-url-mail)
 ;;     				    ("." . browse-url-netscape))				     
 ;; browse-url-browser-function      'browse-url-kde
 
 truncate-partial-width-windows   nil
 ;; calendar-latitude                32.5  ; San Diego
 ;; calendar-longitude               117.0 ; San Diego
 calendar-latitude                41.3853 ; Barcelona
 calendar-longitude                2.1774 ; Barcelona
 calendar-location-name           "Barcelona"
 ;; calendar-latitude                 40.714  ; New York
 ;; calendar-longitude               -74.006  ; New York
 ;; calendar-location-name           "New York"
 mail-host-address                "jones.tc"
 ;; mail-host-address                "eatoni.com"
 mail-archive-file-name           (expand-file-name "~/mail/OUT")
 mail-default-reply-to            "tc.jones@jones.tc"
 ;; mail-default-reply-to            "terry@eatoni.com"
 ;; mail-default-reply-to            "kirilov66@jones.tc"
 mail-from-style                  'system-default
 visible-bell                     t
 gnus-nntp-server                 "news.arrakis.es"
 ;; gnus-nntp-server                 (if (string= (getenv "LOCATION") "eatoni") "news.globix.com" "news.arrakis.es")
 crypt-encryption-type            'gpg
 scroll-preserve-screen-position  t
 scroll-up-aggressively           0.75
 scroll-down-aggressively         0.75
 ;; scroll-conservatively            0
 ;; scroll-margin                    0
 mc-passwd-timeout                99999999
 gc-cons-threshold                (* 1024 1024)
 kill-whole-line                  t
 auto-save-timeout                15
 mark-even-if-inactive            t
 next-line-add-newlines           nil
 pop-up-windows                   t
 blink-matching-paren-distance    20000
 default-major-mode               'text-mode
 enable-recursive-minibuffers     nil
 inhibit-startup-message          t
 compile-command                  "make"
 mark-ring-max                    64
 kill-ring-max                    64
 fill-column                      200
 lpr-command                      "lpr"
 lpr-switches                     (list (concat "-P" (getenv "PRINTER")))
 ps-lpr-command                   "lpr"
 ps-lpr-switches                  (list (concat "-P" (getenv "PRINTER"))))

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'server-buffer-clients 'preserved t)
(put 'truncate-lines 'permanent-local t)

;;;;;;;;;;;;;;;;;; LANGUAGE & ACCENTS ;;;;;;;;;;;;;;

(set-language-environment "Latin-1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'mail-setup-hook
 '(lambda ()
    (mail-abbrevs-setup)
    
    ;; These should be only done once, but where to put it?
    ;; (set-face-foreground 'message-header-cc-face          "orange")
    ;; (set-face-foreground 'message-header-name-face        "orange")
    ;; (set-face-foreground 'message-header-subject-face     "orange")
    
    ;; (iso-accents-mode)
    (gin-mode t)))

(and
 (boundp 'i-use-flyspell)
 i-use-flyspell
 (add-hook 'mail-setup-hook '(lambda ()
			       (flyspell-mode t)
			       (define-key flyspell-mode-map "\M-\C-i" nil))))

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

(and
 (boundp 'i-use-oe)
 i-use-oe
 (require 'my-oe)
 (oe-open)
 (oe-alive-p)
 (add-hook 'find-file-hooks 'oe-send-current-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FILE SUFFIXES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 auto-mode-alist
 (append
  '(("\\.\\([cChHl]\\)\\1?\\(\\.m4\\)?$"           . c-mode)
    ("\\.tex\\(\\.m4\\)?$"                         . LaTeX-mode)
    ("\\.java\\(\\.m4\\)?$"                        . java-mode)
    ("\\.m$"                                       . objc-mode)
    ("\\.htm4?l?\\(\\.m4\\)?$"                     . html-helper-mode)
    ("\\.\\(perl\\|cgi\\|ph\\|pl\\)\\(\\.m4\\)?$"  . perl-mode)
    ("\\.\\(stklos\\|scm\\|stk\\)\\(\\.m4\\)?$"    . scheme-mode)
    ("\\.tk?$"                                     . tcl-mode)
	("\\.save$"                                    . lisp-mode))
  auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;; C, CC AND OBJECTIVE C MODES ;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-style-variables-are-local-p nil)

(add-hook-if-x 'c-mode-common-hook '(lambda () (imenu-add-to-menubar "Functions")))

(and
 i-use-hideshow
 (add-hook
  'c-mode-hook 'hs-minor-mode))

(add-hook
 'c-mode-common-hook
 '(lambda ()
    (setq
     c-basic-offset                     4
     c-inhibit-startup-warnings-p       nil)
    (c-set-offset 'case-label           '+)
    (c-set-offset 'label                 0)
    (c-set-offset 'statement-case-intro '+)
    (c-set-offset 'knr-argdecl-intro     0)
    (c-set-offset 'statement-cont       'c-lineup-math)))

(add-hook
 'c-mode-hook
 '(lambda ()
    ;; (imenu-add-to-menubar "Functions")
    (alter-c-mode-bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JAVA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'java-mode-hook
 '(lambda ()
    (alter-java-mode-bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-keys
  emacs-lisp-mode-map
  (list "\C-m"     'newline-and-indent
	"%"        'goto-matching-paren-or-insert
	"\C-c\C-c" '(lambda ()
		      (interactive)
		      (byte-compile-file (buffer-file-name)))))

(and
 i-use-hideshow
 (add-hook
  'emacs-mode-hook 'hs-minor-mode))

(define-keys
  lisp-interaction-mode-map
  (list
   "\C-m"     'newline-and-indent
   "%"        'goto-matching-paren-or-insert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ISEARCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-keys
;;   isearch-mode-map
;;   (list
;;    "\C-m" 'isearch-repeat-forward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
 vc-initial-comment  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when
    i-use-diary
  (add-hook 'diary-hook 'appt-make-list)
  ;; (setq appt-display-diary nil)
  (setq appt-issue-message nil)
  (add-hook 'diary-display-hook 'fancy-diary-display)
  (setq diary-list-include-blanks t)
  (add-hook 'list-diary-entries-hook 'sort-diary-entries t)
  (add-hook 'list-diary-entries-hook 'include-other-diary-files)
  (add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
  (add-hook 'today-visible-calendar-hook 'calendar-mark-today))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GNUSERV ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when
    i-use-gnuserv
  (load "gnuserv")
  (gnuserv-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BBDB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when
    i-use-bbdb
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message 'vm 'sendmail)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
  (add-hook 'mail-setup-hook 'bbdb-define-all-aliases)
  (add-hook 'message-send-hook 'mc-bbdb-auto-encrypt-send)
  (load-library "mc-bbdb")
  ;; (bbdb-insinuate-message)
  ;; (bbdb-insinuate-w3)
  ;; (bbdb-insinuate-sc)

  (setq
   bbdb-pop-up-display-layout 'one-line
   bbdb-pop-up-target-lines 1

   bbdb-ignore-most-messages-alist '(("From: " . "alerts@citibank.com")
				     ("From: " . "root@")
				     ("From: " . "MAILER-DAEMON@")
				     ("To: " . "@securityfocus.com")
				     ("To: " . "discuss@wine.codeweavers.com")
				     ("X-Spam-Flag: ". "YES")
				     )
  

   bbdb/mail-auto-create-p 'bbdb-prune-not-to-me
   bbdb/news-auto-create-p 'bbdb-prune-not-to-me
   )

  (defun bbdb-prune-not-to-me ()
    "defun called when bbdb is trying to automatically create a record.  Filters out
anything not actually adressed to me then passes control to 'bbdb-ignore-some-messages-hook'.
Also filters out anything that is precedense 'junk' or 'bulk'  This code is from
Ronan Waide <waider@waider.ie"
    (let ((case-fold-search t)
	  (done nil)
	  (b (current-buffer))
	  (marker (bbdb-header-start))
	  field regexp fieldval)
      (set-buffer (marker-buffer marker))
      (save-excursion
	;; The buffer we're in is the mail file, narrowed to the current message.
	(let (to cc precedence)
	  (goto-char marker)
	  (setq to (bbdb-extract-field-value "To"))
	  (goto-char marker)
	  (setq cc (bbdb-extract-field-value "Cc"))
	  (goto-char marker)
	  (setq precedence (bbdb-extract-field-value "Precedence"))
	  ;; Here's where you put your email information.
	  ;; Basically, you just add all the regexps you want for
	  ;; both the 'to' field and the 'cc' field.
	  (if (and (not (string-match "terry@" (or to "")))
		   (not (string-match "tc.jones@" (or cc ""))))
	      (progn
		(message "BBDB unfiling; message to: %s cc: %s" (or to "no-one") (or cc "no-one"))
		;; Return nil so that the record isn't added.
		nil)

	    (if (string-match "junk" (or precedence ""))
		(progn
		  (message "precedence set to junk, bbdb ignoring.")
		  nil)

	      ;; Otherwise add, subject to filtering
	      (bbdb-ignore-some-messages-hook)))))))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRAMP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when
    i-use-tramp
  (require 'tramp)
  (setq tramp-default-method "scp")
  )
