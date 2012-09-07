;; -*-Emacs-Lisp-*-

(defconst i-use-viper-mode               nil "True if I use viper mode.")
(defconst i-use-w3                       nil "True if I use w3 mode.")
(defconst i-use-tm                       nil "Use tm to do mime things.")
(defconst i-use-apel                     nil "Use apel (mainly to do mime things).")
(defconst i-use-flim                     nil "Use flim (to do mime things).")
(defconst i-use-semi                     nil "Use semi (to do mime things).")
(defconst i-use-semi-gnus                nil "Use semi-gnus.")
(defconst i-want-modeline-time           nil "Show the time in the modeline.")
(defconst i-want-modeline-time-and-date  nil "Show the time and date in the modeline.")
(defconst i-want-modeline-host-name      nil "Show the host name in the modeline.")
(defconst i-want-modeline-line-numbers   t   "Show the line number in the modeline.")
(defconst i-want-modeline-column-numbers t   "Show the column number in the modeline.")
(defconst i-want-menus                   t   "Display a menu bar in X.")
(defconst i-want-scroll-bars             nil "Display a scroll bar in X.")
(defconst i-want-menus-in-text-mode      nil "Display a menu bar in text mode.")
(defconst i-use-shell-mode               t   "True if I use shell mode. Duh.")
(defconst i-use-dired                    t   "True if I use dired. Duh.")
(defconst i-use-tags                     nil "True if I use tags.")
(defconst i-use-tcl                      nil "True if I use TCL.")

(defconst running-x                      (equal window-system 'x) "True if we have X.")

(defconst color-x                        (and
					  running-x
					  (x-display-color-p)) "True if I have X & color.")

(defconst i-use-font-lock                color-x "True if I use font lock.")
      
(defun startup ()
  (interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MY ELISP DIR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq
   load-path (append
	      load-path
	      (list (expand-file-name "~/emacs/elisp"))))

  (setq
   load-path (append
	      (list (expand-file-name "/usr/local/gnu/emacs/share/emacs/site-lisp"))
	      load-path))
  
  (if i-use-w3
      (setq
       load-path (append
		  load-path
		  (list "/usr/local/gnu/emacs/share/emacs/site-lisp/w3"
			"/usr/local/gnu/emacs/share/emacs/site-lisp/url"))))

  (if i-use-apel
      (setq
       load-path (append
		  load-path
		  (list "/usr/local/gnu/emacs/share/emacs/site-lisp/apel"))))
  
  (if i-use-flim
      (setq
       load-path (append
		  load-path
		  (list "/usr/local/gnu/emacs/share/emacs/site-lisp/flim"))))
  
  (if i-use-semi
      (setq
       load-path (append
		  load-path
		  (list "/usr/local/gnu/emacs/share/emacs/site-lisp/semi"))))
  
  (if i-use-semi-gnus
      (progn
	(require 'path-util)
	(setq
	 load-path (append
		    (list "/usr/local/gnu/emacs/share/emacs/site-lisp/semi-gnus")
		    load-path))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INFO DIRS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (setq
   Info-directory-list
   (list "/usr/local/gnu/emacs/site-info"
	 "/usr/local/gnu/emacs/info"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REQUIRES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require 'my-keys)
  ;; (require 'my-mac-keys)
  (require 'my-autoloads)
  (require 'my-misc)
  (require 'my-unscroll)
  (require 'my-buffers)
  ;; (require 'ehelp)
  (require 'crypt+pgp-pub)
  ;; (require 'mouse-sel)
  ;; (load "accel" t t)
  (require 'id-select)
  ;; (require 'my-filecache)
  (require 'find-func)
  (require 'my-gnus)

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MIME SHIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if i-use-tm
      (progn
	(autoload 'mime/editor-mode "tm-edit" "Minor mode for editing MIME message." t)
	;; (add-hook 'mail-mode-hook 'mime/editor-mode)
	;; (add-hook 'mail-send-hook 'mime-editor/maybe-translate)

	(require 'my-gnus)
	(load "mime-setup")

	(call-after-loaded
	 'tm-view
	 (function
	  (lambda ()
	    (require 'tm-pgp))))))

  (if i-use-semi
      (progn
	(setq mime-edit-split-message nil)
	(add-hook 'mail-mode-hook 'turn-on-mime-edit)
	(add-hook 'mail-send-hook 'mime-edit-maybe-translate)
	(add-hook 'news-reply-mode-hook 'turn-on-mime-edit)
	(add-hook 'news-inews-hook 'mime-edit-maybe-translate)))
  
  
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
	     (menu-bar-mode -1))
	(and (not i-want-scroll-bars)
	     (scroll-bar-mode -1)))
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
      ("serach"      "search"      nil 0)
      ("intial"      "initial"     nil 0)
      ("thasnk"      "thanks"      nil 0)
      ("taht"        "that"        nil 0)
      ("doen"        "done"        nil 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FONTLOCK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if i-use-font-lock
      (progn
	(require 'my-font-lock)
	(global-font-lock-mode t)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEX/LATEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; (require 'tex-site)
  ;; (add-hook 'LaTeX-mode-hook 'my-latex-hook)
  ;; (eval-after-load "tex" '(require 'my-latex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EDIFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
  (autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PERL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (autoload 'perl-mode "perl-mode")
  (add-hook
   'perl-mode-hook
   '(lambda ()
      (define-key perl-mode-map "\C-m" 'newline-and-indent)
      (define-key perl-mode-map "%"    'goto-matching-paren-or-insert)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (and color-x
       (add-hook
	'm4-mode-hook
	'(lambda ()
	   (define-key m4-mode-map "%" 'goto-matching-paren-or-insert)
	   (require 'my-html))))

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
		 "\C-c'" "`'__SQ`'")))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DMACRO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (require 'dmacro)
  (dmacro-load "~/emacs/elisp/my-dmacros")
  (setq
   auto-dmacro-alist (cons '("\\.h$" . dot-h) auto-dmacro-alist)
   auto-dmacro-alist (cons '("\\.c$" . dot-c) auto-dmacro-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if i-use-tags
      (require 'my-tags))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SHELL && COMINT ;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if i-use-shell-mode
      (require 'my-shell))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIRED ;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if i-use-dired
      (require 'my-dired))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if running-x
      (progn
	(require 'imenu)
	(setq
	 imenu-sort-function 'imenu--sort-by-name
	 imenu-max-items 35)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TCL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if i-use-dired
      (require 'my-tcl))

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
	      (set-face-foreground 'modeline  "black")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISCELLANEOUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (auto-raise-mode nil)

  (setq
   gnus-nntp-server                 "news.ucsd.edu"
   crypt-encryption-type            'pgp
   scroll-preserve-screen-position  t
   ;; scroll-conservatively            0
   ;; scroll-margin                    0
   mc-passwd-timeout                99999999
   gc-cons-threshold                (* 1024 1024)
   kill-whole-line                  t
   target-buffer                    (get-buffer "*scratch*")
   auto-save-timeout                15
   mark-even-if-inactive            t
   next-line-add-newlines           nil
   pop-up-windows                   t
   blink-matching-paren-distance    20000
   default-major-mode               'text-mode
   enable-recursive-minibuffers     t
   inhibit-startup-message          t
   compile-command                  "make"
   mail-archive-file-name           (expand-file-name "~/mail/OUT")
   mail-default-reply-to            "terry@hci.ucsd.edu"
   mark-ring-max                    64
   kill-ring-max                    64
   fill-column                      200
   lpr-switches                     (list (concat "-P" (getenv "PRINTER"))))

  (put 'eval-expression 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'server-buffer-clients 'preserved t)
  (put 'truncate-lines 'permanent-local t)

  ;;;;;;;;;;;;;;;;;; LANGUAGE & ACCENTS ;;;;;;;;;;;;;;

  (standard-display-european t)
  (require 'iso-acc)

  (setq iso-languages
	(append
	 iso-languages
	 '(("catspa"			; A superset of Catalan and Spanish.
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

  ;; Make a bunch of Catalan and Spanish chars into normal word chars, quotes & punctuation.
  (mapcar
   (lambda (c) (modify-syntax-entry c "w"))
   '(?á ?à ?Á ?À ?é ?è ?É ?È ?í ?Í ?ï ?Ï ?ó ?ò ?Ó ?Ò ?ú ?Ú ?ü ?Ü ?ñ ?Ñ ?ç ?Ç))
  (mapcar (lambda (c) (modify-syntax-entry c "\"")) '(?« ?»))
  (mapcar (lambda (c) (modify-syntax-entry c ".")) '(?¡ ?¿))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
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
    '(
      ("\\.\\([cChHl]\\)\\1?\\(\\.fix\\)?\\(\\.m4\\)?$"           . c-mode)
      ("\\.m$"                                                    . objc-mode)
      ("\\.htm4?l?\\(\\.fix\\)?\\(\\.m4\\)?$"                     . html-helper-mode)
      ("\\.\\(perl\\|cgi\\|ph\\|pl\\)\\(\\.fix\\)?\\(\\.m4\\)?$"  . perl-mode)
      ("\\.t$"                                                    . tcl-mode))
    auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;; C, CC AND OBJECTIVE C MODES ;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq c-style-variables-are-local-p nil)
  
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

  (define-keys
    emacs-lisp-mode-map
    (list "\C-m"     'newline-and-indent
	  "%"        'goto-matching-paren-or-insert
	  "\C-c\C-c"   '(lambda ()
			  (interactive)
			  (byte-compile-file (buffer-file-name)))
	  ))

  (define-keys
    lisp-interaction-mode-map
    (list "\C-m"     'newline-and-indent
	  "%"        'goto-matching-paren-or-insert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ISEARCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-keys
    isearch-mode-map
    (list "\C-m" 'isearch-repeat-forward
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq
   vc-default-back-end 'RCS
   vc-initial-comment  nil
   )

;;; CLOSE
  )

;; Now do things to start up, including calling the startup function above.

(startup)
