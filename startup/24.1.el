;; -*-Emacs-Lisp-*-

(defconst my-site-lisp-directory         (expand-file-name "~/emacs/site-lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOAD PATH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prepend
(add-to-list 'load-path (expand-file-name "~/emacs/elisp"))
(add-to-list 'load-path my-site-lisp-directory)
(add-to-list 'load-path (concat my-site-lisp-directory "/vm/share/emacs/site-lisp/vm"))
(add-to-list 'load-path (concat my-site-lisp-directory "/js2-mode"))
(add-to-list 'load-path (concat my-site-lisp-directory "/fluiddb"))

;; Append
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mailcrypt" t)

(defconst my-location                    (or (getenv "LOCATION") "bornemac") "Where are we?")
(defconst my-host                        (or (getenv "HOST") "unknown") "Where are we?")
(defconst my-keyboard                    'pc "Used by emacs/elisp/my-keys.el")
(defconst i-use-gin-mode                 nil "True if I use gin mode.")
(defconst i-use-html-helper-mode         nil "True if I use HTML helper mode.")
(defconst i-use-bbdb                     nil "True if I use BBDB.")
(defconst i-use-server                   t "True if I use server.")
(defconst i-use-gnuserv                  nil "True if I use gnuserv.")
(defconst i-use-tramp                    t   "True if I use tramp.")
(defconst i-use-diary                    nil "True if I use diary mode.")
(defconst i-use-viper-mode               nil "True if I use viper mode.")
(defvar   i-want-modeline-time           t   "Show the time in the modeline.")
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
(defconst i-use-w3m                      nil "True if I use w3m.")
(defconst i-use-semi                     nil "True if I use semi.")
(defconst i-use-franz                    nil "True if I use Franz lisp.")
(defconst i-use-ess                      nil "True if I use ESS (emacs interface to R).")
(defconst i-use-nxml                     nil "True if I use nxml mode (emacs XML mode).")
(defconst i-use-smtpmail                 nil "True if I use smtpmail to send mail.")
(defconst i-want-transparent-frames      t "True if I want frames to be transparent. Only works in Aquamacs")
(defconst i-use-slime                    nil "True if I use Slime lisp interaction mode.")
(defconst i-use-fluidinfo-dev            t "True if I use Fluidinfo development mode.")
(defconst i-use-fsf-python-mode          nil "True if I use the Python mode from the FSF.")
(defconst i-use-python-mode              nil "True if I use python-mode (peters, warsaw, etc).")
(defconst i-use-python-el                t "True if I use python.el.")
(defconst i-use-ipython                  nil "True if I use IPython.")
(defconst i-use-twitter                  t "True if I use Twitter.")
(defconst i-use-pymacs                   nil "True if I use Pymacs - the emacs/python interface.")
(defconst i-use-fluiddb                  t "True if I use Holger Durer's FluidDB interface.")
(defconst i-use-dpaste                   nil "True if I paste things with dpaste")
(defconst i-use-edit-server              t "True if I use the Chromium emacs edit service")
(defconst i-use-js2-mode                 t "True if I use the emacs JS2 javascript mode.")
(defconst i-use-go-mode                  t "True if I use Go mode.")
(defconst i-use-imenu                    t "True if I use imenu.")

;; (defconst running-x                      t "True if we have X.")
;; (defconst color-x                        t "True if I have X & color.")
(defconst running-x                      (equal window-system 'x) "True if we have X.")

(defconst color-x                        (and
                                          running-x
                                          (x-display-color-p)) "True if I have X & color.")

; ----- CONSISTENCY CHECKS -----
(if (and
     (boundp 'i-use-gnuserv)
     (boundp 'i-use-server)
     i-use-gnuserv
     i-use-server)
    (error "You cannot use both gnuserv and server"))

; --------
(defconst i-use-font-lock                color-x "True if I use font lock.")

(when running-x
  (tool-bar-mode 0)
  (defvar my-initial-width 100)
  (defvar my-initial-height 48)
  (defvar my-default-width 100)
  (defvar my-default-height 48))

; (defmacro if-true (var)
;   `(and (boundp ',var) ,var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INFO DIRS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'Info-default-directory-list
;;             (expand-file-name "/usr/local/share/info"))

;; (setq
;;   Info-directory-list
;;   (cond
;;    ((or
;;      (string-equal my-location "borne")
;;      (string-equal my-location "eatoni")) (list (expand-file-name "~/l/info")
;;        (expand-file-name "~/local/emacs/info")
;;        "/usr/share/info"))
;;    ((string-equal my-location "bornemac") (list (expand-file-name "~/l/info")
;; 	(expand-file-name "~/local/emacs/info")
;; 	"/sw/share/info"
;; 	"/usr/share/info"))
;;    ((string-equal my-location "hci") (list "/usr/local/gnu/emacs/site-info"
;; 	"/usr/local/gnu/emacs/info"))
;;    (t (list "/usr/share/info"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REQUIRES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'my-font)
;; (require 'my-mac-keys)
(require 'my-autoloads)
(require 'my-keys)
(require 'my-misc)
;; (require 'my-unscroll)
(require 'my-buffers)
;; (require 'ehelp)
;; (require 'crypt+pgp-pub)
;; (require 'mouse-sel)
;; (load "accel" t t)
;; (require 'id-select)
;; (require 'my-filecache)
;; (require 'find-func)
;; (require 'my-gnus)
;; (require 'mailabbrev)

;;; FONT ;;;

;; (set-bitstream-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MOUSE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(mouse-wheel-mode t)
;(setq mouse-wheel-scroll-amount '(5 . 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODELINE THINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(or
 global-mode-string
 (setq global-mode-string '("")))

(when
    i-want-modeline-time-and-date
  (setq display-time-day-and-date t)
  (setq i-want-modeline-time t)
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mac OS X aquamacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (boundp 'aquamacs-version-id)
    (progn
      (defun fix-process-environment ()
	(interactive)
	(let (new)
	  (dolist (v process-environment)
	    (setq new
		  (append new (if (string-match "^[[:alpha:]_][[:alnum:]_]*=()\\s-{.*[^}[:space:]]\\s-*$" v) nil (list v)))))
	  (setq process-environment new)))

      (create-fontset-from-fontset-spec
       "-apple-courier-medium-r-normal--16-*-*-*-*-*-fontset-mac, ascii:-apple-courier-medium-r-normal--16-*-*-*-*-*-mac-roman, latin-iso8859-1:-apple-courier-medium-r-normal--16-*-*-*-*-*-mac-roman, mule-unicode-0100-24ff:-apple-courier-medium-r-normal--16-*-*-*-*-*-ac-roman")
      (set-frame-font "-apple-courier-medium-r-normal--16-*-*-*-*-*-fontset-mac" 'keep)
      (add-to-list 'default-frame-alist '(font . "-apple-courier-medium-r-normal--16-*-*-*-*-*-fontset-mac"))


      (if (and
	   (boundp 'i-want-transparent-frames)
	   i-want-transparent-frames)
	  (let*
	      ((active 0.9)
	       (inactive 0.6)
	       (new `((active-alpha  . ,active) (inactive-alpha . ,inactive))))

	    (setq initial-frame-alist
                  (append (assq-delete-all
                           'active-alpha
                           (assq-delete-all
                            'inactive-alpha
                            initial-frame-alist))
                          new)

                  default-frame-alist
                  (append (assq-delete-all
                           'active-alpha
                           (assq-delete-all
                            'inactive-alpha
                            default-frame-alist))
                          new))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SPEEDBAR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (require 'speedbar))

(when i-use-speedbar
  (require 'speedbar)
  (speedbar-add-supported-extension ".m4")
  (speedbar-add-supported-extension ".y"))

; GO

(if (and
     (boundp 'i-use-go-mode)
     i-use-go-mode)
    (progn
      (add-to-list 'load-path (concat my-site-lisp-directory "/go") t)
      (require 'go-mode-load)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JS2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and
     (boundp 'i-use-js2-mode)
     i-use-js2-mode)
    (progn
      (require 'js-comint)
      (setq-default js2-basic-offset 4)
      (setq
       js2-enter-indents-newline t
       js2-consistent-level-indent-inner-bracket-p t
       js2-pretty-multiline-decl-indentation-p t
       inferior-js-program-command "/usr/bin/rhino"
       js2-electric-keys '()
       js2-auto-indent-flag nil)

      (add-hook 'js2-mode-hook
                '(lambda ()
                   ;; (require 'js2-highlight-vars)
                   ;; (if (featurep 'js2-highlight-vars)
                   ;;     (js2-highlight-vars-mode))
                   (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                   (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                   (local-set-key "\C-cb" 'js-send-buffer)
                   (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                   (local-set-key "\C-cl" 'js-load-file-and-go)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EDIT SERVER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and
     (boundp 'i-use-edit-server)
     i-use-edit-server
     (locate-library "edit-server"))
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame t)
      (edit-server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq vm-init-file (expand-file-name "~/.vm.el"))

(require 'vm-autoloads)

;; (autoload 'vm "vm" "VM mail reader" t)
;; (autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
;; (autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
;; (autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
;; (autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
;; (autoload 'vm-mail "vm" "Send a mail message using VM." t)

(load-library "mailcrypt-init")
(mc-setversion "gpg")

;; mailcrypt things.
(add-hook 'vm-mode-hook 'mc-install-read-mode)
(add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
(add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
(add-hook 'vm-mail-mode-hook 'mc-install-write-mode)

(and
 (boundp 'i-use-flyspell)
 i-use-flyspell
 (add-hook 'vm-mail-mode-hook '(lambda () (flyspell-mode t))))

(add-hook 'vm-mail-mode-hook '(lambda () (require 'my-vm)))


;; (add-hook 'vm-select-message-hook 'rmime-format)
;; (autoload 'rmime-format "rmime" "" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'rst-mode-hook '(lambda () (flyspell-mode t)))

;;; DPASTE ;;;

(and
 (boundp 'i-use-dpaste)
 i-use-dpaste
 (progn
   (require 'dpaste nil)
   ;; (global-set-key (kbd "C-c p") 'dpaste-region-or-buffer)
   (setq dpaste-poster "Terry Jones (terry@jon.es)")))


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

(if (and
     (boundp 'i-use-franz)
     i-use-franz)
    (define-key global-map "\C-xl" 'run-allegro))

;; Slime

(when (and
       (boundp 'i-use-slime)
       i-use-slime)
  (setq inferior-lisp-program (expand-file-name "~/mds/acl/mac/acl80/alisp"))
  (require 'slime)
  (slime-setup)
  (global-set-key "\C-cs" 'slime-selector))

;; ESS

(when
    (and
     (boundp 'i-use-ess)
     i-use-ess)
  (require 'ess-site)
  (require 'my-ess))

;; SMTPMAIL

(eval-when-compile
  (require 'my-smtpmail))

(when
    (and
     (boundp 'i-use-smtpmail)
     i-use-smtpmail)
  (require 'my-smtpmail)
  (set-smtpmail-server "cambridge")
  )

(setq send-mail-function 'sendmail-send-it)

;;; NXML ;;;

(when
    (and
     (boundp 'i-use-nxml)
     i-use-nxml)
  (load "rng-auto")
  ;(setq nxml-sep-element-flag t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FLUIDDB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when
    (and
     (boundp 'i-use-fluiddb)
     i-use-fluiddb)
  (require 'fluiddbinterface))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FONTLOCK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when
    (and
     (boundp 'i-use-font-lock)
     i-use-font-lock)
  ;; (require 'my-font-lock)
  (global-font-lock-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; W3M ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (eval-when-compile
;;   (require 'w3m))

;; (when
;;     (and
;;      (boundp 'i-use-w3m)
;;      i-use-w3m)
;;   (require 'w3m)
;;   (setq w3m-use-cookies t))

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

;; (eval-when-compile
;;   (require 'hideshow))

;; (and
;;  (boundp 'i-use-hideshow)
;;  i-use-hideshow
;;  (setq hs-show-hidden-short-form nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAKE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'makefile-mode-hook
 '(lambda ()
    (define-keys
      makefile-mode-map
      (list
       "\C-c\C-c" 'compile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FSF PYTHON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and
     (boundp 'i-use-fsf-python-mode)
     i-use-fsf-python-mode)

    (defun py-narrow-to-class nil
      (interactive)
      (save-excursion
        (py-beginning-of-def-or-class t)
        (let
            ((start (point)))
          (py-end-of-def-or-class)
          (narrow-to-region (point) start))))

    (add-hook
     'python-mode-hook
     '(lambda ()
        (setq show-trailing-whitespace t)
        (define-keys
          python-mode-map
          (list
           "\C-m" 'newline-and-indent
           )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PYTHON MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when
    (and
     (boundp 'i-use-python-mode)
     i-use-python-mode)
  (setq py-default-interpreter "ipython"
        mode-name "Py"
        py-install-directory (concat my-site-lisp-directory "/python-mode")
	;; py-python-command "ipython"
	py-shell-name "ipython")
  (require 'python-mode)
  (require 'whitespace)

  ;; Face for long lines' tails
  (set-face-attribute 'whitespace-line nil
                      :background "red1"
                      :foreground "yellow"
                      ;; :weight 'bold
                      )

  ;; Face for Tabs
  ;; (set-face-attribute 'whitespace-tab nil
  ;; :background "red1"
  ;; :foreground "yellow"
  ;; :weight 'bold)

  (add-hook 'python-mode-hook
            '(lambda ()
               ;; (define-key python-mode-map [(meta f)] 'py-forward-into-nomenclature)
               ;; (define-key python-mode-map [(meta b)] 'py-backward-into-nomenclature)
               (whitespace-mode t)
               (which-func-mode t)
               (setq show-trailing-whitespace t
                     py-electric-comment-p nil)))
  (setq whitespace-line-column 79
        whitespace-style '(tabs lines-tail))
  ;; (add-hook 'before-save-hook 'whitespace-cleanup)
  ;; (add-hook 'write-file-functions 'delete-trailing-whitespace)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PYTHON-EL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when
    (and
     (boundp 'i-use-python-el)
     i-use-python-el)
  (add-to-list 'load-path (concat my-site-lisp-directory "/python-el"))
  (require 'python)
  (require 'whitespace)

  ;; Face for long lines' tails
  (set-face-attribute 'whitespace-line nil
                      :background "red1"
                      :foreground "yellow")
  (add-hook
   'python-mode-hook
   '(lambda ()
      ;; (define-key python-mode-map [(meta f)] 'py-forward-into-nomenclature)
      ;; (define-key python-mode-map [(meta b)] 'py-backward-into-nomenclature)
      (whitespace-mode t)
      ;; (which-func-mode t)
      (setq show-trailing-whitespace t)))
  (setq
   whitespace-line-column 79
   whitespace-style '(tabs lines-tail)
   python-shell-virtualenv-path (expand-file-name "~/.virtualenvs/emacs")
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PYTHON PYDB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq pdb-path '/usr/bin/pydb
      gud-pdb-command-name (symbol-name pdb-path))

;; http://lists.gnu.org/archive/html/help-gnu-emacs/2003-10/msg00577.html
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
			    (file-name-nondirectory buffer-file-name)))))

; --------------------------------- IPYTHON ---------------------------------

;; Note that ipython requires python-mode.
(when (and
       (boundp 'i-use-ipython)
       i-use-ipython)
  
  (require 'ipython))

; ----------------------------------- PYMACS --------------------------------

;; (if (and
;;      (boundp 'i-use-pymacs)
;;      i-use-pymacs)
;;     (progn
;;       (autoload 'pymacs-load "pymacs" nil t)
;;       (autoload 'pymacs-eval "pymacs" nil t)
;;       (autoload 'pymacs-apply "pymacs")
;;       (autoload 'pymacs-call "pymacs")

;;       (pymacs-load "bikeemacs" "brm-")
;;       (brm-init)))


; --------------------------------- TWITTER ---------------------------------

(when (and
       (boundp 'i-use-twitter)
       i-use-twitter)
  (require 'my-twitter))

; -------------------------------- FLUIDINFO --------------------------------

(if (and
     (boundp 'i-use-fluidinfo-dev)
     i-use-fluidinfo-dev)
    (progn
      (setq
       load-path (append
                  load-path
                  (list
                   (expand-file-name "~/fluidinfo/other"))))
      (require 'fluidinfo-dev)))

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
    (setq cperl-indent-level 4
	  cperl-merge-trailing-else nil)
    (define-keys
      cperl-mode-map
      (list
       "\C-c\C-c" 'compile
       "\C-m"     'newline-and-indent
       "%"        'goto-matching-paren-or-insert))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCHEME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'cmuscheme))

(and
 (boundp 'i-use-cmu-scheme)
 i-use-cmu-scheme
 (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme" t)
 (setq
  scheme-program-name "stk")

 (add-hook
  'scheme-mode-hook
  '(lambda ()
     (define-keys
       scheme-mode-map
       (list
	"\C-c\C-c" 'compile
	"\C-m"     'newline-and-indent
	"%"        'goto-matching-paren-or-insert)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when
    color-x
  (add-hook
   'm4-mode-hook
   '(lambda ()
      (define-keys
	m4-mode-map
	(list
	 "%"        'goto-matching-paren-or-insert
	 "\C-c\C-c" 'compile))))
  (require 'my-html))

;; Zap html-helper-mode from magic-mode-alist
;; (labels
;;     ((filter-cdr (l omit)
;;        (cond
;;         ((not l) nil)
;;         ((eq (cdar l) omit) (filter-cdr (cdr l) omit))
;;         ((cons (car l) (filter-cdr (cdr l) omit))))))
;;   (setq magic-mode-alist (filter-cdr magic-mode-alist 'html-helper-mode)))

;; (when
;;     (and
;;      (boundp 'i-use-html-helper-mode)
;;      i-use-html-helper-mode)
;;   (eval-when-compile
;;     (require 'html-helper-mode))

;;   (setq
;;    html-helper-new-buffer-template nil
;;    html-helper-use-expert-menu t
;;    html-helper-do-write-file-hooks nil)

;;   (add-hook 'html-helper-mode-hook
;; 	    (lambda ()
;; 	      (require 'my-html)
;; 	      (setq html-helper-basic-offset 1)

;; 	      ;; Undo what html-mode does to ?' because it's important in htm4l
;; 	      (modify-syntax-entry ?` "('" html-helper-mode-syntax-table)
;; 	      (modify-syntax-entry ?' "(`" html-helper-mode-syntax-table)

;; 	      (define-keys
;; 		html-helper-mode-map
;; 		(list
;; 		 "\C-c\C-c" 'compile
;; 		 "\C-c'"    "`'__SQ`'")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DMACRO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'dmacro)
;; (dmacro-load "~/emacs/elisp/my-dmacros")
;; (setq
;;  auto-dmacro-alist (cons '("\\.pl$" . perlmain) auto-dmacro-alist)
;;  auto-dmacro-alist (cons '("\\.h$" . dot-h) auto-dmacro-alist)
;;  auto-dmacro-alist (cons '("\\.c$" . dot-c) auto-dmacro-alist))

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

(when (and running-x i-use-imenu)
  (require 'imenu)
  (setq
   imenu-sort-function 'imenu--sort-by-name
   imenu-max-items 35))

;;; ORG MODE ;;;

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done 'time
      org-return-follows-link t)

(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            ;; (make-variable-buffer-local 'yas/trigger-key)
            ;; (org-set-local 'yas/trigger-key [tab])
            ;; (define-key yas/keymap [tab] 'yas/next-field-group)
            ;; flyspell mode for spell checking everywhere
            ;; (flyspell-mode 1)
            ;; auto-fill mode on
            (auto-fill-mode 1)))

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
     my-initial-height 45
     my-default-height 45
     my-initial-width 90
     my-default-width 90))
   ((string-match my-host "tapdance")
    (setq
     my-initial-height 63
     my-default-height 63
     my-initial-width 100
     my-default-width 100)))

  (setq
   ;; This only works under X: x-pointer-shape x-pointer-hand1

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

  ;; (when color-x

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
    ;; )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISCELLANEOUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (auto-raise-mode nil)

(require 'iswitchb)
(iswitchb-mode t)
(setq iswitchb-buffer-ignore '("\\\\*vc\\\\*" "\\\\*Completions\\\\*" "\\\\*Messages\\\\*" "^ "))

(toggle-uniquify-buffer-names)
(fset 'yes-or-no-p 'y-or-n-p )

;; (set-default 'tab-width 4)
(setq-default indent-tabs-mode nil)
(set-default  'fill-column 75)

(set-default 'cursor-type 'box)
(set 'cursor-type 'box)

(setq
 mail-signature nil
 x-select-enable-clipboard t
 interprogram-paste-function 'x-cut-buffer-or-selection-value
 display-time-24hr-format t
 backward-delete-char-untabify-method nil
 browse-url-browser-function (quote browse-url-generic)
 ;; browse-url-generic-args          '("-newpage")
 browse-url-new-window-p nil
 ;; browse-url-generic-program "firefox-3.0"
 browse-url-generic-program "chromium-browser"
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
 mail-host-address                "jon.es"
 ;; mail-host-address                "eatoni.com"
 mail-archive-file-name           (expand-file-name "~/mail/OUT")
 mail-default-reply-to            "terry@jon.es"
 mail-from-style                  'system-default
 visible-bell                     t
 gnus-nntp-server                 "fw.intellimedia.com"
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
 ;; default-major-mode               'text-mode
 enable-recursive-minibuffers     nil
 inhibit-startup-message          t
 compile-command                  "make"
 mark-ring-max                    64
 kill-ring-max                    64
 lpr-command                      "lpr"
 lpr-switches                     (list (concat "-P" (getenv "PRINTER")))
 ps-lpr-command                   "lpr"
 ps-lpr-switches                  (list (concat "-P" (getenv "PRINTER"))))

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'server-buffer-clients 'preserved t)
(put 'truncate-lines 'permanent-local t)

;;;;;;;;;;;;;;;;;; LANGUAGE & ACCENTS ;;;;;;;;;;;;;;

(set-language-environment "UTF-8")

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

    (if
	(and
	 (boundp 'i-use-flyspell)
	 i-use-flyspell)
	(progn
	  (flyspell-mode t)
	  (define-key flyspell-mode-map "\M-\C-i" nil)))

    (and
     (boundp 'i-use-gin-mode)
     i-use-gin-mode
     (gin-mode t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FILE FINDING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 find-file-visit-truename      t
 find-file-existing-other-name t)

(add-hook
 'find-file-hooks
 '(lambda ()
    (abbrev-mode t)
    (and
     (boundp 'i-use-gin-mode)
     i-use-gin-mode
     (gin-mode t))))

(add-hook
 'find-file-not-found-hooks
 '(lambda ()
    (abbrev-mode t)
    (and
     (boundp 'i-use-gin-mode)
     i-use-gin-mode
     (gin-mode t))))

(eval-when-compile
  (require 'my-oe))

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
  '(("\\.\\([cChHl]\\)\\1?\\(\\.m4\\)?$"             . c-mode)
    ("\\.tex\\(\\.m4\\)?$"                           . LaTeX-mode)
    ("\\.java\\(\\.m4\\)?$"                          . java-mode)
    ("\\.rb\\(\\.m4\\)?$"                            . ruby-mode)
    ("\\.m$"                                         . objc-mode)
    ("[mM]akefile\\.?"                               . makefile-gmake-mode)
    ("\\.htm4?l?\\.m4$"                              . m4-mode)
    ("\\.\\(perl\\|ph\\|pl\\|pm\\)\\(\\.m4\\)?$"     . perl-mode)
    ("\\.\\(stklos\\|scm\\|stk\\)\\(\\.m4\\)?$"      . scheme-mode)
    ("\\.tk?$"                                       . tcl-mode)
    ("\\.save$"                                      . lisp-mode)
    ("\\.\\(tac\\|rpy\\)$"                           . python-mode)
    ("\\.\\(e\\|h\\)rl$"                             . erlang-mode)
    ("\\.\\(e\\|h\\)rl$"                             . erlang-mode)
    ("\\.rst$"                                       . text-mode)
    ("\\.js$"                                        . js2-mode)
    ("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)$"             . nxml-mode)
    )
  auto-mode-alist)

;;; Interpreter modes.

 interpreter-mode-alist (append
			 '(("ruby" . ruby-mode)
                           ("perl" . perl-mode))
			 interpreter-mode-alist)
 )

(add-hook 'ruby-mode-hook
          '(lambda () (inf-ruby-keys)))


;;;;;;;;;;;;;;;;;;;;;;;; C, CC AND OBJECTIVE C MODES ;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-style-variables-are-local-p nil)

(if i-use-imenu
    (add-hook-if-x 'c-mode-common-hook '(lambda () (imenu-add-to-menubar "Functions"))))

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

;; (setq
;; vc-initial-comment  nil
;; vc-handled-backends '(SVN CVS RCS SCCS Arch MCVS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when
;;     i-use-diary
;;   (add-hook 'diary-hook 'appt-make-list)
;;   ;; (setq appt-display-diary nil)
;;   (setq appt-issue-message nil)
;;   (add-hook 'diary-display-hook 'fancy-diary-display)
;;   (setq diary-list-include-blanks t)
;;   (add-hook 'list-diary-entries-hook 'sort-diary-entries t)
;;   (add-hook 'list-diary-entries-hook 'include-other-diary-files)
;;   (add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
;;   (add-hook 'today-visible-calendar-hook 'calendar-mark-today))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GNUSERV ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(eval-when-compile (require 'gnuserv))

(when
    (and (boundp 'i-use-gnuserv)
	 i-use-gnuserv)
  (require 'gnuserv)
  ;; (gnuserv-start)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SERVER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when
    i-use-server
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRAMP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (require 'tramp))

(when
    i-use-tramp
  (require 'tramp)
  (setq tramp-default-method "scp")
  )
