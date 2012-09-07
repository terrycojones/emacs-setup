;;; -*-Emacs-Lisp-*-

(defun my-gnus-kill-mode-string ()
  (kill-local-variable 'global-mode-string))

(defun previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun my-latex-hook ()
  (interactive)
  (progn
    (outline-minor-mode t)
    (turn-on-auto-fill)
    (gin-mode nil)
    (if (equal window-system 'x)
	(setq hilit-auto-rehighlight nil))
    ;; (setq outline-regexp "[ \t]*\\(\\\\chap\\|\\\\\\(sub\\)*section\\)")
    ))

(defun hilit-rehighlight-page ()
  (interactive)
  (save-excursion
    (move-to-window-line 0)
    (beginning-of-line)
    (let ((beg (point)))
      (move-to-window-line -1)
      (end-of-line)
      (hilit-rehighlight-region beg (point) t))))

(defun set-my-primary-buffer-name ()
  (interactive)
  (progn
    (setq my-primary-buffer-name (buffer-name))
    (message (format "Primary buffer set to %s" (buffer-name)))))

(defun switch-to-my-primary-buffer ()
  (interactive)
  (if (and (boundp 'my-primary-buffer-name) (stringp my-primary-buffer-name))
      (switch-to-buffer my-primary-buffer-name)
    (message "Primary buffer not set!")))

(defun startup ()
  (interactive)
  (setq load-path (append load-path (list (expand-file-name "~/emacs/elisp"))))

  (or global-mode-string (setq global-mode-string '("")))
  (setq global-mode-string
        (append global-mode-string 
                (list " " (substring (system-name) 0 (string-match "\\." (system-name))) "  ")))
  
  ;; supercite stuff
  (autoload 'sc-cite-original        "supercite" "Hookified Supercite 2.3" t)
  (autoload 'sc-cite                 "supercite" "Interactive Supercite 2.3" t)
  (add-hook 'mail-citation-hook      'sc-cite-original)

  (setq     sc-citation-leader        "")
  (setq     sc-citation-delimiter     ">")
  (setq     sc-citation-separator     " ")
  (setq     sc-nested-citation-p      nil)
  (setq     sc-downcase-p             nil)
  (setq     sc-preferred-header-style 4)
  (setq     sc-auto-fill-region-p     nil)
  (setq     sc-fixup-whitespace-p     t)
  
  ;; VM stuff
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
  (setq vm-visible-headers '("^From" "^From " "^Subject" "^Date" "^To" "^Cc"))
  (setq vm-in-reply-to-format "Your message at %h on %w, %d %m %y")
  (setq vm-mail-window-percentage 60)
  (setq vm-included-text-prefix "+ ")
  (setq vm-auto-folder-alist 
        '(("From"
           ("daemon@cogsci" . (expand-file-name "~/mail/listserv"))
           ("terry@cluster" . (expand-file-name "~/mail/cluster"))
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
    (("rememeber"   "remember"    nil 0)
     ("teh"         "the"         nil 0)
     ("fcat"        "fact"        nil 0)
     ("ahve"        "have"        nil 0)
     ("interseting" "interesting" nil 0)
     ("intial"      "initial"     nil 0)
     ("thasnk"      "thanks"      nil 0)
     ("taht"        "that"        nil 0)
     ("doen"        "done"        nil 0)))

  (require 'ehelp)
  (require 'crypt)
  
  (require 'tex-site)
  (setq-default TeX-master "main")
  ;; LaTeX-section-hook p 10.
  ;; LaTeX-section-label p 11.
  (setq LaTeX-default-environment "itemize")
  (setq LaTeX-float "htb")
  (setq LaTeX-figure-label "fig:")
  (setq LaTeX-table-label "fig:")
  (setq outline-minor-mode-prefix "\C-c\C-o")
  ;;(setq TeX-outline-extra
  ;;'(("[ \t]*\\\\\\(bib\\)?item\\b" 7)
  ;;  ("\\\\bibliography\\b" 2)))
  (setq TeX-one-master "<none>")
  ;; (setq TeX-parse-self t)
  ;; (setq TeX-auto-save t)
  ;; (setq TeX-auto-untabify nil)
  (setq TeX-auto-local ".auctex/")
    
  (load "terry")
  (load "ebuff-menu")
  (autoload 'vm "vm" "VM mail reader" t)
  (autoload 'vm-mail "vm" "VM mail reader" t)
  (autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
  (autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)

  (load "gin-mode")
  (load "perl-mode")
  (load "mouse-sel")
  (load "bookmark")
  (load "dmacro")
  (dmacro-load "~/emacs/elisp/dm-c-terry.dm")
  (setq auto-dmacro-alist (cons '("\\.h$" . dot-h) auto-dmacro-alist))
  (setq auto-dmacro-alist (cons '("\\.c$" . dot-c) auto-dmacro-alist))

  (load "tags")
  (add-hook 'find-tag-hook   'my-find-tag-hook)
  
  (global-set-key "\M-["     'tag-continue)
  (global-set-key "\M-."     'my-find-tag)
  (global-set-key "\M-'"     'tag-pop)
  (global-set-key "\M-`"     'set-my-primary-buffer-name)
  (global-set-key [f3]       'hilit-rehighlight-page)
  (global-set-key [f4]       '(lambda () (interactive) (point-to-register ?x)))
  (global-set-key [f5]       '(lambda () (interactive) (jump-to-register ?x)))
  (global-set-key [f6]       'goto-line)
  (global-set-key [f7]       'switch-to-my-primary-buffer)
  (global-set-key [f8]       '(lambda () (interactive) (recenter -1)))
  (global-set-key [f9]       'electric-buffer-list)
  (global-set-key [f10]      '(lambda () (interactive) (recenter 0)))
  (global-set-key [f11]      'set-mark-command)                                   ; stop
  (global-set-key [f12]      'previous-buffer)                                    ; again
  (global-set-key [f13]      'save-buffer)                                        ; props
  (global-set-key [f14]      'undo)                                               ; undo
  (global-set-key [f15]      'delete-other-windows)                               ; front
  (global-set-key [f16]      'kill-ring-save)                                     ; copy
  (global-set-key [f17]      'TeX-recenter-output-buffer)                         ; open
  (global-set-key [f18]      'hilit-yank)                                         ; paste
  (global-set-key [f19]      'find-file)                                          ; find
  (global-set-key [f20]      'kill-region)                                        ; cut
  (global-set-key [f21]      'beginning-of-buffer)                                ; pause
  (global-set-key [f22]      '(lambda () (interactive) (kill-buffer nil)))        ; PrSc
  (global-set-key [f23]      'scroll-other-window-down)                           ; scroll/lock/break
  (global-set-key [f24]      'end-of-buffer)                                      ; =
  (global-set-key [f25]      'LaTeX-fill-paragraph)                               ; /
  (global-set-key [f26]      'scroll-other-window)                                ; *
  (global-set-key [f27]      '(lambda () (interactive) (move-to-window-line 0)))  ; home
  (global-set-key [f29]      'scroll-down)                                        ; pgup
  (global-set-key [f30]      'backward-char)                                      ; left
  (global-set-key [f31]      'move-to-window-line)                                ; middle
  (global-set-key [f32]      'forward-char)                                       ; right
  (global-set-key [f33]      '(lambda () (interactive) (move-to-window-line -1))) ; end
  (global-set-key [f35]      'scroll-up)                                          ; pgdn
  (global-set-key [insert]   'recenter)                                           ; insert
  (global-set-key [S-return] 'newline)
  (global-set-key [C-return] 'newline)
  (global-set-key [S-delete] 'backward-delete-char-untabify)
  (global-set-key [C-delete] 'backward-delete-char-untabify)
  (global-set-key "\C-x\C-b" 'electric-buffer-list)
  (global-set-key "\C-h"     'ehelp-command)
  (global-set-key "\C-x\m"   'vm-mail)
  ;; (global-set-key "\C-c-"    '(lambda () (interactive) (recenter -1)))
  ;; (global-set-key "\C-c0"    '(lambda () (interactive) (recenter 0)))
  ;; (global-set-key "\C-ce"    'eval-line)
  ;; (global-set-key "\C-cb"    'bookmark-map)
  (global-set-key "\M-g"     'goto-line)
  (global-set-key "\M-/"     'hippie-expand)
  (global-set-key "\M-i"     'insert-buffer)

  ;;
  ;; imenu.el setup.
  ;;
  (require 'imenu)

  ;; Add a function menu to the menubar.
  (add-hook 'c++-mode-hook '(lambda () (imenu-add-to-menubar "Functions")))
  (add-hook 'c-mode-hook '(lambda () (imenu-add-to-menubar "Functions")))
  (setq imenu-sort-function 'imenu--sort-by-name)
  (setq imenu-max-items 35)

  ;; On a window system bind a mouse key to imenu (This will bring up a
  ;; popup menu in all modes supporting imenu.

  ;(cond (window-system
	; (define-key global-map [S-down-mouse-3] 'imenu)))

  (fmakunbound 'c-mode)
  (makunbound  'c-mode-map)
  (fmakunbound 'c++-mode)
  (makunbound  'c++-mode-map)
  (makunbound  'c-style-alist)


  (autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
  (autoload 'c-mode   "cc-mode" "C Editing Mode" t)
  (autoload 'tcl-mode "tcl-mode" "Sort-of minor mode for editing TCL code" t)
  
  (setq auto-mode-alist
	(append '(("\\.C$"  . c++-mode)
		  ("\\.cc$" . c++-mode)
		  ("\\.c$"  . c-mode)
		  ("\\.h$"  . c-mode)
		  ("\\.t$"  . tcl-mode)
		  ) auto-mode-alist))

  (setq tcl-mode-hook
        '(lambda ()
           ; (setq tcl-program "~/metvuw/src/pqmain")
           (setq tcl-indent 2)
           (setq indent-line-function 'tcl-indent-line)
           (setq gin-left-hang-indent-re
                 "\\([-*#]\\|([a-zA-Z0-9])\\|[a-zA-Z0-9]\\.?:]?\\)")
           (setq gin-retain-indent-re "[a-zA-Z]*#?>+\\( >\\)*[    ]*\\|[  ]*")
           (setq comment-start "# ")
           (setq comment-start-skip "#+ ")
           (setq comment-indent-hook 'tcl-calculate-indent)
           (local-set-key "\r" 'reindent-then-newline-and-indent)
           (if window-system
               (progn
                 (abbrev-mode 1)
                 (defun tcl-proc-bold ()
                   (set-face-font 'bold (face-font 'default))
                   (condition-case nil
                       (make-face-bold 'bold)
                     (error nil))
                   (overlay-put (make-overlay (- (point) 4) (point)) 'face 'bold))))))

  ;; X windows stuff
  (if (equal window-system 'x)
      (progn
;;      (if (not (boundp 'ebuttons-running))
;;          (progn
;;            (load "ebuttons")
;;            (ebuttons)))

        ;; This makes the cursor run away from the mouse.
        ;; (require 'avoid)
        ;; (mouse-avoidance-mode 'cat-and-mouse)

        (load "paren")
        (load "hilit19")

        (setq x-pointer-shape x-pointer-spider)
        (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))

        (setq default-frame-alist (append default-frame-alist (list '(auto-raise . 1))))

        ; (setq hilit-face-check nil)
        (setq hilit-auto-rehighlight 'visible)
        (setq hilit-quietly t)
        (setq hilit-mode-enable-list '(not text-mode))
        (setq hilit-background-mode 'dark)

        (add-hook 'find-file-hooks 'hilit-rehighlight-buffer)

        (hilit-translate comment   'yellow/cornflowerblue)
        (hilit-translate include   'white/royalblue3)
        (hilit-translate define    'white/slateblue1)
        (hilit-translate defun     'darkslateblue/yellow)
        (hilit-translate decl      'black/skyblue)
        (hilit-translate type      nil)
        (hilit-translate keyword   'white/cyan3)
        (hilit-translate label     'white/blue)
        (hilit-translate string    'white/lightskyblue3)
        (hilit-translate crossref  'black/lightblue2)
        (hilit-translate msg-quote 'darkslateblue/yellow)
        (hilit-translate formula   'darkslateblue/yellow)

        (hilit-set-mode-patterns
         'tcl-mode
         '(("\\s #.*$" nil comment)
           ("^#.*$" nil comment)
           ("[^\\]\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
           ;; ORIG ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
           ;; ("\\$[-_a-zA-Z]+" nil varref)
           ("\\$[-_\.a-zA-Z0-9]+" nil label)
           ("^source.*$" nil include)
           ("\\b\\(global\\|upvar\\)\\b" nil decl)
           ("\\b\\(error\\|debug\\)\\b" nil decl)
           ("^\\s *proc\\s +\\(\\w\\|[_']\\)+" nil defun)
           ("\\b\\(set\\|lset\\|list\\|if\\|case\\|while\\|switch\\|then\\|else\\|for\\|foreach\\|return\\|expr\\|catch\\)\\b" nil keyword)))

        (set-face-background 'highlight "green")
        (set-face-background 'region    "gray")
        (set-face-background 'modeline  "cyan") 
        (set-face-foreground 'modeline  "black")))

  ;; Miscellaneous stuff.

  (transient-mark-mode t)
  (auto-raise-mode nil)
  (display-time)

  (setq         kill-whole-line               t)
  (setq         display-time-day-and-date     t)
  (setq         target-buffer                 (get-buffer "*scratch*"))
  (setq         find-file-visit-truename      t)
  (setq         find-file-existing-other-name t)
  (setq         auto-save-timeout             15)
  (setq         mark-even-if-inactive         t)
  (setq         next-line-add-newlines        nil)
  (setq         pop-up-windows                t)
  (setq         blink-matching-paren-distance 20000)
  (setq         default-major-mode            'text-mode)
  (setq         enable-recursive-minibuffers  t)
  (setq         inhibit-startup-message       t)
  (setq         compile-command               "make")
  (setq         mail-archive-file-name        (expand-file-name "~/mail/OUT"))
  (setq         mark-ring-max                 64)
  (setq         kill-ring-max                 64)
  (setq         fill-column                   200)
  (setq         lpr-switches                  (list (concat "-P" (getenv "PRINTER"))))
  ;; (setq         tex-default-mode              'latex-mode)

  (add-hook     'mail-setup-hook              'mail-abbrevs-setup)
  (add-hook     'mail-setup-hook              '(lambda () (gin-mode t)))
  ;; (add-hook     'text-mode-hook               'turn-on-auto-fill)
  (add-hook     'find-file-hooks              '(lambda () (abbrev-mode t)))
  (add-hook     'find-file-hooks              '(lambda () (line-number-mode t)))
  (add-hook     'find-file-hooks              '(lambda () (gin-mode t)))
  (add-hook     'find-file-not-found-hooks    '(lambda () (abbrev-mode t)))
  (add-hook     'find-file-not-found-hooks    '(lambda () (line-number-mode t)))
  (add-hook     'find-file-not-found-hooks    '(lambda () (gin-mode t)))
  ;;(add-hook     'latex-mode-hook              'my-latex-hook)
  (add-hook     'LaTeX-mode-hook              'my-latex-hook)

  (put          'eval-expression              'disabled nil)
  ;; (put          'upcase-region                'disabled nil)
  ;; (put          'downcase-region              'disabled nil)
  (put          'narrow-to-region             'disabled nil)
  (put          'server-buffer-clients        'preserved t)
  
  (define-key   perl-mode-map "\C-m"          'newline-and-indent)
  
  ;; c mode (BOCM) stuff.
  ;; (modify-syntax-entry ?# "w"                 c-mode-syntax-table)
  ;; (modify-syntax-entry ?_ "w"                 c-mode-syntax-table)
  ;; (define-key   c-mode-map "%"                'goto-matching-paren-or-insert)
  ;; (define-key   c-mode-map "\C-m"             'newline-and-indent)
  ;; (define-key   c-mode-map "\C-c\C-c"         'compile)
  ;; (define-key   c-mode-map "\C-cc"            'c-comment-line)
  ;; (define-key   c-mode-map "\C-ce"            'c-func-to-extern)
  ;; (define-key   c-mode-map "\C-c\C-k"         'kill-compilation)
  ;; (define-key   c-mode-map "\C-c\C-m"         '(lambda () (interactive) (recenter 0)))
  ;; (define-key   c-mode-map "\C-cu"            'c-uncomment-line)
  ;; (setq         c-indent-level                4)
  ;; (setq         c-continued-statement-offset  4)
  ;; (setq         c-brace-offset                0)
  ;; (setq         c-continued-brace-offset      -4)
  ;; (setq         c-brace-imaginary-offset      0)
  ;; (setq         c-argdecl-indent              0)
  ;; (setq         c-label-offset                0)

  (define-key emacs-lisp-mode-map "\C-m"     'newline-and-indent)
  (define-key emacs-lisp-mode-map [S-return] 'newline-and-indent)
  (define-key emacs-lisp-mode-map [C-return] 'newline-and-indent)

  (defun my-c-mode-common-setup ()
    (c-set-offset 'case-label           '+)
    (c-set-offset 'label                 0)
    (c-set-offset 'statement-case-intro '+)
    (c-set-offset 'knr-argdecl-intro     0)
    (c-set-offset 'statement-cont       'c-lineup-math)
    (define-key   c-mode-map "%"        'goto-matching-paren-or-insert)
    (define-key c-mode-map "\C-m"       'newline-and-indent)
    (define-key c-mode-map [S-return]   'newline-and-indent)
    (define-key c-mode-map [C-return]   'newline-and-indent))
  
  ;; cc-mode configuration
  (setq c-basic-offset 4)
  (setq c-inhibit-startup-warnings-p nil)
  ;(setq c-hungry-delete-key t)
  ;(setq c-auto-newline t)

  (add-hook 'c-mode-common-hook 'my-c-mode-common-setup)


  ;; GNUS stuff
  (setq         gnus-large-newsgroup          100)
  (setq         gnus-use-long-file-name       t)
  (setq         gnus-article-save-directory   (expand-file-name "~/things"))
  (setq         gnus-use-cross-reference      t)
  (setq         gnus-default-article-saver    'gnus-summary-save-in-mail)
  (setq         gnus-local-distributions      '("world"))
  (setq         gnus-auto-center-summary      nil)
  (add-hook     'news-reply-header-hook       '(lambda () (gin-mode t)))
  (add-hook     'gnus-group-mode-hook         'my-gnus-kill-mode-string)
  (add-hook     'gnus-article-mode-hook       'my-gnus-kill-mode-string)
)

;; call the startup function defined above.
(startup)

;;; --- Compile this file when saving it.
;;; Local Variables:
;;; after-save-hook: ((lambda () (byte-compile-file buffer-file-name)))
;;; End:
