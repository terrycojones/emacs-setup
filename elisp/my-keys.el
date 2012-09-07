(require 'my-misc)

(setq
 my-keymap (make-keymap "terry")
 my-fix-keymap (make-keymap "terry-fix")
 my-delete-keymap (make-keymap "terry-delete")
 my-find-file-func (if (and (boundp 'i-use-dired) i-use-dired) 'dired-x-find-file 'find-file))

(and
 (fboundp 'mapc)
 (mapc
  `(lambda (c)
     (let ((s (char-to-string c)))      
       (define-key my-fix-keymap s
         `(lambda ()
            (interactive)
            (my-fix-last ,s)))))
  my-fixable-chars )

 (mapc
  `(lambda (c)
     (let ((s (char-to-string c)))      
       (define-key my-delete-keymap s
         `(lambda ()
            (interactive)
            (my-delete-last ,s)))))
  my-fixable-chars))
  

(define-keys
  my-keymap
  (list
   ; [f1]    '(lambda () (interactive) (switch-to-buffer-by-name "*shell-0*"))
   ; [f2]    '(lambda () (interactive) (switch-to-buffer-by-name "*shell-1*"))
   [f11]   my-fix-keymap
   ;; [XF86AudioRaiseVolume]   my-fix-keymap
   [f12]   'shell
   ;; [f19]   'toggle-case-fold-search
   ;; [SunF36] 'iso-accents-mode

   "0"     'delete-window
   "1"     'delete-other-windows
   "9"     '(lambda ()
	      (interactive)
	      (my-fix-last "9"))
   
   "2"     'split-window-vertically
   "a"     'align
   
   "b"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/html/bookmarks/htm4l/bookmarks.htm4l")))
   
   "c"     'toggle-case-fold-search
   "d"     my-delete-keymap
   "e"     'visit-my-emacs-startup-file
   ;; "f"     'find-function
   "f"    'flyspell-mode
   ;; "f"     'set-bitstream-font
   "g"     'goto-line
   "h"     'mark-whole-buffer
   "i"     'insert-buffer
   "j"     'dired-jump
   "k"     'kill-this-buffer
   "l"     'eval-last-sexp
   "m"     'start-or-end-kbd-macro
   "n"     '(lambda ()
	      (interactive)
	      (beginning-of-line)
	      (set-mark-command (point))
	      (narrow-to-region (point) (point)))
   "o"     'other-window
   "p"     'previous-buffer
   "r"     'rename-buffer
   "q"     'quote-to-end
   "r"     'gnus
   "s"     '(lambda ()
	      (interactive)
	      (save-excursion
		(call-interactively 'indent-to-with-spaces)))
   
   "t"     'todo-insert-item
   
;;   "t"     '(lambda ()
;;	      (interactive)
;;	      (setq truncate-lines (not truncate-lines)))
   
   "u"     '(lambda () (interactive) (insert-last-urls 1))
   "U"     '(lambda () (interactive) (insert-last-urls 10))
   "v"     'view-file
   "w"     'widen
   "y"     'yank
   "/"     'hippie-expand
   "+"     'plus-to-end
   ">"     'gt-to-end
   ;; ">"     '(lambda () (interactive) (finish-kbd-macro-and-exec 0)) ; exec until error
   "|"     'gt-to-end
   "."     'finish-kbd-macro-and-exec
   ","     'set-mark-command
   "`"     'font-lock-mode
   ;; "'"     'iso-accents-mode
   "/"     '(lambda () (interactive) (set-mark-command 1))
   
   "]"     '(lambda ()
	      (interactive)
	      (let
		  ((p (point))
		   (m (mark)))
		(kill-rectangle (min m p) (max m p))))
   
   "-t"    'text-mode
   "-m"    'm4-mode
   "-e"    'emacs-lisp-mode
   "-f"    'flyspell-mode
   "-p"    'perl-mode
   "-s"    'shell-script-mode
   "-k"    'make-mode
   "-l"    'lisp-interaction-mode
   "-h"    'html-helper-mode
   
   "="     '(lambda ()
	      (interactive)
	      (if
		  (and
		   (boundp 'vc-mode)
		   vc-mode)
		  
		  (call-interactively 'vc-diff)
	      (recenter 0)))
   
   "$"     '(lambda ()
	      (interactive)
	      (setq truncate-lines (null truncate-lines))) ; why is this broken?
   
   "B"     '(lambda ()
	      (interactive)
	      (find-file-at-end-if-unvisited (expand-file-name "~/.bashrc")))
   
   "C"     '(lambda () (interactive) (switch-to-buffer "*compilation*"))
   
   "E"     'visit-my-emacs-startup-file
   
   "F"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/.fvwm2rc")))
   
   "K"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/emacs/elisp/my-keys.el")))
   
   "L"     '(lambda ()
	      (interactive)
	      (dired (expand-file-name "~/emacs/elisp")))
   
   "M"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/.mailrc")))
   
   "O"     '(lambda ()
	      (interactive)
	      (view-file (expand-file-name "~/mail/OUT"))
	      (end-of-buffer))
   
   "P"     '(lambda ()
	      (interactive)
	      (find-file-at-end-if-unvisited (expand-file-name "~/.phonebook")))
   
   "R"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/bin/r")))
   
   "S"     '(lambda ()
	      (interactive)
	      (delete-other-windows)
	      (switch-to-buffer "*scratch*"))

   "T"     'todo-show
   
   "V"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/.vm.el")))

   "X"     '(lambda ()
	      (interactive)
	      (find-file (getenv "XINITRC")))	      
   ))

(and (boundp 'i-use-viper-mode)
     i-use-viper-mode
     (global-set-key "\C-z" 'vip-change-state-to-vi))
     
(define-keys
  global-map
  (list "\C-x\C-b"  'electric-buffer-list
	"\C-x\m"    'vm-mail
	"\C-\M-z"   'delete-enclosing-function
	"\M-g"      'delete-white-after
	"\M-/"      'hippie-expand
	"\M-i"      'insert-buffer
	"\C-x\C-j"  'dired-jump
	"\C-x4\C-j" 'dired-jump-other-window))

(defun set-basic-function-keys ()
  (interactive)
  (define-keys
    global-map
    (list
     ;; [f2]     '(lambda () (interactive) (message "hi"))
     [f3]     'save-buffer
     [f4]     '(lambda () (interactive) (point-to-register ?x))
     [f5]     '(lambda () (interactive) (jump-to-register ?x))
     [f6]     'goto-line
     [f7]     'start-or-end-kbd-macro
     [C-f7]   'finish-kbd-macro-and-exec
     [f8]     '(lambda () (interactive) (recenter -1))
     [f9]     'electric-buffer-list
     [f10]    '(lambda () (interactive) (recenter 0))
     [f11]    my-keymap
     [f12]    'previous-buffer)))
  

(global-unset-key [mouse-3])
(global-unset-key [down-mouse-3])
(set-basic-function-keys)

;; Mac OS X aquamacs
(when
    (boundp 'aquamacs-version-id)
  ;; (cua-mode 0)
  (transient-mark-mode t)
  (define-key osx-key-mode-map '[remap next-line] 'next-line)
  (define-key osx-key-mode-map '[remap previous-line] 'previous-line)
  ;;Use the Apple command key as meta
  ;;(setq mac-command-modifier 'meta)
)


(cond
 ((and (boundp 'running-x) running-x)

  (define-keys
    global-map
    (list
     [print]               'kill-this-buffer
     [down-mouse-3]          'my-dired-mouse-find-file
     [M-mouse-3]             'hs-mouse-toggle-hiding))

  (cond
   ((equal my-keyboard 'pc)
    
    (define-keys
      function-key-map
      (list
       [kp-0]                [KP_0]
       [kp-1]                [KP_1]
       [kp-2]                [KP_2]
       [kp-3]                [KP_3]
       [kp-4]                [KP_4]
       [kp-5]                [KP_5]
       [kp-6]                [KP_6]
       [kp-7]                [KP_7]
       [kp-8]                [KP_8]
       [kp-9]                [KP_9]
       [kp-divide]           [KP_Divide]
       [kp-multiply]         [KP_Multiply]
       [kp-add]              [KP_Add]
       [kp-subtract]         [KP_Subtract]
       [kp-enter]            [KP_Enter]
       [kp-decimal]          [KP_Decimal]))
    
    (define-keys
      global-map
      (list
       [KP_0]                'delete-window
       [KP_1]                '(lambda () (interactive) (move-to-window-line -1))
       [KP_2]                'next-line
       [KP_3]                '(lambda () (interactive) (insert-last-url -1))
       [KP_4]                'backward-char
       [KP_5]                'move-to-window-line
       [KP_6]                'forward-char
       [KP_7]                '(lambda () (interactive) (move-to-window-line 0))
       [KP_8]                'previous-line
       [KP_9]                '(lambda () (interactive) (insert-last-url 1))
       [KP_Divide]           'delete-other-windows
       [KP_Multiply]         '(lambda () (interactive) (insert-last-urls 100))
       [KP_Add]              'scroll-other-window
       [KP_Subtract]         'scroll-other-window-down
       ;;[KP_Enter]          '(lambda () (interactive) (insert "[kp-enter]"))
       [KP_Decimal]          'set-mark-command
       [f17]                 '(lambda () (interactive) (delete-other-windows))
       [f18]                 'undo	; control right - see ~/.xmodmaprc
       ;; [f19]                 'finish-kbd-macro-and-exec ; Alt right - see ~/.xmodmaprc
       [f20]                 'next-error
       ;;[f21]               '(lambda () (interactive) (insert "[Windows right]"))
       ;; [f22]                 'ps-print-buffer ; printer icon key - see ~/.xmodmaprc
       [prior]               'scroll-down
       [next]                'scroll-up
       [end]                 'end-of-buffer
       [home]                'beginning-of-buffer
       [insert]              'recenter
       [pause]               my-find-file-func)))
       
   ((equal my-keyboard 'sun)
    (define-keys
      global-map
      (list
       [SunF36]              my-keymap	; F11 on a sun5 keybd
       [SunF37]              'previous-buffer ; F12 on a sun5 keybd
       [f13]                 'save-buffer ; prop
       [f14]                 'undo		; undo
       [f15]                 'delete-other-windows ; front
       [f16]                 'kill-ring-save ; copy
       [f17]                 'delete-other-windows ; open
       [f18]                 'yank		; paste    
       [f19]                 'my-fast-dired-x-find-file ; find
       [f20]                 'kill-region ; cut
       [f21]                 my-find-file-func ; pause
       [f22]                 'kill-this-buffer ; PrSc
       [f23]                 'unscroll	; scroll/lock/break
       [f24]                 'my-fast-dired-x-find-file ; KP -
       [f25]                 'view-file	; KP /
       [f26]                 'scroll-other-window ; KP *
       [f27]                 '(lambda () (interactive) (move-to-window-line 0))	; home
       [begin]               'undo
       [f29]                 'scroll-down ; pgup
       [f30]                 'backward-char	; left
       [f31]                 'move-to-window-line ; middle
       [f32]                 'forward-char ; right
       [f33]                 '(lambda () (interactive) (move-to-window-line -1)) ; end
       [f35]                 'scroll-up	; pgdn
       [pause]               my-find-file-func ; pause
       [SunPowerSwitch]      'set-mark-command
       [SunAudioMute]        '(lambda () (interactive) (kill-this-buffer) (delete-window))
       [SunAudioLowerVolume] 'scroll-other-window-down
       [SunAudioRaiseVolume] 'scroll-other-window)))))
       

 ((equal my-keyboard 'mac)
  (set-basic-function-keys)
  (define-keys
    global-map
    (list
     [f13]                   '(lambda () (interactive) (recenter -1))
     [f14]                   'electric-buffer-list
     [f15]                   '(lambda () (interactive) (recenter 0))
     [menu]                  my-keymap
     [f17]                   'previous-buffer ; again
     [f18]                   'kill-this-buffer ; PrSc
     [f20]                   my-find-file-func ; pause
     [f29]                   'scroll-down ; pgup
     [f30]                   'backward-char	; left
     [f31]                   'move-to-window-line ; middle
     [f32]                   'forward-char))) ; right
     
 (t	;; Not running X, not on a Mac.
  (define-keys
    global-map
    (list
     "\C-c-" '(lambda () (interactive) (recenter -1))
     "\C-c0" '(lambda () (interactive) (recenter 0))))))

(provide 'my-keys)
