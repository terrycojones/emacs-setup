(require 'my-misc)

(setq
 my-keymap (make-keymap "terry")
 my-find-file-func (if (and (boundp 'i-use-dired) i-use-dired) 'dired-x-find-file 'find-file))

(define-keys
  my-keymap
  (list
   [menu]  'iso-accents-mode

   "0"     'delete-window
   "1"     'delete-other-windows
   "2"     'split-window-vertically
   "a"     'iso-accents-mode
   "b"     'electric-buffer-list
   "c"     '(lambda ()
	      (interactive)
	      (if
		  (and
		   (boundp 'vc-mode)
		   vc-mode)
		  
		  (vc-toggle-read-only)
	      (call-interactively 'kill-region)))
   
   "d"     '(lambda () (interactive) (switch-to-buffer-by-name "terry"))
   "e"     'visit-my-emacs-startup-file
   "f"     my-find-file-func
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
   "q"     'fill-paragraph
   "r"     'gnus
   "s"     '(lambda ()
	      (interactive)
	      (save-excursion
		(call-interactively 'indent-to-with-spaces)))
   
   "t"     'beginning-of-buffer
   "u"     'undo
   "v"     'view-file
   "w"     'widen
   "y"     'yank
   "/"     'hippie-expand
   "+"     'plus-to-end
   ">"     'gt-to-end
   "|"     'pipe-to-end
   "."     'finish-kbd-macro-and-exec
   ;; "." 'call-last-kbd-macro
   ","     'set-mark-command
   "`"     'font-lock-mode
   "'"     'iso-accents-mode
   "/"     '(lambda () (interactive) (set-mark-command 1))
   
   "]"     '(lambda ()
	      (interactive)
	      (let
		  ((p (point))
		   (m (mark)))
		(kill-rectangle (min m p) (max m p))))
   
   "<"     'scroll-left
   ">"     'scroll-right
   "-t"    'text-mode
   "-m"    'm4-mode
   "-e"    'emacs-lisp-mode
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
	      (setq truncate-lines (if truncate-lines nil t)))
   
   "B"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/.bashrc"))
	      (end-of-buffer))
   
   "C"     '(lambda () (interactive) (switch-to-buffer "*compilation*"))
   
   "E"     'visit-my-emacs-startup-file
   "F"     'find-function
   
   "K"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/emacs/elisp/my-mac-keys.el")))
   
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
	      (find-file (expand-file-name "~/.phonebook"))
	      (end-of-buffer))
   
   "R"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/bin/r")))
   
   "S"     '(lambda () (interactive) (switch-to-buffer "*scratch*"))
   
   "X"     '(lambda ()
	      (interactive)
	      (find-file (expand-file-name "~/.xinitrc.chiliad"))
	      (end-of-buffer))
   ))
  
(and i-use-viper-mode
     (global-set-key "\C-z" 'vip-change-state-to-vi))
     
(define-keys
  global-map
  (list "\C-x\C-b"  'electric-buffer-list
	"\C-x\m"    'vm-mail
	"\M-g"      'kill-white-after
	"\M-/"      'hippie-expand
	"\M-i"      'insert-buffer
	"\C-x\C-j"  'dired-jump
	"\C-x4\C-j" 'dired-jump-other-window
	))

(define-keys
  global-map
    (list
     [f8]     'save-buffer
     [f9]     '(lambda () (interactive) (point-to-register ?x))
     [f10]     '(lambda () (interactive) (jump-to-register ?x))
     [f11]     'goto-line
     [f12]     'start-or-end-kbd-macro
     [f13]     '(lambda () (interactive) (recenter -1))
     [f14]     'electric-buffer-list
     [f15]    '(lambda () (interactive) (recenter 0))
     [menu]   my-keymap
     [f17]    'previous-buffer		; again
;     [f14]    'undo			; undo
;     [f15]    'delete-other-windows	; front
;     [f16]    'kill-ring-save		; copy
;     [f17]    'delete-other-windows	; open
;     [f18]    'yank 		        ; paste    
;     [f19]    'my-fast-dired-x-find-file ; find
;     [f20]    'kill-region		; cut
     [f20]    my-find-file-func         ; pause
      [f18]    'kill-this-buffer		; PrSc
;     [f23]    'unscroll			; scroll/lock/break
;     [f24]    'my-fast-dired-x-find-file ; KP -
;     [f25]    'view-file		; KP /
;     [f26]    'scroll-other-window	; KP *
;     [f27]    '(lambda () (interactive) (move-to-window-line 0)) ; home
     [f29]    'scroll-down		; pgup
     [f30]    'backward-char		; left
     [f31]    'move-to-window-line	; middle
     [f32]    'forward-char		; right
     [f33]    '(lambda () (interactive) (move-to-window-line -1)) ; end
     [f35]    'scroll-up		; pgdn
     [print]  'kill-this-buffer		; print
     [pause]  my-find-file-func         ; pause
     [SunPowerSwitch] 'set-mark-command
     [SunAudioMute] '(lambda () (interactive) (kill-this-buffer) (delete-window))
     [SunAudioLowerVolume] 'scroll-other-window-down
     [SunAudioRaiseVolume] 'scroll-other-window
     [insert] 'recenter
     [down-mouse-3] 'my-dired-mouse-find-file
     ))

(provide 'my-mac-keys)
