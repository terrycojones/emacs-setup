(require 'my-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMINT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

(add-hook 'comint-output-filter-functions
	  'comint-strip-ctrl-m)

(add-hook 'shell-mode-hook '(lambda ()
			      (font-lock-mode nil)))

(setq
 comint-input-ignoredups t
 comint-completion-autolist nil
 comint-input-autoexpand t
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SHELL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "shell"
  '(progn
     (define-keys
     shell-mode-map
     (list
      " "        'comint-magic-space
      "\C-c\C-p" 'shell-goto-last-nonempty-cmd
      "\C-c\C-n" 'shell-goto-next-nonempty-cmd
      "\C-p"     'comint-previous-input
      "\C-n"     'comint-next-input
      "\C-a"     'comint-bol
      "\C-l"     '(lambda () (interactive) (recenter 0))
      ))))
     

(setq
 shell-pushd-regexp    "pushd\\|pu"
 shell-popd-regexp     "popd\\|po"
 shell-cd-regexp       "cd\\|c"
 shell-prompt-pattern  "^[^ ]* [$#] *"
 )

(defun new-shell-frame ()
  "*Create a new buffer with a shell in it. The buffer will be named `*shell-N*'
where N is some number (the first unallocated)."
  
  (interactive)
  (select-frame (make-frame))
  (shell)
  (font-lock-mode nil)

  (let
      ((num 0))
    (while (buffer-from-buffer-name (concat "*shell-" (number-to-string num) "*"))
      (setq num (+ num 1)))

    (rename-buffer (concat "*shell-" (number-to-string num) "*"))))

(defun shell-insert-last-cmd ()
  (interactive)
  (insert (comint-previous-input-string 0))
)

(defun shell-goto-last-nonempty-cmd ()
  (interactive)
  (let
      ((pos (point)))

    (if (eq pos (comint-previous-prompt 1))
	()
      (if (eolp)
	  (shell-goto-last-nonempty-cmd)))))

(defun shell-goto-next-nonempty-cmd ()
  (interactive)
  (if (comint-next-prompt 1)
      (if (eolp)
	  (shell-goto-next-nonempty-cmd))))

(provide 'my-shell)
