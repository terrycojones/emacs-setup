(defun my-c-lineup-block-close (langelem)
  ;; lineup block closes.
  (save-excursion
    (message "lining it up...")
    (beginning-of-line)
    (if (looking-at ".*else") 0 4)))

(add-hook 'c-mode-hook
	  '(lambda ()
	     (c-set-offset 'block-close 'my-c-lineup-block-close)))

