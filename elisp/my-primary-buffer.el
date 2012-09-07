(defvar my-primary-buffer-name "*scratch*"   "The buffer switched to by default.")

(defun set-my-primary-buffer-name ()
  (interactive)
  (setq my-primary-buffer-name (buffer-name))
  (message (format "Primary buffer set to %s" (buffer-name))))

(defun switch-to-my-primary-buffer ()
  (interactive)
  (if (and (boundp 'my-primary-buffer-name) (stringp my-primary-buffer-name))
      (switch-to-buffer my-primary-buffer-name)
    (message "Primary buffer not set!")))

(provide 'my-primary-buffer)
