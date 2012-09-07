;;(add-hook 'python-mode-hook 'twitter-python-mode)
;;(remove-hook 'python-mode-hook 'twitter-python-mode)

;;(add-hook 'find-file-hooks 'twitter-find-file-hook)
;;(remove-hook 'find-file-hooks 'twitter-find-file-hook)

(defun twitter-find-file-hook ()
  (interactive)
  (call-process "tweet" nil 0 nil (format "switching to buffer %s" (buffer-file-name))))

(defun twitter-python-mode ()
  (interactive)
  (call-process "tweet" nil 0 nil "Entering Python mode."))

(defun tweet (mesg)
  (interactive "MTweet: ")
  ;;(call-process "tweet" nil '(t t) nil (format "'%s'" (encode-coding-string mesg 'utf-8))))
  ;;(call-process "tweet" nil 0 nil mesg))
  ;; The following will send iso-8859-1 to the shell.
  (call-process "tweet" nil 0 nil mesg))

(provide 'my-twitter)
