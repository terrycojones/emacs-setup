(defun brace-1-line-stmt ()
  (interactive)
  (end-of-line)
  (insert "{")
  (forward-line 1)
  (end-of-line)
  (insert "
}")
  (indent-for-tab-command)
  (forward-line 1))

(defun brace-case ()
  (interactive)
  (end-of-line)
  (insert "{")
  (when (search-forward "break" nil t)
    (end-of-line)
    (insert "
}")
    (indent-for-tab-command)
    (forward-line 1)))

(provide 'my-c-java)
