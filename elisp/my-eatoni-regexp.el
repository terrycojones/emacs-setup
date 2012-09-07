(defun eatoni-count (regexp)
  (interactive "sCount which regexp? -> ")
  (save-excursion
    (goto-char (point-min))
    (let
        ((total 0))
      (while (re-search-forward regexp nil t)
        (beginning-of-line)
        (re-search-forward "[0-9]+" nil nil)
        (setq total (+ total (string-to-int (buffer-substring (match-beginning 0) (match-end 0)))))
        (forward-line 1))
      (message (format "Total matches for '%s': %d" regexp total)))))
