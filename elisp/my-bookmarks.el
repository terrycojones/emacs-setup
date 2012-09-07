(defun bm-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (let
      ((protocol "http")
       (prefix ""))
    
    (when (looking-at "ftp://")
      (setq protocol "ftp")
      (setq prefix "ftp")
      (delete-char 6))

    (when (looking-at "http://")
      (delete-char 7))   

    ;; Send off the bookmark to the Object Exchange.
    (and
     (boundp 'i-use-oe)
     i-use-oe
     (let
	 ((start (point)))
       (end-of-line)
       (oe-send-bookmark (concat protocol "://" (buffer-substring start (point))))
       (beginning-of-line)))
  
    (insert "        " prefix "bookmark(`")
    (end-of-line)
    (insert "', `')")
    (backward-char 2)))

(defun unbm-line ()
  (interactive)
  (beginning-of-line)
  (let
      ((protocol "http"))
    (if (looking-at "[ 	]*ftpbookmark")
	(setq protocol "ftp"))
    (zap-to-char 1 ?`)
    (insert protocol "://")
    (when
	(search-forward "'" nil t)
      (backward-char 1)
      (kill-line)
      (beginning-of-line))))


(provide 'my-bookmarks)
