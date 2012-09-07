(require 'my-c-java)

(defun alter-c-mode-bindings ()
  (interactive)
  ;; (modify-syntax-entry ?# "w" c-mode-syntax-table)
  ;; (modify-syntax-entry ?_ "w" c-mode-syntax-table)

  (define-keys
    c-mode-map
    (list
     "%"        'goto-matching-paren-or-insert
     "\C-m"     'newline-and-indent
     "\C-c\C-c" 'compile
     "\C-cb1"   'brace-1-line-stmt
     "\C-cbc"   'brace-case
     "\C-cc"    'c-comment-line
     "\C-cf"    'c-fprintf-line
     "\C-cp"    'c-printf-line
     "\C-cu"    'c-uncomment-line
     "\C-c-"    'c-re-surround-line-dashed
     "\C-ce"    'c-func-to-extern
     "\C-c\C-k" 'kill-compilation
     "\C-m"     'newline-and-indent
     ;; [S-return] 'newline-and-indent
     ;; [C-return] 'newline-and-indent
     )))
    

(defun c-fprintf-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (insert-string "fprintf(stderr, \"")
  (end-of-line)
  (delete-horizontal-space)
  (insert-string "\\n\");")
  (c-indent-command))
  
(defun c-printf-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (insert-string "printf(\"")
  (end-of-line)
  (delete-horizontal-space)
  (insert-string "\\n\");")
  (c-indent-command))
  
(defun c-comment-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (insert-string "/* ")
  (end-of-line)
  (delete-horizontal-space)
  (insert-string " */")
  (c-indent-command))

(defun c-uncomment-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (if (looking-at "/\\* ")
      (delete-char 3))
  (let ((start (point)))
    (end-of-line)
    (delete-horizontal-space)
    (forward-char -3)
    (if (and (>= (point) start) (looking-at " \\*/"))
	(delete-char 3)))
  (forward-char -1)
  (c-indent-command))

(defun c-func-to-extern ()
  (interactive)
  (beginning-of-line)
  (insert-string "extern ")
  (end-of-line)
  (delete-horizontal-space)
  (delete-char 1)
  (forward-char -1)
  (if (not (looking-at "\*"))
      (progn
	(forward-char 1)
	(insert-string " ")))
  (skip-chars-forward "^(\n")
  (if (looking-at "(")
      (progn
	(forward-char 1)
	(let ((beg (point)))
	  (skip-chars-forward "^)\n")
	  (if (looking-at ")")
	      (delete-region beg (point))))))
  (end-of-line)
  (insert-string ";"))

(provide 'my-c)
