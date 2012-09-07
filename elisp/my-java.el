(require 'my-c-java)

(defun alter-java-mode-bindings ()
  (interactive)

  (define-keys
    java-mode-map
    (list
     "%"        'goto-matching-paren-or-insert
     "\C-m"     'newline-and-indent
     "\C-c\C-c" 'compile
     "\C-cb1"   'brace-1-line-stmt
     "\C-cbc"   'brace-case
     "\C-cc"    'java-comment-line
     "\C-cf"    'java-fprintf-line
     "\C-cp"    'java-printf-line
     "\C-cu"    'java-uncomment-line
     "\C-c-"    'java-re-surround-line-dashed
     "\C-c\C-k" 'kill-compilation
     "\C-c\C-m" '(lambda () (interactive) (recenter 0))
     "\C-m"     'newline-and-indent
     [S-return] 'newline-and-indent
     [C-return] 'newline-and-indent
     )))
    

(defun java-fprintf-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (insert-string "System.out.println(\"")
  (end-of-line)
  (delete-horizontal-space)
  (insert-string "\");")
  (c-indent-command))
  
(defun java-printf-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (insert-string "System.out.println(\"")
  (end-of-line)
  (delete-horizontal-space)
  (insert-string "\");")
  (c-indent-command))
  
(defun java-comment-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (insert-string "// ")
  (c-indent-command))

(defun java-uncomment-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (if (looking-at "// ")
      (delete-char 3))
  (c-indent-command))

(provide 'my-java)
