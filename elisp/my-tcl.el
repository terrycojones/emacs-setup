(autoload 'tcl-mode  "tcl-mode" "Sort-of minor mode for editing TCL code" t)
  
(setq
 tcl-mode-hook
 '(lambda ()
    (setq
     tcl-indent 2
     indent-line-function 'tcl-indent-line
     gin-left-hang-indent-re "\\([-*#]\\|([a-zA-Z0-9])\\|[a-zA-Z0-9]\\.?:]?\\)"
     gin-retain-indent-re "[a-zA-Z]*#?>+\\( >\\)*[    ]*\\|[  ]*"
     comment-start "# "
     comment-start-skip "#+ "
     comment-indent-hook 'tcl-calculate-indent)
	   
    (local-set-key "\r" 'reindent-then-newline-and-indent)
    (local-set-key "%"  'goto-matching-paren-or-insert)
	   
    (if running-x
	(progn
	  (abbrev-mode 1)
	  (defun tcl-proc-bold ()
	    (set-face-font 'bold (face-font 'default))
	    (condition-case nil
		(make-face-bold 'bold)
	      (error nil))
	    (overlay-put (make-overlay (- (point) 4) (point)) 'face 'bold))))))

(provide 'my-tcl)
