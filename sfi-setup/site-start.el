(let ((version-specific-site-lisp-dir
      (concat 
       "/network/software/packages/emacs/share/emacs/site-lisp/"
       emacs-major-version 
       "." 
       emacs-minor-version)))

  (if (file-accessible-directory-p version-specific-site-lisp-dir)
      (setq load-path 
	    (append (list version-specific-site-lisp-dir) load-path))))
