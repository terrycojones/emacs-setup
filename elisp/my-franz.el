(load (expand-file-name "~/mds-no-svn/acl/mac/acl80/eli/fi-site-init"))

;; from derek.
;; (setq-default fi:subprocess-enable-superkeys t)

;;----------------- to make m-. m-. ---------------------------------

(setq fi:lisp-mode-hook
      (function
       (lambda ()
	 (let ((map (current-local-map)))
					; (define-key map "\C-c."	'find-tag)
					; (define-key map "\C-c,"	'tags-loop-continue)
	   (define-key map "\e."	'fi:lisp-find-definition)
	   (define-key map "\e,"	'fi:lisp-find-next-definition)))))

;;------------------------------------------------------------------
;;					   STARTING LISP
;;------------------------------------------------------------------

(setq fi:common-lisp-buffer-name "LISP"
      ;; fi:common-lisp-directory	(expand-file-name "~/mds/src/mds")
      fi:common-lisp-directory	(expand-file-name "~/mds-no-svn")
      fi:common-lisp-image-name (expand-file-name "~/mds-no-svn/acl/mac/acl80/alisp")
      fi:common-lisp-image-file (expand-file-name "~/mds-no-svn/acl/mac/acl80/alisp.dxl")
      fi:common-lisp-host "localhost"
      ;; fi:auto-arglist-pop-up-style '(split . nil)
      ;; fi:pop-up-temp-window-behavior '(minibuffer . nil)
      ;; fi:pop-up-temp-window-behavior '(other . nil)
      fi:lisp-evals-always-compile nil
      )

(defun run-allegro ()
  "This function starts up lisp with your defaults"
  (interactive)
  (fi:common-lisp fi:common-lisp-buffer-name
		  fi:common-lisp-directory
		  fi:common-lisp-image-name
		  fi:common-lisp-image-arguments
		  fi:common-lisp-host
		  fi:common-lisp-image-file))

(define-key global-map "\C-xl" 'run-allegro)
;; (define-key global-map "\C-xu" 'fi:shell)

(provide 'my-franz)
