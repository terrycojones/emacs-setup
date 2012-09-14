;;; -*-Emacs-Lisp-*-

;;;
;;; Load a version specific emacs startup file. Byte compiles
;;; the version specific file if it needs it. This file will
;;; be automatically recompiled when you write it out.
;;;
;;; Terry Jones
;;; 12 March 1996
;;;

; (setq debug-on-error t)

(let* ((version-specific-el
	(expand-file-name
	 (concat
	  "~/emacs/startup/" (int-to-string emacs-major-version) "." (int-to-string emacs-minor-version) ".el")))
       (version-specific-elc
	(concat version-specific-el "c")))

  (if (or
       (file-readable-p version-specific-el)
       (file-readable-p version-specific-elc))
      (progn
	(if (file-newer-than-file-p version-specific-el version-specific-elc)
	    (byte-compile-file version-specific-el))
	(load-file version-specific-elc))))

; (setq debug-on-error t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(org-agenda-files (quote ("~/org/todo.org" "~/org/personal.org")))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 102 :width normal)))))

;;; Local Variables:
;;; after-save-hook: ((lambda () (byte-compile-file buffer-file-name)))
;;; End:
