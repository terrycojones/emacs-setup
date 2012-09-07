(defun my-latex-hook ()
  (interactive)
  (outline-minor-mode t)
  (turn-on-auto-fill)
  (gin-mode nil)
  (define-key LaTeX-mode-map "%" 'goto-matching-paren-or-insert)
  (set-face-foreground 'font-latex-bold-face   "orange")
  (set-face-foreground 'font-latex-italic-face "orange")
)

(setq-default TeX-master "main")
  
(setq
 LaTeX-default-environment "itemize"
 LaTeX-float "htb"
 LaTeX-figure-label "fig:"
 LaTeX-table-label "fig:"
 outline-minor-mode-prefix "\C-c\C-o"
 ;; TeX-one-master "<none>"
 TeX-auto-local ".auctex/")

(provide 'my-latex)

  
