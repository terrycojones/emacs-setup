;;
;; This could really be done with C-a M-C-f C-x C-e
;; but I want to bind it to a single key.
;;
(defun eval-line ()
  (interactive)
  (beginning-of-line)
  (forward-sexp)
  (eval-last-sexp nil))

(provide 'my-emacs-lisp)
