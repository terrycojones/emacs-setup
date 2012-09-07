(defvar unscroll-point (make-marker)
  "Cursor position for next call to 'unscroll'.")

(defvar unscroll-window-start (make-marker)
  "Window start position for next call to 'unscroll'.")

(defvar unscroll-hscroll 0
  "Horizontal window position  for next call to 'unscroll'.")

(put 'scroll-up           'unscrollable t)
(put 'scroll-down         'unscrollable t)
(put 'scroll-left         'unscrollable t)
(put 'scroll-right        'unscrollable t)
(put 'beginning-of-buffer 'unscrollable t)
(put 'end-of-buffer       'unscrollable t)


(defun unscroll-maybe-remember ()
  (if (and
       (symbolp last-command)
       (not (get last-command 'unscrollable)))
      (progn
	(set-marker unscroll-point (point))
	(set-marker unscroll-window-start (window-start))
	(setq unscroll-hscroll (window-hscroll)))))


(defun unscroll ()
  "Revert to 'unscroll-point' and 'unscroll-window-start' after a series
of scrolling commands."

  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))


(defadvice scroll-up (before remember-for-unscroll activate compile)
  "Remember where we started from for subsequent unscrolling."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll activate compile)
  "Remember where we started from for subsequent unscrolling."
  (unscroll-maybe-remember))

(defadvice scroll-left (before remember-for-unscroll activate compile)
  "Remember where we started from for subsequent unscrolling."
  (unscroll-maybe-remember))

(defadvice scroll-right (before remember-for-unscroll activate compile)
  "Remember where we started from for subsequent unscrolling."
  (unscroll-maybe-remember))

(defadvice beginning-of-buffer (before remember-for-unscroll activate compile)
  "Remember where we started from for subsequent unscrolling."
  (unscroll-maybe-remember))

(defadvice end-of-buffer (before remember-for-unscroll activate compile)
  "Remember where we started from for subsequent unscrolling."
  (unscroll-maybe-remember))

(provide 'my-unscroll)
