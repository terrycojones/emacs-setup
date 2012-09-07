(defun browse-pointer-shapes ()
  "Browse all pointer shapes.  Choose by saying `y'.
Return shape chosen as an integer, or nil if none chosen."
  (interactive)
  (require 'avoid)
  (catch 'shape-choice
    (all-completions
     "x-pointer-" obarray
     #'(lambda (sym)
	 (and (boundp sym)
	      (let ((val (symbol-value sym)))
		(and (integerp val)
		     (progn
		       (mouse-avoidance-set-pointer-shape val)
		       (if (y-or-n-p (format "(%s) Done? " val))
			   (throw 'shape-choice val))))))))))


(defun ron (url)
  (interactive "sURL: ")
  (save-excursion
    (let
	(deactivate-mark
	(buf (buffer-from-buffer-name "ron.htm4l")))

      (if buf (switch-to-buffer buf)
	(find-file (expand-file-name "~/html/other/ron.htm4l")))
      
      (beginning-of-buffer)
      (search-forward "white_body_open")
      (end-of-line)
      (insert "\n\n" url)
      
      (beginning-of-line)
      (delete-horizontal-space)
      
      (if
	  (looking-at "http://")
	  (delete-char 7))
      (insert "a_http_url(`")
      (end-of-line)  
      (insert "') <br>")
      (backward-char 2)
      (save-buffer)
      (compile "make ron.html")
      (delete-other-windows)
      (kill-buffer nil))))

