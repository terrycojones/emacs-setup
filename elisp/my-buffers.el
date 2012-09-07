(defadvice switch-to-buffer (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only, unless
given a prefix argument."

  (interactive
   (list (read-buffer "Switch to buffer: "
		      (other-buffer)
		      (null current-prefix-arg)))))

(defun buffer-visiting-file (filename)
  
  "Return the buffer that is visiting `filename', or `nil' if
no such buffer exists."
  
  (interactive "s")
  (let
      ((buffers (buffer-list)))
    (catch 'buf
      (progn
	(while buffers
	  (if (equal filename (buffer-file-name (car buffers)))
	      (throw 'buf (car buffers)))
	  (setq buffers (cdr buffers)))
	(throw 'buf nil)))))

(defun buffer-from-buffer-name (name)
  
  "Return the buffer whose name is `name', or `nil' if
no such buffer exists."
  
  (interactive "s")
  (let
      ((buffers (buffer-list)))
    (catch 'buf
      (progn
	(while buffers
	  (if (equal name (buffer-name (car buffers)))
	      (throw 'buf (car buffers)))
	  (setq buffers (cdr buffers)))
	(throw 'buf nil)))))


(defun switch-to-buffer-by-name (name)

  "Switch to the buffer whose name is `name'. If the buffer does not exist, ding."
  
  (interactive "BBuffer name: ")
  (let
      ((buf (buffer-from-buffer-name name)))

    (if buf
	(switch-to-buffer buf)
      (message (format "No such buffer: `%s'." name))
      (ding))))

(defun my-buffer-replace (from to fold)
  "Replace from with to wherever it occurs in the buffer. Set case-fold-search to the value in fold."
  
  (beginning-of-buffer)
  (message "Replacing '%s' with '%s'..." from to)

  (let
	((old-cfs case-fold-search))

    (setq case-fold-search fold)
    
    (while (re-search-forward from nil t)
      (replace-match to t nil))

    (setq case-fold-search old-cfs)))
  

(defun my-buffer-replace-from-list (subs fold)
  "Do a replacement for each pair of elements in the list subs.
   Set case-fold-search to the value in fold."
  
  (while (not (null subs))
    (my-buffer-replace (car subs) (car (cdr subs)) fold)
    (setq subs (cdr (cdr subs)))))

(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
  (interactive)
  (let* ((totals '())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test 'equal)))
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let
            ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals)))
             ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                      total-buffers (length totals)))
      (dolist (item totals)
        (let
            ((key (car item))
             (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))


(provide 'my-buffers)
