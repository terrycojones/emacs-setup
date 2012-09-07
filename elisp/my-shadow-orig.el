(defun find-shadows (path)
  (interactive "P")
  (save-excursion
    (let
	(files
	 dir
	 output-buffer)

      (while path
	(let*
	    (orig-dir
	     files-seen-this-dir
	     (dir (car path))
	     (curr-files (directory-files dir nil ".*\.elc?" t)))
	  (while curr-files
	    (let ((curr-file
		   (substring
		    (car curr-files) 0
		    (- (length (car curr-files))
		       (if (equal (substring (car curr-files) -1) "c") 4 3)))))

	      (if (not (member curr-file files-seen-this-dir))
		  (progn
		    (setq files-seen-this-dir (cons curr-file files-seen-this-dir))
	      
		    (if (setq orig-dir (assoc curr-file files))
			(progn
			  (if (null output-buffer)
			      (progn
				(setq output-buffer (get-buffer-create "*Shadows*"))
				(display-buffer output-buffer)
				(set-buffer output-buffer)
				(erase-buffer)))
		
			  (insert (format "%s/%s shadows\n%s/%s\n\n"
					  (car (cdr orig-dir)) curr-file
					  dir curr-file)))

		      (setq files (cons (list curr-file dir) files)))))

	      (setq curr-files (cdr curr-files))))
	  (setq path (cdr path))))
      (not (null output-buffer)))))

(find-shadows load-path)
