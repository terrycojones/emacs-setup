(defun find-elisp-shadows (path)
  "Traverse path looking for elisp files that create shadows.  This
function does the work for the interactively-callable
list-load-path-shadows (which simply passes load-path and displays the
result.

A \(possibly null\) even-length list of files is returned. A file in
this list at position 2i shadows the file in position 2i+1. Emacs lisp
file suffixes \(.el and .elc\) are stripped from the file names in the
list."
  
  (let (shadows  ; To be be returned.
	files)   ; The file names ever encountered, with their directories.
    (while path
      (let* (orig-dir            ; The directory the file was first seen in.
	     files-seen-this-dir ; A list of files found so far in this directory.
	     (dir (car path))    ; The path directory being currently considered.
	     (curr-files         ; This directory's elisp files.
	      (directory-files dir nil ".*\.elc?" t))) 
	(while curr-files
	  (let ((curr-file
		 (substring
		  (car curr-files) 0
		  (- (length (car curr-files))
		     (if (equal (substring (car curr-files) -1) "c") 4 3)))))

	    ;; curr-file contains the current file name, with the suffix stripped.
	    
	    (if (not (member curr-file files-seen-this-dir))
		(progn
		  ;; curr-file has not been seen yet in this directory.
		  ;; This test prevents us declaring that xx.el shadows
		  ;; xx.elc (or vice-versa) when they are in the same directory.
		  (setq files-seen-this-dir (cons curr-file files-seen-this-dir))
	      
		  (if (setq orig-dir (assoc curr-file files))
		      ;; This file was seen before, we have a shadowing.
		      (setq shadows
			    (append shadows
				    (list (concat (car (cdr orig-dir)) "/" curr-file)
					  (concat dir "/" curr-file))))

		    ;; It hasn't been seen before, add it to the list of seen files.
		    (setq files (cons (list curr-file dir) files)))))

	    (setq curr-files (cdr curr-files))))
	(setq path (cdr path))))

    ;; Return the list of shadowings.
    shadows))

(defun list-load-path-shadows ()
  
  "List potential load-path problems. Directories in the load-path are
searched, in order, for emacs lisp files. When a previously encountered
file name is re-located, a message is displayed indicating that the
later file is \"shadowed\" by the earlier.

For example, suppose load-path is set to

\(\"/usr/gnu/emacs/site-lisp\" \"/usr/gnu/emacs/share/emacs/19.30/lisp\"\)

and that each of these directories contains a file called xxx.el. Then
xxx.el in the site-lisp directory is referred to by all of:
\(require 'xxx\), \(autoload .... \"xxx\"\), \(load-library \"xxx\"\) etc.

The first xxx.el file prevents emacs from seeing the second \(unless
the second is loaded explicitly via load-file.

When not intended, such shadowings can be the source of subtle
problems. For example, the above situation may have arisen because the
xxx package was not distributed with versions of emacs prior to
19.30. An emacs maintainer downloaded xxx from elsewhere and installed
it. Later, xxx was updated and included in the emacs distribution.
Unless the emacs maintainer checks for this, the new version of xxx
will be hidden behind the old \(which may no longer work with the new
emacs version\).

This function performs these checks and flags all possible
shadowings. Because a .el file may exist without a corresponding .elc
\(or vice-versa\), these suffixes are essentially ignored. A file
xx.elc in an early directory \(that does not contain xx.el\) is
considered to shadow a later file xx.el, and vice-versa.

When run interactively, the shadowings \(if any\) are displayed in a
buffer called \"*Shadows*\".  Shadowings are located by calling a
\(non-interactive\) companion function, find-elisp-shadows."
  
  (interactive)
  (let ((shadows (find-elisp-shadows load-path)))
    (if shadows
	(if noninteractive
	    ;; We are non-interactive, print shadows via message.
	    (while shadows
	      (message (format "%s shadows %s" (car shadows) (car (cdr shadows))))
	      (setq shadows (cdr (cdr shadows))))
	  (save-excursion
	    ;; We are interactive.
	    ;; Create the *Shadows* buffer and display shadowings there.
	    (let ((output-buffer (get-buffer-create "*Shadows*")))
	      (display-buffer output-buffer)
	      (set-buffer output-buffer)
	      (erase-buffer)
	      (while shadows
		(insert (format "%s shadows %s\n" (car shadows) (car (cdr shadows))))
		(setq shadows (cdr (cdr shadows))))))))))
