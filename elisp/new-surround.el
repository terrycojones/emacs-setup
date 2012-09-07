(defvar surround-s0 ""
  "*String 0, inserted at the beginning of the line.")

(defvar surround-copy-left "-"
  "*The left-hand string to be inserted repeatedly.")

(defvar surround-s1 " "
  "*String 1, inserted after surround-copy-left and before pre-existing text.")

(defvar surround-s2 " "
  "*String 2, inserted after pre-existing text.")

(defvar surround-copy-right "-"
  "*The right-hand string to be inserted repeatedly.")

(defvar surround-s3 ""
  "*String 3, inserted at the end of the line.")

(defvar surround-length (- fill-column 8)
  "*The line length to use when surrounding.")

(defvar surround-weight 0.5
  "*The proportion of space to allocate to the left when inserting copies.")

(defvar surround-wish-list "0312"
  "*Priority ordering of strings in case they don't all fit.")

(defvar surround-space-pad
  "*If t pad the existing text (if necessary) with spaces to meet the target line length.")

(defvar surround-extra-space-left
  "*If t and surround-space-pad is t and there is an odd number of padding spaces, put the extra to the left.")

(defvar surround-extra-copy-left
  "*If t and it is possible to insert an extra copy of surround-copy-left by allocating an extra space to that region, do so.")


(defun replace-nth (n l object)
  (cond
   ((or (null l) (< n 0)) nil)
   ((> n (length l)) l)
   (t (replace-nth-r n l object))))

(defun replace-nth-r (n l object)
  (cond
   ((zerop n) (append (list object) (cdr l)))
   (t (append (list (car l)) (replace-nth-r (1- n) (cdr l) object)))))

(defun c-surround-region ()
  (interactive)
  (save-excursion
    (let
	((lines (count-lines (region-beginning) (region-end))))

      (goto-char (region-beginning))
    
      (cond

       ((= lines 1) (c-surround-line-1))
       ((= lines 2) (c-surround-line-start) (forward-line 1) (c-surround-line-end))
       (t
	(c-surround-line-start)
	(while (> lines 2)
	  (forward-line 1)
	  (c-surround-line-int)
	  (setq lines (1- lines)))
	(forward-line)
	(c-surround-line-end))))))
      
  

(defun html-comment-line ()
  (interactive)
  (general-surround-line "<!--" "" " " " " "" "-->" 84 1 nil nil nil "0123"))

(defun binsh-surround-line ()
  (interactive)
  (general-surround-line "#!/bin/sh" "" "" "" "" "" surround-length 1 nil nil nil "0123"))

(defun m4-surround-line-dashed ()
  (interactive)
  (general-surround-line "dnl " "-" " " " " "-" "" surround-length surround-weight t t t "0312"))

(defun m4-un-surround-line-dashed ()
  (interactive)
  (general-un-surround-line "dnl " "-" " " " " "-" "" t))

(defun dash-surround-line ()
  (interactive)
  (general-surround-line "" "-" " " " " "-" "" surround-length 0.5 t t t "1203"))

(defun c-surround-line-start ()
  (interactive)
  (general-surround-line "/*" " " " " " " " " "*" (1- surround-length) surround-weight t t t "0312"))

(defun c-surround-line-end ()
  (interactive)
  (general-surround-line " *" " " " " " " " " "*/" surround-length surround-weight t t t "0312"))

(defun c-surround-line-int ()
  (interactive)
  (general-surround-line " *" " " " " " " " " "*" (1- surround-length) surround-weight t t t "0312"))

(defun c-dash-line ()
  (interactive)
  (general-surround-line "/* " "-" " " " " "-" " */" surround-length surround-weight t t t "0312"))

(defun c-surround-line-1 ()
  (interactive)
  (general-surround-line "/* " " " " " " " " " "*/" surround-length surround-weight t t t "0312"))

(defun c-surround-line-2 ()
  (interactive)
  (general-surround-line "/*" "*" " " " " "*" "*/" surround-length surround-weight t t t "0312"))

(defun c-surround-line-dashed ()
  (interactive)
  (general-surround-line "/* " "-" " " " " "-" " */" surround-length surround-weight t t t "0312"))

(defun c-un-surround-line-dashed ()
  (interactive)
  (general-un-surround-line "/* " "-" " " " " "-" " */" t))

(defun c-re-surround-line-dashed ()
  (interactive)
  (c-un-surround-line-dashed)
  (c-surround-line-dashed))

(defun m4-re-surround-line-dashed ()
  (interactive)
  (m4-un-surround-line-dashed)
  (m4-surround-line-dashed))

(defun java-surround-line-dashed ()
  (interactive)
  (general-surround-line "// " "-" " " " " "-" "" surround-length surround-weight t t t "0312"))

(defun java-un-surround-line-dashed ()
  (interactive)
  (general-un-surround-line "# " "-" " " " " "-" "" t))

(defun java-re-surround-line-dashed ()
  (interactive)
  (java-un-surround-line-dashed)
  (java-surround-line-dashed))

(defun python-surround-line-dashed ()
  (interactive)
  (general-surround-line "# " "-" " " " " "-" "" surround-length surround-weight t t t "0312"))

(defun python-un-surround-line-dashed ()
  (interactive)
  (general-un-surround-line "# " "-" " " " " "-" "" t))

(defun python-re-surround-line-dashed ()
  (interactive)
  (python-un-surround-line-dashed)
  (python-surround-line-dashed))

(defun lisp-surround-line-dashed ()
  (interactive)
  (general-surround-line ";; " "-" " " " " "-" "" surround-length surround-weight t t t "0312"))

(defun lisp-un-surround-line-dashed ()
  (interactive)
  (general-un-surround-line ";; " "-" " " " " "-" "" t))

(defun lisp-re-surround-line-dashed ()
  (interactive)
  (lisp-un-surround-line-dashed)
  (lisp-surround-line-dashed))

(defun surround-line (&optional prompt)
  (interactive "P")
   (if prompt
       (general-surround-line
	
	 (setq surround-s0               (read-from-minibuffer "s0: " surround-s0))
	 (setq surround-copy-left        (read-from-minibuffer "copy left string : " surround-copy-left))
	 (setq surround-s1               (read-from-minibuffer "s1: " surround-s1))
	 (setq surround-s2               (read-from-minibuffer "s2: " surround-s2))
	 (setq surround-copy-right       (read-from-minibuffer "copy right string : " surround-copy-right))
	 (setq surround-s3               (read-from-minibuffer "s3: " surround-s3))

	 (setq surround-length           (string-to-number
					  (read-from-minibuffer "Maximum line length: "
							  (number-to-string surround-length))))
	 (setq surround-weight           (string-to-number
					  (read-from-minibuffer "Left hand copies weight: "
							  (number-to-string surround-weight))))
	 (setq surround-extra-copy-left  (yes-or-no-p "extra copy goes left? "))
	 (setq surround-space-pad        (yes-or-no-p "Pad with spaces to achieve target length? "))
	 (setq surround-extra-space-left (yes-or-no-p "Should an extra space be added on the left? "))
	       
	 (setq surround-wish-list        (read-from-minibuffer "wish list " surround-wish-list)))
	 
     (general-surround-line)))

(defun general-surround-line (&optional s0 copy-left s1 s2 copy-right s3 target-len weight extra-copy-left space-pad extra-space-left wish-list)
  (interactive "P")
  (save-excursion
    (let* (current-line-len
	   available-len
	   s0-len
	   copy-left-len
	   s1-len
	   s2-len
	   copy-right-len
	   s3-len
	   s0123-len)

      (or s0
	  (setq
	   s0 surround-s0
	   copy-left surround-copy-left
	   s1 surround-s1
	   s2 surround-s2
	   copy-right surround-copy-right
	   s3 surround-s3
	   target-len surround-length
	   weight surround-weight
	   extra-copy-left t
	   space-pad t
	   extra-space-left t
	   wish-list "1203"))

      (setq
       current-line-len (- (progn (end-of-line) (point)) (progn (beginning-of-line) (point)))
       available-len (- target-len current-line-len))

      ;; (message "target len %d, current len %d, available %d" target-len current-line-len available-len copy-left)
      ;; (message "s0 '%s' copy-left '%s' s1 '%s' s2 '%s' copy-right '%s' s3 '%s'" s0 copy-left s1 s2 copy-right s3)
      
      (if (zerop current-line-len)
	  ;; Empty s1 and s2 as these are only inserted if there is pre-existing text.
	  (setq
	   s1 ""
	   s2 ""))

      (setq
       s0-len         (length s0)
       copy-left-len  (length copy-left)
       s1-len         (length s1)
       s2-len         (length s2)
       copy-right-len (length copy-right)
       s3-len         (length s3)
       s0123-len      (+ s0-len s1-len s2-len s3-len))

      (cond
	 
       ((<= s0123-len available-len)

	;; We can insert all 4 strings and 0 or more copies of 'copy-left' and 'copy-right'.

	(let*
	    ((copy-space (- available-len s0123-len))
	     (copy-space-left (+ (floor (* copy-space weight))
				 (if extra-copy-left (% copy-space 2) 0)))
	     (copies-left (if (zerop copy-left-len) 0
			    (/ copy-space-left copy-left-len)))
	     (copy-space-right (- copy-space copy-space-left))
	     (copies-right (if (zerop copy-right-len) 0
			     (/ copy-space-right copy-right-len)))

	     (spaces (if space-pad
			 (- available-len
			    s0123-len
			    (* copies-left copy-left-len)
			    (* copies-right copy-right-len))
		       0))

	     (spaces-left (+ (/ spaces 2) (% spaces 2)))
	     (spaces-right (- spaces spaces-left)))

	  ;; (message "copy space: %d\ncopy-space-left: %d\ncopies-left: %d\ncopy-space-right: %d\ncopies-right: %d\nspaces: %d\nspaces-left: %d\nspaces-right: %d\n" copy-space copy-space-left copies-left copy-space-right copies-right spaces spaces-left spaces-right)

	  (beginning-of-line)

	  (insert s0)

	  (while (> copies-left 0)
	    (insert copy-left)
	    (setq copies-left (1- copies-left)))
      
	  (insert s1)

	  (insert-char ?  spaces-left nil)
      
	  (end-of-line)

	  (insert-char ?  spaces-right nil)

	  (insert s2)

	  (while (> copies-right 0)
	    (insert copy-right)
	    (setq copies-right (1- copies-right)))
      
	  (insert s3)))

	 ;;;;;;;;;;;;;;;;;;;;;;; Can we include any of the delimiters at all?
	 ;;;;;;;;;;;;;;;;;;;;;;; Check to see if we can at least manage the highest priority one.
	 
       (wish-list

	(if
	    (and
	     (stringp wish-list)
	     (= 4 (length wish-list))
	     (string-match "0" wish-list)
	     (string-match "1" wish-list)
	     (string-match "2" wish-list)
	     (string-match "3" wish-list))

	    (let*
		((i 0)
		 (hit-list '(nil nil nil nil))
		 (s-list (list s0 s1 s2 s3))
		 (s-len-list (list s0-len s1-len s2-len s3-len))

		 ;; wish-list is a list of the string indices in desired order.
		 (wish-list
		  (list
		   (string-to-number (substring wish-list 0 1))
		   (string-to-number (substring wish-list 1 2))
		   (string-to-number (substring wish-list 2 3))
		   (string-to-number (substring wish-list 3 4))))E)

	      ;; (message (format "wish list is %s\n" wish-list))
		   
	      (while (< i 4)
		(if
		    (>= available-len (nth (nth i wish-list) s-len-list))
		    (progn
		      ;; (message "Including string %d.\n" (nth i wish-list))
		      ;; (message (format "Hit list is %s\n" hit-list))
		      (setq
		       available-len (- available-len (nth (nth i wish-list) s-len-list))
		       hit-list (replace-nth (nth i wish-list) hit-list t)))
		  (setq i 4))
		(setq i (1+ i)))


	      (beginning-of-line)
		
	      (if (nth 0 hit-list) (insert s0))
	      (if (nth 1 hit-list) (insert s1))

	      (end-of-line)

	      (if (nth 2 hit-list) (insert s2))
	      (if (nth 3 hit-list) (insert s3)))

	  ;; (message "Malformed wish-list passed to general-surround-line. Use a string permutation of \"1234\"")
	  ))

       (t
	;; (message "The strings passed to general-surround-line are too long to permit action on this line.")
	)))))


(defun un-surround-line (&optional prompt)
  (interactive "P")
   (if prompt
       (general-un-surround-line
	
	 (setq surround-s0         (read-from-minibuffer "s0: " surround-s0))
	 (setq surround-copy-left  (read-from-minibuffer "copy left string : " surround-copy-left))
	 (setq surround-s1         (read-from-minibuffer "s1: " surround-s1))
	 (setq surround-s2         (read-from-minibuffer "s2: " surround-s2))
	 (setq surround-copy-right (read-from-minibuffer "copy right string : " surround-copy-right))
	 (setq surround-s3         (read-from-minibuffer "s3: " surround-s3))
	 (setq surround-space-pad  (yes-or-no-p "Delete extra spaces? ")))

     (general-un-surround-line)))

(defun general-un-surround-line (&optional s0 copy-left s1 s2 copy-right s3 delete-surrounding-whitespace)
  (interactive "P")
  (save-excursion

    (or s0
	(setq
	 s0                            surround-s0
	 copy-left                     surround-copy-left
	 s1                            surround-s1
	 s2                            surround-s2
	 copy-right                    surround-copy-right
	 s3                            surround-s3
	 delete-surrounding-whitespace surround-space-pad))
      
    (let* (beg
	   point
	   (copy-right-found t)
	   (s0-len         (length s0))
	   (copy-left-len  (length copy-left))
	   (s1-len	   (length s1))
	   (s2-len	   (length s2))
	   (copy-right-len (length copy-right))
	   (s3-len	   (length s3)))

      ;; (message "s0 '%s' copy-left '%s' s1 '%s' s2 '%s' copy-right '%s' s3 '%s'" s0 copy-left s1 s2 copy-right s3)
      
      (beginning-of-line)

      (setq point (point))
      (setq beg point)

      (if (and (> s0-len 0)
	       (>= (- (buffer-size) point) s0-len)
	       (string-match s0 (buffer-substring point (+ point s0-len))))
	  (progn
	    ;; (message "Deleting s0: '%s'" s0)
	    (delete-char s0-len))
	;; (message "Not looking at s0: '%s'" s0)
	)

      (if (> copy-left-len 0)
	  (while (and (>= (- (buffer-size) point) copy-left-len)
		      (string-match copy-left (buffer-substring point (+ point copy-left-len))))
	    ;; (message "Deleting copy-left: '%s'" copy-left)
	    (delete-char copy-left-len)))

      (if (and (> s1-len 0)
	       (>= (- (buffer-size) point) s1-len)
	       (string-match s1 (buffer-substring point (+ point s1-len))))
	  (progn
	    ;; (message "Deleting s1: '%s'" s1)
	    (delete-char s1-len))
	;; (message "Not looking at s1: '%s'" s1)
	)

      (if delete-surrounding-whitespace (delete-horizontal-space))

      (end-of-line)

      (if (and (> s3-len 0)
	       (>= (- (point) s3-len) beg))
	  (progn
	    (backward-char s3-len)
	    (setq point (point))
	    
	    (if (string-match s3 (buffer-substring point (+ point s3-len)))
		(progn
		  ;; (message "Deleting s3: '%s'" s3)
		  (delete-char s3-len))
	      ;; (message "Not looking at s3: '%s'" s3)
	      (forward-char s3-len))))

	
      (if (> copy-right-len 0)
	  (progn
	    (while (and copy-right-found
			(>= (- (point) copy-right-len) beg))
		
	      (backward-char copy-right-len)
	      (setq point (point))

	      (if (string-match copy-right (buffer-substring point (+ point copy-right-len)))
		  (progn
		    ;; (message "Deleting copy-right: '%s'" copy-right)
		    (delete-char copy-right-len))
		(setq copy-right-found nil)))

	    (if (null copy-right-found) (forward-char copy-right-len))))

      (if (and (> s2-len 0)
	       (>= (- (point) s2-len) beg))
	  (progn
	    (backward-char s2-len)
	    (setq point (point))

	    (if (string-match s2 (buffer-substring point (+ point s2-len)))
		(progn
		  ;; (message "Deleting s2: '%s'" s2)
		  (delete-char s2-len))
	      ;; (message "Not looking at s2: '%s'" s2)
	      (forward-char s2-len))))

      (if delete-surrounding-whitespace (delete-horizontal-space)))))
	

(defun re-surround-line (&optional prompt)
  (interactive "P")
  (if prompt
      (progn
	(setq surround-s0               (read-from-minibuffer "s0: " surround-s0))
	(setq surround-copy-left        (read-from-minibuffer "copy left string : " surround-copy-left))
	(setq surround-s1               (read-from-minibuffer "s1: " surround-s1))
	(setq surround-s2               (read-from-minibuffer "s2: " surround-s2))
	(setq surround-copy-right       (read-from-minibuffer "copy right string : " surround-copy-right))
	(setq surround-s3               (read-from-minibuffer "s3: " surround-s3))

	(setq surround-length           (string-to-number
					 (read-from-minibuffer "Maximum line length: "
							       (number-to-string surround-length))))
	(setq surround-weight           (string-to-number
					 (read-from-minibuffer "Left hand copies weight: "
							       (number-to-string surround-weight))))
	(setq surround-extra-copy-left  (yes-or-no-p "extra copy goes left? "))
	(setq surround-space-pad        (yes-or-no-p "Pad with spaces to achieve target length? "))
	(setq surround-extra-space-left (yes-or-no-p "Should an extra space be added on the left? "))
	       
	(setq surround-wish-list        (read-from-minibuffer "wish list " surround-wish-list))))
  
  (general-un-surround-line)
  (general-surround-line))


(provide 'my-surround)
