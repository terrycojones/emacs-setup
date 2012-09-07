(defvar surround-default-string "-"
  "*The string used to surround the line.")

(defvar surround-default-separator " "
  "*The string used between surround-default-string and the pre-existing line.")

(defvar surround-default-length (+ fill-column 8)
  "*The default line length to use when surrounding.")

(defun surround-line (&optional prompt)
  (interactive "P")
   (if prompt
       (basic-surround-line
	(read-from-minibuffer "Repeat string: " surround-default-string)
	(read-from-minibuffer "Delimiter string: " surround-default-separator)
	(string-to-number
	 (read-from-minibuffer "Maximum line length: "
			       (number-to-string surround-default-length))))
	(basic-surround-line
	 surround-default-string
	 surround-default-separator
	 surround-default-length)))

(defun basic-surround-line (ins-str sep-str target-line-len)
  (if (equal ins-str "")
      (message "The insert string for basic-surround-line must be non-empty.")
    (save-excursion
    
      (let (beg
	    end
	    line-len
	    (ins-len (length ins-str)))
	  
	(beginning-of-line)
	(setq beg (point))
	(end-of-line)
	(setq end (point))
	(setq line-len (- end beg))

	(if (zerop line-len)
	    (let
		((ncopies (/ target-line-len ins-len)))
	      (while (> ncopies 0)
		(insert ins-str)
		(setq ncopies (1- ncopies))))
	
	  (progn
	    (let*
		((sep-len (length sep-str))
		 (side-space (/ (- target-line-len (+ line-len (* sep-len 2))) 2))
		 (ncopies (/ side-space ins-len))
		 (nspaces (- side-space (* ncopies ins-len))))

	      (if (<= ncopies 0)
		  (message "No copies of '%s' could be inserted." ins-str)

		(progn

		  (beginning-of-line)

		  (let ((i ncopies))
		    (while (> i 0)
		      (insert ins-str)
		      (setq i (1- i))))
      
		  (insert-char ?  nspaces nil)

		  (insert sep-str)
      
		  (end-of-line)
		  (insert sep-str)

		  (insert-char ?  nspaces nil)

		  (let ((i ncopies))
		    (while (> i 0)
		      (insert ins-str)
		      (setq i (1- i))))

		  (if (and
		       (equal 1 ins-len)
		       (equal 1
			      (- target-line-len
				 (+ line-len
				    (* 2 sep-len)
				    (* 2 nspaces)
				    (* 2 ncopies ins-len)))))
		      (insert ins-str))

		  (delete-horizontal-space))))))))))

(defun un-surround-line (&optional prompt)
  (interactive "P")
  (if prompt
      (basic-un-surround-line
       (read-from-minibuffer "Repeat string: " surround-default-string)
       (read-from-minibuffer "Delimiter string: " surround-default-separator))
    (basic-un-surround-line
     surround-default-string
     surround-default-separator)))

(defun basic-un-surround-line (del-str sep-str)
  (if (equal del-str "")
      (message "The deletion string for basic-un-surround-line must be non-empty.")
    (save-excursion
      (let
	  (beg
	   (del-len (length del-str))
	   (sep-len (length sep-str)))

	(beginning-of-line)
	(delete-horizontal-space)
	(setq beg (point))
      
	(while (looking-at del-str)
	  (delete-char del-len))

	(delete-horizontal-space)
	(if (and (not (equal sep-str "")) (looking-at sep-str))
	    (delete-char sep-len))
      
	(end-of-line)
	(delete-horizontal-space)
	(backward-char del-len)
      
	(while (and (>= (point) beg) (looking-at del-str))
	  (delete-char del-len)
	  (backward-char del-len))

	(end-of-line)
	(delete-horizontal-space)

	(backward-char sep-len)
      
	(if (and (>= (point) beg) (not (equal sep-str "")) (looking-at sep-str))
	    (delete-char sep-len))))))

(defun re-surround-line (&optional prompt)
  (interactive "P")
  (let
      ((ins-str (if prompt
		    (read-from-minibuffer "Repeat string: " surround-default-string)
		  surround-default-string))
		
       (sep-str (if prompt
		    (read-from-minibuffer "Delimiter string: " surround-default-separator)
		  surround-default-separator))
       
       (line-len (if prompt
		     (string-to-number
		      (read-from-minibuffer "Maximum line length: "
					    (number-to-string surround-default-length)))
		  surround-default-length)) )
	
	(basic-un-surround-line ins-str sep-str)
	(basic-surround-line ins-str sep-str line-len)))
