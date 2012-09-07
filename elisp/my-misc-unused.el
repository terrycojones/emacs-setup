(defun find-line-with-length (len &optional testfun direction)

  "Return the buffer position of the start of the next line whose
  length satisfies (funcall test length len). If direction is >0, the
  next line is found, otherwise, the previous line is found. If no
  such line exists, return -1."

  ;; Use > for the test function if we didn't get a function,
  ;; and go forward if we didn't get a direction.
  (if (null testfun) (setq testfun '>))
  (if (null direction) (setq direction 1))

  (save-excursion
    (let
	(
	 (inc (if (> direction 0) 1 -1))
	 (done -1)
	 pos
	 )
      (while (and
	      (eq done -1)
	      (eq (forward-line inc) 0))
	(beginning-of-line)
	(setq pos (point))
	(end-of-line)
	(if (funcall testfun (- (point) pos) len)
	    (setq done pos)))

      pos)))
