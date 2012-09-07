(require 'my-buffers)

(defun html-accents-to-iso ()
  (interactive)
  (save-excursion
    (let
	((old-cfs case-fold-search))

      (setq case-fold-search nil)
      
      (beginning-of-buffer)
      (message "Replacing &aacute;...")
      (while (search-forward "&aacute;" nil t)
	(replace-match "á" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &eacute;...")
      (while (search-forward "&eacute;" nil t)
	(replace-match "é" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &iacute;...")
      (while (search-forward "&iacute;" nil t)
	(replace-match "í" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &oacute;...")
      (while (search-forward "&oacute;" nil t)
	(replace-match "ó" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &uacute;...")
      (while (search-forward "&uacute;" nil t)
	(replace-match "ú" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Aacute;...")
      (while (search-forward "&Aacute;" nil t)
	(replace-match "Á" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Eacute;...")
      (while (search-forward "&Eacute;" nil t)
	(replace-match "É" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Iacute;...")
      (while (search-forward "&Iacute;" nil t)
	(replace-match "Í" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Oacute;...")
      (while (search-forward "&Oacute;" nil t)
	(replace-match "Ó" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Uacute;...")
      (while (search-forward "&Uacute;" nil t)
	(replace-match "Ú" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &agrave;...")
      (while (search-forward "&agrave;" nil t)
	(replace-match "à" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &egrave;...")
      (while (search-forward "&egrave;" nil t)
	(replace-match "è" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &igrave;...")
      (while (search-forward "&igrave;" nil t)
	(replace-match "ì" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &ograve;...")
      (while (search-forward "&ograve;" nil t)
	(replace-match "ò" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &ugrave;...")
      (while (search-forward "&ugrave;" nil t)
	(replace-match "ù" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Agrave;...")
      (while (search-forward "&Agrave;" nil t)
	(replace-match "À" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Egrave;...")
      (while (search-forward "&Egrave;" nil t)
	(replace-match "È" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Igrave;...")
      (while (search-forward "&Igrave;" nil t)
	(replace-match "`I" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Ograve;...")
      (while (search-forward "&Ograve;" nil t)
	(replace-match "Ò" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Ugrave;...")
      (while (search-forward "&Ugrave;" nil t)
	(replace-match "Ù" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &uuml;...")
      (while (search-forward "&uuml;" nil t)
	(replace-match "ü" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &ccedil;...")
      (while (search-forward "&ccedil;" nil t)
	(replace-match "ç" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &ntilde;...")
      (while (search-forward "&ntilde;" nil t)
	(replace-match "ñ" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Uuml;...")
      (while (search-forward "&Uuml;" nil t)
	(replace-match "Ü" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Ccedil;...")
      (while (search-forward "&Ccedil;" nil t)
	(replace-match "Ç" nil t))
    
      (beginning-of-buffer)
      (message "Replacing &Ntilde;...")
      (while (search-forward "&Ntilde;" nil t)
	(replace-match "Ñ" nil t))
    
      (setq case-fold-search old-cfs)
      )))


(defun html-to-htm4l ()
  "Change HTML into equivalent HTM4L text."
  (interactive)
  (save-excursion
    (let
	((subs (list
		"height[ 	]*=[ 	]*\"?\\([0-9]+%?\\)\"?[ 	]*width[ 	]*=[ 	]*\\\"?\\([0-9]+%?\\)\\\"?" "f_hw(\\1, \\2)"
		"width[ 	]*=[ 	]*\"?\\([0-9]+%?\\)\"?[ 	]*height[ 	]*=[ 	]*\\\"?\\([0-9]+%?\\)\\\"?" "f_hw(\\1, \\2)"
		"height[ 	]*=[ 	]*\"?\\([0-9]+%?\\)\"?" "f_h(\\1)"
		"width[ 	]*=[ 	]*\"?\\([0-9]+%?\\)\"?" "f_w(\\1)"
		
		"colspan[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?[ 	]*rowspan[ 	]*=[ 	]*\\\"?\\([0-9]+\\)\\\"?" "f_cr(\\1, \\2)"
		"rowspan[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?[ 	]*colspan[ 	]*=[ 	]*\\\"?\\([0-9]+\\)\\\"?" "f_rc(\\1, \\2)"
		"colspan[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_cols(\\1)"
		"rowspan[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_rows(\\1)"
		
		"cellspacing[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?[ 	]*cellpadding[ 	]*=[ 	]*\\\"?\\([0-9]+\\)\\\"?" "f_csp(\\1, \\2)"
		"cellpadding[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?[ 	]*cellspacing[ 	]*=[ 	]*\\\"?\\([0-9]+\\)\\\"?" "f_cps(\\1, \\2)"
		"cellspacing[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_cs(\\1)"
		"cellpadding[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_cp(\\1)"
		
		"marginheight[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?[ 	]*marginwidth[ 	]*=[ 	]*\\\"?\\([0-9]+\\)\\\"?" "f_mhw(\\1, \\2)"
		"marginwidth[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?[ 	]*marginheight[ 	]*=[ 	]*\\\"?\\([0-9]+\\)\\\"?" "f_mwh(\\1, \\2)"
		"marginheight[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_mh(\\1)"
		"marginwidth[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_mw(\\1)"
		
		"maxlength[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_ml(\\1)"
		
		"border[ 	]*=[ 	]*\"?[ 	]*\\([0-9]+\\)[ 	]*\"?" "f_b(\\1)"
		"f_b(\\([01]\\))" "f_b\\1"
		
		"size[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_sz(\\1)"
		
		"\\(bg\\)?color[ 	]*=[ 	]*\"?\\([#0-9A-z]+\\)\"?" "f_\\1c(`\\2')"
		"f_BGc(" "f_bgc("
		"f_\\(bg\\)?c(`#\\([0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]\\)')" "f_\\1c(``#'\\2')"
		
		"hspace[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?[ 	]*vspace[ 	]*=[ 	]*\\\"?\\([0-9]+\\)\\\"?" "f_hvs(\\1, \\2)"
		"vspace[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?[ 	]*hspace[ 	]*=[ 	]*\\\"?\\([0-9]+\\)\\\"?" "f_vhs(\\1, \\2)"
		"hspace[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_hs(\\1)"
		"vspace[ 	]*=[ 	]*\"?\\([0-9]+\\)\"?" "f_vs(\\1)"
		
		
		"\\(v\\)?align[ 	]*=[ 	]*\"?top\"?" "f_\\1at"
		"\\(v\\)?align[ 	]*=[ 	]*\"?bottom\"?" "f_\\1ab"
		"\\(v\\)?align[ 	]*=[ 	]*\"?left\"?" "f_\\1al"
		"\\(v\\)?align[ 	]*=[ 	]*\"?right\"?" "f_\\1ar"
		"\\(v\\)?align[ 	]*=[ 	]*\"?middle\"?" "f_\\1am"
		"\\(v\\)?align[ 	]*=[ 	]*\"?center\"?" "f_\\1ac"
		"f_Va\\([tblrmc]\\)" "f_va\\1"

		"<P>" "<p>"
		"<BR>" "<br>"
		"<CENTER>" "o_c"
		"</CENTER>" "c_c"
		"<BLOCKQUOTE>" "o_bq"
		"</BLOCKQUOTE>" "c_bq"
		"</TABLE>" "c_t"
		"</FONT>" "c_ft"
		"</FORM>" "c_f"
		"<br clear=all>" "a_bca"

		"m4_http" "a_http"
		"m4_rfile" "a_rfile"
		"m4_html_end" "a_html_end"
		"m4_mailto" "a_mailto"
		"m4_my_me" "my_nue"
		"m4_my_" "my_"
		
		"<\\(/\\)?TD\\([ 	>]\\)" "<\\1td\\2"
		"<\\(/\\)?TR\\([ 	>]\\)" "<\\1tr\\2"
		)
	       )
	 )

      (my-buffer-replace-from-list subs t))))


(defun html-add-langs-long (current pre-needed post-needed)
  (let
      ((start (point))
       (end (mark))
       start-col
       end-col
       (indent-str "")
       tmp
       (end-marker (make-marker))
       )

    ;; Make sure start <= end
    (if (> start end)
	(setq
	 tmp start
	 start end
	 end tmp))

    (goto-char start)
    (beginning-of-line)
    (setq start-col (- start (point)))
    (goto-char end)
    (beginning-of-line)
    (setq end-col (- end (point)))

    (setq tmp start-col)

    (while (> tmp 0)
      (setq
       indent-str (format " %s" indent-str)
       tmp (- tmp 1)))
    
    (set-marker end-marker end)
    
    (goto-char start)

    (setq tmp (if pre-needed t nil))
    
    (while pre-needed
      (insert "\n" indent-str (car pre-needed) "\n\n")
      (setq pre-needed (cdr pre-needed)))

    (insert (if tmp "\n" "") current "\n\n" indent-str)

    (goto-char end-marker)

    (if (not (eq start-col end-col))
	(insert "\n"))

    (while post-needed
      (insert "\n" indent-str (car post-needed) "\n\n")
      (setq post-needed (cdr post-needed)))

    (insert "\n" indent-str "ALL_ON\n")))

(defun html-add-langs-short (current pre-needed post-needed concise)
  (let
      ((start (point))
       (end (mark))
       start-col
       end-col
       (indent-str "")
       tmp
       (end-marker (make-marker))
       )

    ;; Make sure start <= end
    (if (> start end)
	(setq
	 tmp start
	 start end
	 end tmp))

    (goto-char start)
    (narrow-to-region start end)

    ;; Quote ' chars, with a special test to avoid existing &`#'39;
    (while (search-forward "'" nil t)
	
	(if (and
	     (save-excursion
	      (forward-char -4)
	      (not (looking-at "&`#'39;")))

	     (save-excursion
	      (forward-char -2)
	      (not (looking-at "`"))))
	   
	    (progn
	      (delete-char -1)
	      (insert "&`#'39;"))))

    (goto-char (point-max))
    (widen)
    (setq end (point))

    (goto-char start)
    (beginning-of-line)
    (setq start-col (- start (point)))
    (goto-char end)
    (beginning-of-line)
    (setq end-col (- end (point)))

    (if (not concise)
	(progn
	  (setq tmp start-col)

	  (while (> tmp 0)
	    (setq
	     indent-str (format " %s" indent-str)
	     tmp (- tmp 1)))))
    
    (set-marker end-marker end)
    
    (goto-char start)

    (setq tmp (if pre-needed t nil))
    
    (while pre-needed
      (if concise
	  (insert (car pre-needed) "(`')`'")
	(insert "\n" indent-str (car pre-needed) "(`')\n"))

      (setq pre-needed (cdr pre-needed)))

    (insert (if tmp "\n" "") current "(`")

    (goto-char end-marker)

    (insert "')")
    
    (if (not concise)
	(insert "\n"))

    (while post-needed
      (if concise
	  (insert "`'" (car post-needed) "(`')")
	(insert "\n" indent-str (car post-needed) "(`')\n"))
      (setq post-needed (cdr post-needed)))))

(defun html-eng-fre-spa-long ()
  (interactive)
  (html-add-langs-long "ENG_ON" nil '("FRE_ON" "SPA_ON")))

(defun html-eng-fre-spa-short-concise ()
  (interactive)
  (html-add-langs-short "IF_ENG" nil '("IF_FRE" "IF_SPA") t))

(defun html-eng-fre-spa-short-concise-next ()
  (interactive)
  (search-forward "`" nil t)
  (set-mark (point))
  (search-forward "')" nil t)
  (forward-char -2)
  (html-add-langs-short "IF_ENG" nil '("IF_FRE" "IF_SPA") t))

(defun html-eng-fre-spa-short ()
  (interactive)
  (html-add-langs-short "IF_ENG" nil '("IF_FRE" "IF_SPA") nil))

(defun html-eng-fre-spa-short-next ()
  (interactive)
  (search-forward "`" nil t)
  (set-mark (point))
  (search-forward "')" nil t)
  (forward-char -2)
  (html-add-langs-short "IF_ENG" nil '("IF_FRE" "IF_SPA") nil))

(defun html-eng-spa-long ()
  (interactive)
  (html-add-langs-long "ENG_ON" nil '("SPA_ON")))

(defun html-eng-spa-short-concise ()
  (interactive)
  (html-add-langs-short "IF_ENG" nil '("IF_SPA") t))

(defun html-eng-spa-short-concise-next ()
  (interactive)
  (search-forward "`" nil t)
  (set-mark (point))
  (search-forward "')" nil t)
  (forward-char -2)
  (html-add-langs-short "IF_ENG" nil '("IF_SPA") t))

(defun html-eng-spa-short ()
  (interactive)
  (html-add-langs-short "IF_ENG" nil '("IF_SPA") nil))

(defun html-eng-spa-short-next ()
  (interactive)
  (search-forward "`" nil t)
  (set-mark (point))
  (search-forward "')" nil t)
  (forward-char -2)
  (html-add-langs-short "IF_ENG" nil '("IF_SPA") nil))

(provide 'my-html)
