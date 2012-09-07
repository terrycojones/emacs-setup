;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BBDB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (require 'bbdb))

(when
    i-use-bbdb
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message 'vm 'sendmail)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
  (add-hook 'mail-setup-hook 'bbdb-define-all-aliases)
  (add-hook 'message-send-hook 'mc-bbdb-auto-encrypt-send)
  (load-library "mc-bbdb")
  ;; (bbdb-insinuate-message)
  ;; (bbdb-insinuate-w3)
  ;; (bbdb-insinuate-sc)

  (setq
   bbdb-pop-up-display-layout 'one-line
   bbdb-pop-up-target-lines 1

;;    bbdb-ignore-most-messages-alist '(("From: " . "alerts@citibank.com")
;; 				     ("From: " . "root@")
;; 				     ("From: " . "MAILER-DAEMON@")
;; 				     ("To: " . "@securityfocus.com")
;; 				     ("To: " . "discuss@wine.codeweavers.com")
;; 				     ("X-Spam-Flag: ". "YES")
;; 				     )
  

   bbdb/mail-auto-create-p 'bbdb-prune-not-to-me
   bbdb/news-auto-create-p 'bbdb-prune-not-to-me
   )

  (defun bbdb-prune-not-to-me ()
    "defun called when bbdb is trying to automatically create a record.  Filters out
anything not actually adressed to me then passes control to 'bbdb-ignore-some-messages-hook'.
Also filters out anything that is precedense 'junk' or 'bulk'  This code is from
Ronan Waide <waider@waider.ie"
    (let ((case-fold-search t)
	  (done nil)
	  (b (current-buffer))
	  (marker (bbdb-header-start))
	  field regexp fieldval)
      (set-buffer (marker-buffer marker))
      (save-excursion
	;; The buffer we're in is the mail file, narrowed to the current message.
	(let (to cc precedence)
	  (goto-char marker)
	  (setq to (bbdb-extract-field-value "To"))
	  (goto-char marker)
	  (setq cc (bbdb-extract-field-value "Cc"))
	  (goto-char marker)
	  (setq precedence (bbdb-extract-field-value "Precedence"))
	  ;; Here's where you put your email information.
	  ;; Basically, you just add all the regexps you want for
	  ;; both the 'to' field and the 'cc' field.
	  (if (and (not (string-match "terry@" (or to "")))
		   (not (string-match "tc.jones@" (or cc ""))))
	      (progn
		(message "BBDB unfiling; message to: %s cc: %s" (or to "no-one") (or cc "no-one"))
		;; Return nil so that the record isn't added.
		nil)

	    (if (string-match "junk" (or precedence ""))
		(progn
		  (message "precedence set to junk, bbdb ignoring.")
		  nil)

	      ;; Otherwise add, subject to filtering
	      (bbdb-ignore-some-messages-hook)))))))
  
  )