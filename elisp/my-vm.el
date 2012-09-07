(defun is-ymd-folder-p (buffer-name)
  (eq (string-match "IN-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\\( Summary\\)?" buffer-name) 0))

(defun visit-ymd-folder (ymd &optional new-frame)
  (let*
      ((old vm-frame-per-folder)
       (vm-frame-per-folder new-frame))
    (vm-visit-folder
     (expand-file-name
      (concat "~/mail/incoming/IN-" (my-yyyymmdd-string ymd))))
    (setq vm-frame-per-folder old)))

(defun buffer-to-ymd ()
  (let ((name (buffer-name)))
    (if (is-ymd-folder-p name)
        (mapcar 'string-to-number (list
				   (substring name 3 7)
				   (substring name 7 9)
				   (substring name 9 11)))
      (my-today))))

(define-key vm-mode-map "A"
  '(lambda ()
     (interactive)
     (let ((name (buffer-name)))
       (if (or (string= name "INBOX")
               (string= name "INBOX Summary")
               (eq (string-match "INBOX Presentation" name) 0))
           (message "use M-x vm-auto-archive-messages.")
         (vm-auto-archive-messages)))))

(define-key vm-mode-map "g"
  '(lambda ()
     (interactive)
     (let ((name (buffer-name)))
       (if (or (string= name "INBOX")
               (string= name "INBOX Summary")
               (eq (string-match "INBOX Presentation" name) 0)
               ;; The following for my per-day inboxes whose .spool files are created by procmail:
               ;; (is-ymd-folder-p name)
               )
           (vm-get-new-mail)
         (message "Only use g in INBOX.")))))

;;(define-key vm-mode-map "i"
;;  ;; Visit the incoming mail file for today, always in a new frame.
;;  '(lambda ()
;;     (interactive)
;;     (visit-ymd-folder (my-today) t)))

;; (define-key vm-mode-map [left]
;;   ;; Visit the previous day's incoming mail file.
;;   '(lambda ()
;;     (interactive)
;;     (visit-ymd-folder (my-previous-day (buffer-to-ymd))
;;                       ;; Ask for a new frame if we're not a ymd folder.
;;                       (not (is-ymd-folder-p (buffer-name))))))

;; (define-key vm-mode-map [right]
;;   ;; Visit the next day's incoming mail file, if it look like there is a
;;   ;; next day.
;;   '(lambda ()
;;     (interactive)
;;     (if (is-ymd-folder-p (buffer-name))
;;         (visit-ymd-folder (my-next-day (buffer-to-ymd)))
;;       (message "There is no tomorrow!"))))

;;; (define-key vm-mode-map "@" '(lambda () (interactive) (message "use M-x vm-send-digest.")))
;;; (define-key vm-mode-map "@" 'vm-send-digest)

(defun show-new-vm-version-available ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (let
       ((vm-version-number (string-to-number vm-version))
	(incoming-version-number -1.0))
     (save-excursion
       (goto-char (point-min))
       (if (re-search-forward "^X-Mailer: VM \\(.*\\) under Emacs" nil t)
	   (progn
	     (setq incoming-version-number (string-to-number (match-string 1)))
	     (if (> incoming-version-number vm-version-number)
		 (let
		     ((msg (format "Message has newer VM version %.2f (you have %.2f)."
				   incoming-version-number vm-version-number)))
		   (if (equal window-system 'x)
		       (x-show-tip msg nil nil 1)
		     (message msg))))))))))


;; (add-hook 'vm-select-new-message-hook 'show-new-vm-version-available t)
;; (add-hook 'vm-select-message-hook 'show-new-vm-version-available t)

;; (setq vm-select-new-message-hook nil)
;; (setq vm-select-message-hook nil)

(defun my-set-reply-to-OLD ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless
	(and
	 (search-forward-regexp "^From: ")
	 (cond
	  ((search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*terry\\.jones@upf.edu" (point-max) t)
	   ;;(message "reply will go to upf")
	   (email-from-upf))

	  ((search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*terry\\.jones@gmail.com" (point-max) t)
	   ;;(message "reply will go to gmail")
	   (email-from-gmail))

	  ((search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*terry@jon\\.es" (point-max) t)
	   ;;(message "reply will go to jon.es")
	   (email-from-jon.es))

	  ((search-forward-regexp "cam.ac.uk" (point-max) t)
	   ;;(message "reply will go to zoo")
	   (email-from-zoo))

	  ((or
	    (search-forward-regexp "@jones.tc" (point-max) t)
	    (search-forward-regexp "@eatoni.com" (point-max) t)
	    (search-forward-regexp "@santafe.edu" (point-max) t))
	   ;;(message "reply will go to jones.tc")
	   ;;(email-from-tc.jones)
	   (email-from-jon.es)
	   )))

      ;; (message "reply to UNCHANGED")
      )))

(defun my-set-reply-to ()
  (interactive)
  (save-excursion
    (cond
     ((progn
	(goto-char (point-min))
	(search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*terry\\.jones@upf\\.edu" (point-max) t))
      (message "reply-to = UPF")
      (email-from-upf))
     ((progn
        (or
         (and
          (goto-char (point-min))
          (search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*@fluidinfo\\.com" (point-max) t))
         (and
          (goto-char (point-min))
          (search-forward-regexp "^X-Launchpad-Branch: ~fluiddb-dev" (point-max) t))
         (and
          (goto-char (point-min))
          (search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*@oodl\\.es" (point-max) t))
         (and
          (goto-char (point-min))
          (search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*fluid.*@googlegroups\\.com" (point-max) t))))
      (message "reply-to = fluidinfo")
      (email-from-fluidinfo))
     ((progn
	(goto-char (point-min))
	(search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*terry\\.jones@gmail\\.com" (point-max) t))
      (message "reply-to = gmail")
      (email-from-gmail))
     ((progn
	(goto-char (point-min))
	(search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*terry@jon\\.es" (point-max) t))
      (message "reply-to = jon.es")
      (email-from-jon.es))
     ((progn
	(goto-char (point-min))
	(search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*tcj25@cam.ac.uk" (point-max) t))
      (message "reply-to = cam")
      ;; (email-from-zoo))
      (email-from-jon.es))
     ((progn
	(goto-char (point-min))
	(search-forward-regexp "terry\\.jones@upf.edu" (point-max) t))
      (message "reply-to = UPF [relaxed search]")
      (email-from-upf))
     ((progn
	(goto-char (point-min))
	(search-forward-regexp "terry\\.jones@gmail.com" (point-max) t))
      (message "reply-to = gmail [relaxed search]")
      (email-from-gmail))
     ((or
       (progn       
	 (goto-char (point-min))
	 (search-forward-regexp "^\\(?:To\\|X-Original-To\\|Cc\\|Bcc\\|From\\): .*@antigenic-cartography.org" (point-max) t))
       (progn
	 (goto-char (point-min))
	 (search-forward-regexp "cam.ac.uk" (point-max) t)))
      ;; (message "reply-to = cam [relaxed search]")
      ;; (email-from-zoo)
      (message "reply-to = jon.es [relaxed search]")
      (email-from-jon.es)
      )

     (t
      (message "reply-to = jon.es [default case]")
      (email-from-jon.es)))))
	   
	  

;; (remove-hook 'vm-select-message-hook 'my-set-reply-to)
(add-hook 'vm-select-message-hook 'my-set-reply-to t)
;;(x-show-tip "reply will go to jones.tc" nil nil 1)


(defun my-vm-kill-subject (&optional arg)
  "Refuse to kill messages with an empty subject."
  (interactive "p")
  (and
   (boundp 'vm-message-pointer)
   (let ((subject (vm-so-sortable-subject (car vm-message-pointer))))
	 (if (string-match "^[ 	]*$" subject)
		 (message "Will not kill messages with empty subject. Use M-x vm-kill-subject manually.")
	   ;;(message "calling kill.....")
	   (vm-kill-subject arg)
	   ))))

(define-key vm-mode-map "k" 'my-vm-kill-subject)


;;; SpamBayes

(defvar spambayes-filter-program "/usr/bin/sb_filter.py" "Path to the sb_filter program.")


(define-key vm-mode-map "ls" 'spambayes-vm-retrain-as-spam)
(define-key vm-summary-mode-map "ls" 'spambayes-vm-retrain-as-spam)
(define-key vm-mode-map "lh" 'spambayes-vm-retrain-as-ham)
(define-key vm-summary-mode-map "lh" 'spambayes-vm-retrain-as-ham)

(defun spambayes-vm-retrain (is-spam)
  "Retrain on all processable articles, or the one under the cursor.

is-spam is a boolean--true if you want to retrain the message as spam,
false if you want to retrain as ham.
"
  (interactive)
  (message (concat "Retraining" (if is-spam " as SPAM" " as HAM") " ..."))
  (vm-pipe-message-to-command
   (concat spambayes-filter-program (if is-spam " -s" " -g") " -f") nil)
  (message (concat "Done retraining messages"
                   (if is-spam " as SPAM" " as HAM") ".")))

(defun spambayes-vm-retrain-as-spam ()
  "Retrain and refilter messages as spam"
  (interactive)
  (spambayes-vm-retrain t))

(defun spambayes-vm-retrain-as-ham ()
  "Retrain and refilter messages as ham"
  (interactive)
  (spambayes-vm-retrain nil))

(provide 'my-vm)
