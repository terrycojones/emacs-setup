(require 'starttls)

(defconst my-no-smtp-server "none" "Special name to indicate that no remote STMP server should be used. Instead, emacs will use sendmail-send-it to connect to the smtpd on the local machine")


(defun my-server-match (server list)
  (cond
   ((null list) nil)
   ((string= server (caar list)) (car list))
   (t (my-server-match server (cdr list)))))


(defun set-smtpmail-server (&optional server-arg)
  (interactive)
  (let
      ((my-smtp-servers
	`(("cambridge" "smtp.hermes.cam.ac.uk" 587 "tcj25" "2soft2look")
	  ("derek" "mail.btinternet.com" 587 "derek.j.smith@hg1.btinternet.com" "tym0thyt")
	  ("upf" "mx.upf.edu" 25 "terry.jones" "rst3008")
	  ("sfi" "smtp.santafe.edu" 25 "terry" "2soft2look")
	  ("eresmas" "smtp.eresmas.net" 25 "ret005vh@accesopremium" "12894183")))
       default server match server-list)
	  
    (if (not (null (my-server-match my-no-smtp-server my-smtp-servers)))
	(message (format "Your my-smtp-servers variable contains the reserved no-server name '%s'." my-no-smtp-server))
      (setq default (caar my-smtp-servers)
	    server-list (mapcar 'car my-smtp-servers)
	    server (or server-arg
		       (completing-read "SMTP server: " server-list nil t default nil my-no-smtp-server nil))
	    match (my-server-match server my-smtp-servers))
      (cond
       ((equal server my-no-smtp-server)
	(setq send-mail-function 'sendmail-send-it)
	(message "No remote SMTP server will be used. Using direct sendmail-send-it instead."))
	 
       ((null match)
	(message (concat "No such SMTP server '" server "'.")))
	 
       (t
	(setq send-mail-function 'smtpmail-send-it
	      smtpmail-smtp-server (nth 1 match)
	      smtpmail-smtp-service (nth 2 match)
	      smtpmail-auth-credentials (mapcar 'cdr my-smtp-servers)
	      smtpmail-starttls-credentials (mapcar '(lambda (a) (list (nth 1 a) (nth 2 a))) my-smtp-servers)
	      smtpmail-debug-info t
	      smtpmail-debug-verb t)
	(message (format "SMTP server set to '%s' port %d" smtpmail-smtp-server smtpmail-smtp-service)))))))

(provide 'my-smtpmail)
