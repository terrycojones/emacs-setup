(defvar oe-host "localhost" "Object Exchange host to connect to.")
(defvar oe-port 8001 "Object Exchange port to connect to.")
(defvar oe-user (user-real-login-name) "User name.")
(defvar oe-process nil "The Object Exchange process.")

(defun oe-open ()
  (interactive)
  (setq oe-process (open-network-stream "oe" "*oe*" oe-host oe-port))
  (if (oe-alive-p)
      (progn
	(message "Connection to object exchange on %s:%d open." oe-host oe-port)
	t)
    (message "Could not connect to object exchange on %s:%d open." oe-host oe-port)
    nil))

(defun oe-alive-p ()
  (interactive)
  (eq (process-status "oe") 'open))

(defun oe-close ()
  (interactive)
  (if (oe-alive-p)
      (progn
	(delete-process "oe")
	(message "Object Exchange connection dropped."))
    (message "Object Exchange connection was already inactive.")))

(defun oe-send-file (file)
  (interactive "F")
  (if (or (oe-alive-p) (oe-open))
      (progn
	(process-send-string oe-process (format "application:s: emacs\nfile:s: %s\nuser: %s\nend\n" file oe-user))
	t)
    nil))

(defun oe-send-current-file ()
  (interactive)
  (and
   buffer-file-name
   (oe-send-file buffer-file-name)))

(defun oe-send-bookmark (bookmark)
  (interactive "s")
  (if (or (oe-alive-p) (oe-open))
      (progn
	(process-send-string oe-process (format "application: bookmarker\nURL: %s\nuser: %s\nend\n"
						bookmark oe-user))
	t)
    nil))

(provide 'my-oe)
