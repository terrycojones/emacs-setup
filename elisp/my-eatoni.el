(defun eatoni-url ()
  (interactive)
    (insert-before-last-white-maybe "http://www.eatoni.com")
    (if (not (looking-at " \\|\t\\|\n")) (insert "/")))

(defun eatoni-ling ()
  (interactive)
    (insert-before-last-white-maybe "http://www.eatoni.com/docs/lingarg.html"))

(defun eatoni-demo ()
  (interactive)
    (insert-before-last-white-maybe "http://www.eatoni.com/windemo"))

(defun eatoni-press ()
  (interactive)
    (insert-before-last-white-maybe "http://www.eatoni.com/press"))

(defun eatoni-pricing ()
  (interactive)
    (insert-before-last-white-maybe "http://www.eatoni.com/letterwise/pricing"))

(defun eatoni-ftp ()
  (interactive)
    (insert-before-last-white-maybe "ftp://ftp.eatoni.com/pub")
    (if (not (looking-at " \\|\t\\|\n")) (insert "/")))

(defun sign-eatoni ()
  (interactive)
  (insert "Terry Jones
Eatoni Ergonomics Inc.
42 W. 24th Street
New York, NY 10010")

  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun sign-eatoni-verbose ()
  (interactive)
  (insert "Terry Jones                                E-mail: terry@eatoni.com
Eatoni Ergonomics Inc.                     Tel: (917) 325-7581
42 W. 24th Street                          Fax: (212) 337-1984
New York, NY 10010                         URL: http://www.eatoni.com")

  (if (looking-at "\n")
      (forward-char 1)
      (insert "\n")))

(defun email-from-eatoni ()
  (interactive)
  (and
   (fboundp 'vm-version)
   (setq vm-mail-header-from "Terry Jones <terry@eatoni.com>"))
  (setq
    mail-default-reply-to            "terry@eatoni.com"
    user-mail-address                "terry@eatoni.com"
    mail-from-style                  'system-default
    mail-envelope-from               "Terry Jones <terry@eatoni.com>"))

(provide 'my-eatoni)
