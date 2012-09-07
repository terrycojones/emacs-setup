;;(defun my-gnus-kill-mode-string ()
;;  (kill-local-variable 'global-mode-string))

;;(add-hook 'gnus-group-mode-hook     'my-gnus-kill-mode-string)
;;(add-hook 'gnus-article-mode-hook   'my-gnus-kill-mode-string)

;; Things to add mailcrypt to GNUS
;; (add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
;; (add-hook 'news-reply-mode-hook 'mc-install-write-mode)

;; Tiny Mime stuff
;; (add-hook 'news-reply-mode-hook 'mime/editor-mode)
;; (add-hook 'news-inews-hook 'mime-editor/maybe-translate)

;; (setq tm-gnus/automatic-mime-preview          nil

(setq
 gnus-show-mime                          nil
 gnus-local-domain                       "jones.tc"
 gnus-select-method                      '(nntp "fw.intellimedia.com")
 ;; gnus-check-new-newsgroups               'ask-server
 gnus-check-new-newsgroups               nil
 gnus-read-active-file                   'some
 gnus-subscribe-newsgroup-method         'gnus-subscribe-hierarchically
 gnus-save-newsrc-file                   nil
 gnus-save-killed-list                   nil
 gnus-group-sort-function                'gnus-group-sort-by-rank
 gnus-subscribe-hierarchical-interactive t
 gnus-show-threads                       t
 gnus-fetch-old-headers                  nil
 gnus-thread-sort-functions              '(gnus-thread-sort-by-number
					   gnus-thread-sort-by-subject
					   gnus-thread-sort-by-score)
 ;; gnus-asynchronous                       t
 ;; nntp-async-number                       3
 gnus-use-adaptive-scoring               t
 ;; gnus-use-nocem                          t
 gnus-large-newsgroup                    500
 gnus-use-long-file-name                 t
 gnus-article-save-directory             (expand-file-name "~/things")
 gnus-use-cross-reference                t
 gnus-default-article-saver              'gnus-summary-save-in-mail
 gnus-auto-center-summary                t)

(defvar gnus-default-adaptive-score-alist
       '((gnus-unread-mark)
         (gnus-ticked-mark (from 4))
         (gnus-dormant-mark (from 5))
         (gnus-del-mark (from -4) (subject -1))
         (gnus-read-mark (from 4) (subject 2))
         (gnus-expirable-mark (from -1) (subject -1))
         (gnus-killed-mark (from -1) (subject -3))
         (gnus-kill-file-mark)
         (gnus-ancient-mark)
         (gnus-low-score-mark)
         (gnus-catchup-mark (from -1) (subject -1))))

(add-hook 'news-reply-header-hook   '(lambda () (gin-mode t)))
(add-hook 'gnus-summary-exit-hook   'gnus-summary-bubble-group)

;; (if running-x
;;     (add-hook 
;;      'gnus-started-hook
;;      '(lambda ()
;; 	(set-face-foreground 'gnus-group-mail-low-empty-face  "orange")
;; 	(set-face-foreground 'gnus-group-mail-low-face        "orange")
;; 	(set-face-foreground 'gnus-header-content-face        "orange")
;; 	(set-face-foreground 'gnus-header-name-face           "orange"))))

(provide 'my-gnus)
