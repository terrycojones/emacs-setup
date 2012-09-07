(defun hilit-rehighlight-page ()
  (interactive)
  (save-excursion
    (move-to-window-line 0)
    (beginning-of-line)
    (let ((beg (point)))
      (move-to-window-line -1)
      (end-of-line)
      (hilit-rehighlight-region beg (point) t))))

(setq hilit-auto-rehighlight 'visible
      hilit-quietly t
      hilit-mode-enable-list '(not text-mode)
      hilit-background-mode 'dark
      )

(hilit-set-mode-patterns
 'tcl-mode
 '(("\\s #.*$" nil comment)
   ("^#.*$" nil comment)
   ("[^\\]\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ;; ORIG ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ;; ("\\$[-_a-zA-Z]+" nil varref)
   ("\\$[-_\.a-zA-Z0-9]+" nil label)
   ("^source.*$" nil include)
   ("\\b\\(global\\|upvar\\)\\b" nil decl)
   ("\\b\\(error\\|debug\\)\\b" nil decl)
   ("^\\s *proc\\s +\\(\\w\\|[_']\\)+" nil defun)
   ("\\b\\(set\\|lset\\|list\\|if\\|case\\|while\\|switch\\|then\\|else\\|for\\|foreach\\|return\\|expr\\|catch\\)\\b" nil keyword)))

(hilit-translate
 comment                 'yellow/cornflowerblue
 include                 'white/royalblue3
 define                  'white/slateblue1
 defun                   'darkslateblue/yellow
 decl                    'black/skyblue
 type                    nil
 keyword                 'white/cyan3
 label                   'white/blue
 string                  'white/lightskyblue3
 crossref                'black/lightblue2
 summary-seen            'yellow/cornflowerblue
 summary-unread          'white/lightskyblue3
 summary-new             'white/cornflowerblue
 msg-subject             'darkslateblue/yellow
 msg-from                'darkslateblue/yellow
 msg-header              'yellow/cornflowerblue
 msg-quote               'yellow/cornflowerblue
 msg-separator           'white/royalblue3
 gnus-group-unsubscribed 'white/black
 gnus-group-empty        'white/royalblue3
 gnus-group-full         'white/cyan3
 gnus-group-overflowing  'yellow/cornflowerblue
 jargon-entry            'white/cyan3
 jargon-xref             'white
 jargon-keyword          'darkslateblue/yellow
 formula                 'darkslateblue/yellow)

(provide 'my-hilit)
