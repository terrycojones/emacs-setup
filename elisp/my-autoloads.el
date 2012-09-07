(and
 (boundp 'i-use-franz)
 i-use-franz
 (autoload 'run-allegro             "my-franz"        nil t))

(and
 (boundp 'i-use-js2-mode)
 i-use-js2-mode
 (autoload 'js2-mode                          "js2-mode"             nil t))

(autoload 'pydb                          "pydb"             nil t)
(autoload 'email-from-eatoni             "my-eatoni"        nil t)

(autoload 'alter-c-mode-bindings         "my-c"             nil t)
(autoload 'c-printf-line                 "my-c"             nil t)
(autoload 'c-fprintf-line                "my-c"             nil t)
(autoload 'c-comment-line                "my-c"             nil t)
(autoload 'c-uncomment-line              "my-c"             nil t)
(autoload 'c-func-to-extern              "my-c"             nil t)
(autoload 'c-surround-line-dashed        "new-surround"     nil t)
(autoload 'c-re-surround-line-dashed     "new-surround"     nil t)
(autoload 'c-dash-line                   "new-surround"     nil t)

(autoload 'ruby-mode                     "ruby-mode"        "Major mode for editing ruby scripts." t)
(autoload 'run-ruby                      "inf-ruby"         "Run an inferior Ruby process")
(autoload 'inf-ruby-keys                 "inf-ruby"         "Set local key defs for inf-ruby in ruby-mode")

(autoload 'alter-java-mode-bindings      "my-java"          nil t)
(autoload 'java-printf-line              "my-java"          nil t)
(autoload 'java-fprintf-line             "my-java"          nil t)
(autoload 'java-comment-line             "my-java"          nil t)
(autoload 'java-uncomment-line           "my-java"          nil t)
(autoload 'java-re-surround-line-dashed  "new-surround"     nil t)

(autoload 'python-surround-line-dashed        "new-surround"     nil t)
(autoload 'python-re-surround-line-dashed     "new-surround"     nil t)
(autoload 'general-surround-line         "new-surround"     nil t)


(autoload 'eval-line                     "my-emacs-lisp"    nil t)

(autoload 'bm-line                       "my-bookmarks"     nil t)
(autoload 'unbm-line                     "my-bookmarks"     nil t)

(and
 (boundp 'i-use-w3m)
 i-use-w3m
 (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
 (autoload 'w3m-find-file "w3m" "Find a local file using emacs-w3m." t)
 (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 (autoload 'w3m-search "w3m-search" "Search words using emacs-w3m." t)
 (autoload 'w3m-weather "w3m-weather" "Display a weather report." t)
 (autoload 'w3m-antenna "w3m-antenna" "Report changes of web sites." t)
 (autoload 'w3m-namazu "w3m-namazu" "Search files with Namazu." t))

(and
 (boundp 'i-use-dired)
 i-use-dired
 (autoload 'dired-x-find-file         "dired"            "Dired" t)
 (autoload 'my-fast-dired-x-find-file "my-dired"         "Dired" t)
 (autoload 'dired-filename-at-point   "dired-x"          "Dired" t)
 (autoload 'my-dired-mouse-find-file  "my-dired"         "Dired" t))

(autoload 'my-gnus-kill-mode-string      "my-gnus"          nil t)
(autoload 'my-latex-hook                 "my-latex"         nil t)
(autoload 'html-helper-mode              "html-helper-mode" "Yay HTML" t)
(autoload 'gin-mode                      "gin-mode"         "Gin filling mode." t)
(autoload 'electric-buffer-list          "ebuff-menu"       "Electric buffer list." t)
(autoload 'vip-change-state-to-vi        "viper"            "VIPER mode." t)
(autoload 'surround-line                 "new-surround"     "Surround a line." t)
(autoload 'un-surround-line              "new-surround"     "Un-surround a line." t)
(autoload 're-surround-line              "new-surround"     "Re-surround a line." t)
(autoload 'list-load-path-shadows        "my-shadow"        "Show load path problems." t)

(autoload 'm4-mode                       "m4-mode"          nil t)

(autoload 'todo-mode "todo-mode" "Major mode for editing TODO lists." t)
(autoload 'todo-show "todo-mode" "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode" "Add TODO item." t)


;; (autoload 'cycle-buffer                  "cycle-buffer-new" "Cycle forward." t)
;; (autoload 'cycle-buffer-backward         "cycle-buffer-new" "Cycle backward." t)

(autoload 'gid                            "gid"              nil t)
;; (autoload 'align-cols                     "align"            nil t)
(autoload 'align                     "align"            nil t)

(autoload 'mc-install-write-mode "mailcrypt" nil t)
(autoload 'mc-install-read-mode "mailcrypt" nil t)
(add-hook 'mail-mode-hook 'mc-install-write-mode)

(and
 (boundp 'i-use-w3)
 i-use-w3
 (progn
   ;; w3 autoloads.
   (autoload 'w3-preview-this-buffer "w3"     "WWW Previewer"                     t)
   (autoload 'w3-follow-url-at-point "w3"     "Find document at pt"               t)
   (autoload 'w3                     "w3"     "WWW Browser"                       t)
   (autoload 'w3-open-local          "w3"     "Open local file for WWW browsing"  t)
   (autoload 'w3-fetch               "w3"     "Open remote file for WWW browsing" t)
   (autoload 'w3-use-hotlist         "w3"     "Use shortcuts to view WWW docs"    t)
   (autoload 'w3-show-hotlist        "w3"     "Use shortcuts to view WWW docs"    t)
   (autoload 'w3-follow-link         "w3"     "Follow a hypertext link."          t)
   (autoload 'w3-batch-fetch         "w3"     "Batch retrieval of URLs"           t)
   (autoload 'url-get-url-at-point   "url"    "Find the url under the cursor"     nil)
   (autoload 'url-file-attributes    "url"    "File attributes of a URL"          nil)
   (autoload 'url-popup-info         "url"    "Get info on a URL"                 t)
   (autoload 'url-retrieve           "url"    "Retrieve a URL"                    nil)
   (autoload 'url-buffer-visiting    "url"    "Find buffer visiting a URL."       nil)
   (autoload 'gopher-dispatch-object "gopher" "Fetch gopher dir"                  t)))

(if (and (boundp 'i-use-semi) i-use-semi)
    (autoload 'turn-on-mime-edit "mime-edit"
      "Minor mode for editing MIME message." t))

(if (and (boundp 'i-use-hideshow) i-use-hideshow)
    (autoload 'hs-minor-mode "hideshow"
      "Selective code display" t))

(if (and (boundp 'i-use-speedbar) i-use-speedbar)
    (progn
      (autoload 'Info-speedbar-buttons "sb-info"
	"Info specific speedbar button generator.")
      (autoload 'gud-speedbar-buttons "sb-gud"
	"GUD specific speedbar button generator.")))

(if (and (boundp 'i-use-oe) i-use-oe)
    (autoload 'oe-send-current-file "my-oe"
      "Send file names to the Object Exchange." t))

(if (and (boundp 'i-use-flyspell) i-use-flyspell)
    (autoload 'flyspell-mode-on "flyspell"
      "On the fly spelling alerts." t))

(provide 'my-autoloads)
