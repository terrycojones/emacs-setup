
(autoload 'file-cache-minibuffer-complete "filecache" nil t)
  
(eval-after-load "filecache"
  '(progn
     (message "Loading file cache...")
     ;;(file-cache-add-directory-using-find "~/projects")
     ;;(file-cache-add-directory-list load-path)
     (file-cache-add-directory "~/")
     ;;(file-cache-add-file-list (list "~/foo/bar" "~/baz/bar"))
     ))


(provide 'my-filecache)
