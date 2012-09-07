(require 'my-misc)

(autoload 'tag-continue "tags" "Tag stack things" t)
(autoload 'my-find-tag  "tags" "Tag stack things" t)
(autoload 'tag-pop      "tags" "Tag stack things" t)

(add-hook 'find-tag-hook 'my-find-tag-hook)
  
(define-keys
  global-map
  (list "\M-[" 'tag-continue
	"\M-." 'my-find-tag
	"\M-'" 'tag-pop))

(provide 'my-tags)
