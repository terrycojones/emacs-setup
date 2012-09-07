;; (make-variable-buffer-local 'lazy-lock-defer-time)
;; (set-default 'lazy-lock-defer-time 1.0)

(setq

 font-lock-global-modes '(not shell-mode)
 ;; font-lock-support-mode 'lazy-lock-mode
 
 ;; lazy-lock-minimum-size nil
 ;; lazy-lock-stealth-time 30
 ;; lazy-lock-stealth-verbose

 ;; lazy-lock-defer-time 1
 ;; lazy-lock-defer-on-scrolling t
 
;;  font-lock-face-attributes
;; '(
   ;; (font-lock-comment-face       "pink")
   ;; (font-lock-string-face        "white" "lightskyblue4")
   ;; (font-lock-keyword-face       "white" "cyan3")
   ;; (font-lock-function-name-face "darkslateblue" "yellow")
   ;; (font-lock-variable-name-face "darkslateblue" "yellow")
   ;; (font-lock-type-face          "white" "cyan3")
   ;; (font-lock-reference-face     "white" "steelblue")
   ;; )
)

(provide 'my-font-lock)
