
(defun set-bitstream-font ()
  (interactive)
  (set-face-font 'default "-apple-bitstream vera sans mono-medium-r-normal--0-0-75-75-m-0-mac-roman")
  (set-face-font 'bold "-apple-bitstream vera sans mono-bold-r-normal--0-0-75-75-m-0-mac-roman")
  (set-face-attribute 'default nil :height 135)
  (set-face-attribute 'default nil :height 135)
  )

(provide 'my-font)
