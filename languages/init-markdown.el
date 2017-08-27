;;; package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(custom-set-variables
 '(markdown-command "/usr/bin/pandoc"))
(provide 'init-markdown)
;;; init-markdown.el ends here
