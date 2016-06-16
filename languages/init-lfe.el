;;; package --- Summary
										; lfe modifications
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "vendor/lfe" user-emacs-directory))

(require 'lfe-start)
(add-hook 'lfe-mode-hook #'enable-paredit-mode)
(provide 'init-lfe)
;;; init-lfe.el ends here
