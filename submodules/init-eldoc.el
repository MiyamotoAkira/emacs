;;; package --- Summary
;;; Commentary:
;;; Code:
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(diminish 'eldoc-mode)

(provide 'init-eldoc)
;;; init-eldoc.el ends here
