;;; package --- Summary
; Setting up rainbow delimiters

;;; Commentary:

;;; Code:
(require 'rainbow-delimiters)

;;; Add color customization here


(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(provide 'init-rainbow)
;;; init-rainbow.el ends here
