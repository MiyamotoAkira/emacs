;;; package --- Summary
;;CSharp modifications

;;; Commentary:

;;; Code:
(require 'omnisharp)
(setq omnisharp-server-executable-path "~/code/omnisharp/OmniSharp")
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;; Keybindings
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o s s") 'omnisharp-start-omnisharp-server)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o s p") 'omnisharp-stop-server)))
;;(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o }") 'jump-to-enclosing-func)))
;;(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o .") 'add-dot-and-auto-complete)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o a") 'omnisharp-auto-complete)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o g d") 'omnisharp-go-to-definition)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o g w") 'omnisharp-go-to-definition-other-window)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o u") 'omnisharp-find-usages)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o i") 'omnisharp-find-implementations)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o k d") 'omnisharp-code-format-entire-file)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o r r") 'omnisharp-rename)))

(provide 'init-csharp)
;;; init-csharp.el ends here
