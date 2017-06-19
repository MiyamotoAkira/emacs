;;; package --- Summary
;;CSharp modifications

;;; Commentary:

;;; Code:
(require 'omnisharp)
(setq omnisharp-server-executable-path "~/code/omnisharp/OmniSharp")
(add-hook 'csharp-mode-hook 'omnisharp-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

;; Keybindings
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o s s") 'omnisharp-start-omnisharp-server)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o s p") 'omnisharp-stop-server)))
;;(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o }") 'jump-to-enclosing-func)))
;;(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o .") 'add-dot-and-auto-complete)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o a") 'omnisharp-auto-complete)))

(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o c r") 'recompile)))

(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o i t") 'omnisharp-current-type-information)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o i d") 'omnisharp-current-type-documentation)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o i s") 'omnisharp-show-overloads-at-point)))

;;(add-hook 'omnisharp-mode-hook (lambda ()  (local-set-key (kbd "C-c o t s") (lambda () (interactive) (omnisharp-unit-test "single")))))
;;(add-hook 'omnisharp-mode-hook (lambda ()  (local-set-key (kbd "C-c o t f") (lambda () (interactive) (omnisharp-unit-test "fixture")))))
;;(add-hook 'omnisharp-mode-hook (lambda ()  (local-set-key (kbd "C-c o t a") (lambda () (interactive) (omnisharp-unit-test "all")))))

(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o g d") 'omnisharp-go-to-definition)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o g w") 'omnisharp-go-to-definition-other-window)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o g u") 'omnisharp-find-usages)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o g i") 'omnisharp-find-implementations)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o g s") 'omnisharp-navigate-to-solution-member)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o g m") 'omnisharp-navigate-to-current-file-member)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o g f") 'omnisharp-navigate-to-solution-file-then-file-member)))


(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o k d") 'omnisharp-code-format-entire-file)))

(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o r r") 'omnisharp-rename)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o r f") 'omnisharp-fix-usings)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o r c") 'omnisharp-fix-code-issue-at-point)))
(add-hook 'omnisharp-mode-hook (lambda () (local-set-key (kbd "C-c o r a") 'omnisharp-run-code-action-refactoring)))

(provide 'init-csharp)
;;; init-csharp.el ends here
