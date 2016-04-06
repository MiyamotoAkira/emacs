
(setq omnisharp-server-executable-path "~/code/playground/omnisharp-roslyn/artifacts/publish/OmniSharp/osx.10.11-x64/dnxcore50/omnisharp")

(require 'omnisharp)

(add-hook 'csharp-mode-hook
        (lambda ()
          (setq-local company-backends '((company-omnisharp)))))

(add-hook 'csharp-mode-hook 'omnisharp-mode)

(define-key omnisharp-mode-map (kbd "C-c o u") 'omnisharp-fix-usings)
(define-key omnisharp-mode-map (kbd "C-c o s") 'omnisharp-start-omnisharp-server)
(define-key omnisharp-mode-map (kbd "C-c o b") 'omnisharp-build-in-emacs)
(define-key omnisharp-mode-map (kbd "C-c o t a") 'omnisharp-unit-test-all)
(define-key omnisharp-mode-map (kbd "C-c o t s") 'omnisharp-unit-test-single)
(define-key omnisharp-mode-map (kbd "C-c o t f") 'omnisharp-unit-test-fixture)
(define-key omnisharp-mode-map (kbd "C-c o t w") 'omnisharp-unit-test-worker)
(define-key omnisharp-mode-map (kbd "C-c o r r") 'omnisharp-rename)
(define-key omnisharp-mode-map (kbd "C-c o r i") 'omnisharp-rename-interactively)
(define-key omnisharp-mode-map (kbd "C-c o f u") 'omnisharp-find-usages)
(define-key omnisharp-mode-map (kbd "C-c o f i") 'omnisharp-find-implementations)


(provide 'init-csharp)
