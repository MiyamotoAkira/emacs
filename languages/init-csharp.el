
(setq omnisharp-server-executable-path "~/code/playground/omnisharp-roslyn/artifacts/publish/OmniSharp/osx.10.11-x64/dnxcore50/omnisharp")

(require 'omnisharp)

(add-hook 'csharp-mode-hook
        (lambda ()
          (setq-local company-backends '((company-omnisharp)))))

(add-hook 'csharp-mode-hook 'omnisharp-mode)

(provide 'init-csharp)
