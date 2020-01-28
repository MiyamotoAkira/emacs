;;; package --- Summary
;;;Languages setup

;;; Commentary:
;;; Here we are setting up all the neccessary elements for
;;; the support of several languages.

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package auto-yasnippet
  :diminish yas-minor-mode
  :ensure t
  :bind (("C-c C-y C-c" . aya-create)
         ("C-c C-y C-e" . aya-expand)))

(use-package flycheck
  :ensure t
  :config
  (show-paren-mode 1)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package tagedit
  :ensure t)

(use-package editorconfig
  :diminish
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package highlight-indentation
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package company
  :ensure t
  :diminish
  :bind (("C-S-i" . company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package buttercup
  :ensure t)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))

(use-package web-mode
  :defer t
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
  :config
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package json-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :defer t
  :ensure t
  :mode "\\.Dockerfile\\'")

(use-package gradle-mode
  :defer t
  :ensure t)

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package puppet-mode
  :defer t
  :ensure t)

(use-package terraform-mode
  :defer t
  :ensure t)

(use-package company-terraform
  :ensure t
  :config
  (company-terraform-init))

(use-package scala-mode
  :defer t
  :ensure t)

(use-package robe
  :defer t
  :ensure t
  :hook
  ((ruby-mode . robe-mode))
  :config
  (push 'company-robe company-backend))

(use-package lfe-mode
  :defer t
  :ensure t)

(use-package erlang
  :defer t
  :ensure t)

(use-package elm-mode
  :defer t
  :ensure t)

(use-package rust-mode
  :defer t
  :ensure t)

(use-package cargo
  :defer t
  :ensure t
  :hook
  ((rust-mode . cargo-minor-mode)))

(use-package flycheck-rust
  :defer t
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :defer t
  :ensure t
  :config
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "~/code/externals/rust/src")
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :config
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc")))

(use-package elixir-mode
  :defer t
  :ensure t
  :config
  (add-hook 'elixir-mode-hook 'my-pretty-lambda-elixir)
  (add-hook 'elixir-mode-hook (lambda ()
                                (setq tab-width 2)
                                (setq indent-tabs-mode nil))))

(use-package alchemist
  :defer t
  :ensure t
  :init
  (add-hook 'alchemist-mode-hook 'company-mode)
  :hook
  ((elixir-mode . alchemist-mode)))

(defun clj-clojure-setup ()
  "Functionality to be added for Clojure."
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package cider
  :defer t
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode))

(use-package clojure-mode
  :defer t
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq inferior-lisp-program "lein repl")
              (font-lock-add-keywords
               nil
               '(("(\\(facts?\\)"
                  (1 font-lock-keyword-face))
                 ("(\\(background?\\)"
                  (1 font-lock-keyword-face))))
              (define-clojure-indent (fact 1))
              (define-clojure-indent (facts 1))))
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook 'my-pretty-lambda-clojure))

;; (use-package midje-mode
;;   :defer t
;;   :ensure t
;;   :pin melpa-stable
;;   :config
;;   (add-hook 'clojure-mode-hook 'midje-mode))

;; (use-package clojure-jump-to-file
;;   :defer t
;;   :ensure t)

(use-package clj-refactor
  :defer t
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'clojure-mode-hook #'clj-clojure-setup))

(use-package clojure-mode-extra-font-locking
  :defer t
  :ensure t
  :pin melpa-stable)

(use-package java-imports
  :ensure t
  :defer t
  :config
  (add-hook 'java-mode-hook 'java-imports-scan-file))

(use-package lsp-mode
  :defer t
  :defines lsp-highlight-symbol-at-point
  :ensure t
  :init (setq lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil))

(use-package lsp-ui
  :defer t
  :ensure t
  :config
  (setq lsp-ui-sideline-update-mode 'point))

(use-package company-lsp
  :after company
  :ensure t
  :config
  (add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))
  (setq company-lsp-cache-candidates t)
  (push 'company-lsp company-backend))


(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java
  :after 'lsp-java)

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook  'lsp-java-enable)
  (add-hook 'java-mode-hook  'flycheck-mode)
  (add-hook 'java-mode-hook  'company-mode)
  (add-hook 'java-mode-hook  (lambda () (lsp-ui-flycheck-enable t)))
  (add-hook 'java-mode-hook  'lsp-ui-mode))

(use-package fsharp-mode
  :defer t
  :ensure t
  ;; :config 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode)
  )

(use-package haskell-mode
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package groovy-mode
  :defer t
  :ensure t)

(use-package slime-company
  :ensure t
  :config
  (setq slime-company-major-modes (quote (lisp-mode slime-repl-mode scheme-mode))))

(use-package slime
  :defer t
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  (slime-setup '(slime-fancy slime-company)))

(use-package slime-company
  :ensure t
  :config
  (setq slime-company-major-modes (quote (lisp-mode slime-repl-mode scheme-mode))))

;; This one has to happen after all modes that use parens are loaded
(use-package paredit
  :ensure t
  :diminish
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'lfe-mode-hook #'enable-paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'java-mode-hook #'rainbow-delimiters-mode))

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(use-package company-auctex
  :defer t
  :ensure t)

(use-package latex-preview-pane
  :ensure t
  :defer t
  :config
  (latex-preview-pane-enable))

(use-package geiser
  :defer t
  :ensure t)

(use-package lua-mode
  :defer t
  :ensure t)

(use-package company-lua
  :defer t
  :ensure t)

(use-package luarocks
  :defer t
  :ensure t)

(use-package purescript-mode
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package psc-ide
  :defer t
  :ensure t
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode))))

(use-package csharp-mode
  :defer t
  :ensure t)

(use-package omnisharp
  :defer t
  :ensure t
  :bind (
         ("C-c o s s" . omnisharp-start-omnisharp-server)
         :map omnisharp-mode-map
         ("C-c o s p" . omnisharp-stop-server)
         ("C-c o a" . omnisharp-auto-complete)
         ("C-c o c r" . recompile)
         ("C-c o i t" . omnisharp-current-type-information)
         ("C-c o i d" . omnisharp-current-type-documentation)
         ("C-c o i s" . omnisharp-show-overloads-at-point)
         ("C-c o g d" . omnisharp-go-to-definition)
         ("C-c o g w" . omnisharp-go-to-definition-other-window)
         ("C-c o g u" . omnisharp-find-usages)
         ("C-c o g i" . omnisharp-find-implementations)
         ("C-c o g s" . omnisharp-navigate-to-solution-member)
         ("C-c o g m" . omnisharp-navigate-to-current-file-member)
         ("C-c o g f" . omnisharp-navigate-to-solution-file-then-file-member)
         ("C-c o k d" . omnisharp-code-format-entire-file)
         ("C-c o r r" . omnisharp-rename)
         ("C-c o r f" . omnisharp-fix-usings)
         ("C-c o r c" . omnisharp-fix-code-issue-at-point)
         ("C-c o r a" . omnisharp-run-code-action-refactoring))
  :config
  (push 'company-omnisharp company-backends)
  (add-hook 'csharp-mode-hook #'omnisharp-mode))

(use-package powershell
  :defer t
  :ensure t)

(use-package kotlin-mode
  :defer t
  :ensure t)

(use-package graphviz-dot-mode
  :defer t
  :ensure t)

(use-package tide
  :defer t
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :bind (("C-c C-t C-f" . tide-fix))
  :config
  (add-hook 'typescript-mode-hook (lambda ()
                                    (setq typescript-indent-level 2)))
  (setq tide-format-options '(:indentSize 2 :insertSpaceBeforeFunctionParenthesis t :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :insertSpaceAfterConstructor t)))

(use-package prettier-js
  :defer t
  :ensure t
  :diminish
  :hook ((typescript-mode . prettier-js-mode)))

(use-package tuareg
  :defer t
  :ensure t)

(provide 'init-languages)
;;; init-languages.el ends here

