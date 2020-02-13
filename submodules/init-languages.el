;;; package --- Summary
;;;Languages setup

;;; Commentary:
;;; Here we are setting up all the neccessary elements for
;;; the support of several languages.

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :defines tools-map
  :bind (:map yas-minor-mode-map
              ("n" . yas-new-snippet)
              ("s" . yas-insert-snippet)
              ("v" . yas-visit-snippet-file))
  :config
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)
  (define-prefix-command 'yas-minor-mode-map)
  (define-key tools-map (kbd "y") 'yas-minor-mode-map)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package auto-yasnippet
  :diminish yas-minor-mode
  :ensure t
  :bind (:map yas-minor-mode-map
              ("c" . aya-create)
              ("e" . aya-expand)))

(use-package flycheck
  :ensure t
  :config
  (show-paren-mode 1)
  :hook
  ((after-init . global-flycheck-mode)))

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
  :hook
  ((emacs-lisp-mode . aggressive-indent-mode)))

(use-package company
  :ensure t
  :diminish
  :bind (("C-S-i" . company-complete))
  :hook
  ((after-init . global-company-mode)))

(use-package buttercup
  :ensure t)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))

(use-package web-mode
  :defer t
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
  :hook
  ((web-mode . my-web-mode-hook)))

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
  :ensure t
  :config
  (terraform-format-on-save-mode t))

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
  :ensure t
  :defines languages-map
  :bind (:map rust-evil-map
              ("d" . rust-dbg-wrap-or-unwrap)
              ("f" . rust-format-buffer))
  :config
  (defvar rust-evil-map (make-sparse-keymap))
  (define-prefix-command 'rust-evil-map)
  (define-key languages-map (kbd "r") 'rust-evil-map))

(use-package cargo
  :defer t
  :ensure t
  :bind (:map cargo-evil-map
              ("a" . cargo-process-add)
              ("b" . cargo-process-build)
              ("c" . cargo-process-repeat)
              ("d" . cargo-process-doc)
              ("e" . cargo-process-bench)
              ("f" . cargo-process-current-test)
              ("TAB" . cargo-process-init)
              ("k" . cargo-process-check)
              ("l" . cargo-process-clean)
              ("RET" . cargo-process-fmt)
              ("n" . cargo-process-new)
              ("o" . cargo-process-current-file-tests)
              ("r" . cargo-process-run)
              ("s" . cargo-process-search)
              ("t" . cargo-process-test)
              ("u" . cargo-process-update)
              ("v" . cargo-process-doc-open)
              ("x" . cargo-process-run-example)
              ("C-a" . cargo-process-audit)
              ("C-d" . cargo-process-rm)
              ("C-k" . cargo-process-clippy)
              ("C-o" . cargo-process-outdated))
  :hook
  ((rust-mode . cargo-minor-mode))
  :config
  (defvar cargo-evil-map (make-sparse-keymap))
  (define-prefix-command 'cargo-evil-map)
  (define-key rust-evil-map (kbd "c") 'cargo-evil-map))

(use-package flycheck-rust
  :defer t
  :ensure t
  :hook
  ((flycheck-mode . flycheck-rust-setup)))

(use-package racer
  :defer t
  :ensure t
  :config
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "~/code/externals/rust/src")
  :hook
  ((rust-mode . racer-mode)
   (racer-mode . eldoc-mode)
   (racer-mode . company-mode)))

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
  :hook
  ((elixir-mode-hook . my-pretty-lambda-elixir)
   (elixir-mode-hook . (lambda ()
                         (setq tab-width 2)
                         (setq indent-tabs-mode nil)))))

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
  :hook
  ((cider-repl-mode . paredit-mode)
   (cider-mode . paredit-mode)
   (cider-mode . eldoc-mode)
   (cider-mode . company-mode)
   (cider-repl-mode . company-mode)))

(use-package clojure-mode
  :defer t
  :ensure t
  :pin melpa-stable
  :hook
  ((clojure-mode . subword-mode)
   (clojure-mode . aggressive-indent-mode)
   (clojure-mode . (lambda ()
                     (setq inferior-lisp-program "lein repl")
                     (font-lock-add-keywords
                      nil
                      '(("(\\(facts?\\)"
                         (1 font-lock-keyword-face))
                        ("(\\(background?\\)"
                         (1 font-lock-keyword-face))))
                     (define-clojure-indent (fact 1))
                     (define-clojure-indent (facts 1))))
   (clojure-mode . cider-mode)
   (clojure-mode . my-pretty-lambda-clojure)))

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
  :hook
  (clojure-mode . clj-clojure-setup))

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
  :hook
  ((java-mode . (lambda () (push 'company-lsp company-backends))))
  :config
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
  :hook
  ((java-mode . lsp-java-enable)
   (java-mode . flycheck-mode)
   (java-mode . company-mode)
   (java-mode . (lambda () (lsp-ui-flycheck-enable t)))
   (java-mode . lsp-ui-mode)))

(use-package fsharp-mode
  :defer t
  :ensure t)

(use-package haskell-mode
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  :hook
  ((haskell-mode . haskell-indentation-mode)))

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
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)
   (eval-expression-minibuffer-setup . enable-paredit-mode)
   (ielm-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (lisp-interaction-mode . enable-paredit-mode)
   (scheme-mode . enable-paredit-mode)
   (clojure-mode . enable-paredit-mode)
   (lfe-mode . enable-paredit-mode)))

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (eval-expression-minibuffer-setup . rainbow-delimiters-mode)
   (ielm-mode . rainbow-delimiters-mode)
   (lisp-mode . rainbow-delimiters-mode)
   (lisp-interaction-mode . rainbow-delimiters-mode)
   (scheme-mode . rainbow-delimiters-mode)
   (clojure-mode . rainbow-delimiters-mode)
   (java-mode . rainbow-delimiters-mode)))

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  :hook
  ((latex-mode . turn-on-reftex)
   (LaTeX-mode . turn-on-reftex)))

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
  :hook
  ((purescript-mode . turn-on-purescript-indentation)))

(use-package psc-ide
  :defer t
  :ensure t
  :hook
  (purescript-mode . (lambda ()
                       (psc-ide-mode)
                       (company-mode)
                       (flycheck-mode))))

(use-package csharp-mode
  :defer t
  :ensure t)

(use-package omnisharp
  :defer t
  :ensure t
  :defines languages-map
  :bind (
         ("C-c o s s" . omnisharp-start-omnisharp-server)
         :map omnisharp-mode-map
         ("s p" . omnisharp-stop-server)
         ("a" . omnisharp-auto-complete)
         ("c r" . recompile)
         ("i t" . omnisharp-current-type-information)
         ("i d" . omnisharp-current-type-documentation)
         ("i s" . omnisharp-show-overloads-at-point)
         ("g d" . omnisharp-go-to-definition)
         ("g w" . omnisharp-go-to-definition-other-window)
         ("g u" . omnisharp-find-usages)
         ("g i" . omnisharp-find-implementations)
         ("g s" . omnisharp-navigate-to-solution-member)
         ("g m" . omnisharp-navigate-to-current-file-member)
         ("g f" . omnisharp-navigate-to-solution-file-then-file-member)
         ("k d" . omnisharp-code-format-entire-file)
         ("r r" . omnisharp-rename)
         ("r f" . omnisharp-fix-usings)
         ("r c" . omnisharp-fix-code-issue-at-point)
         ("r a" . omnisharp-run-code-action-refactoring))
  :config
  (define-prefix-command 'omnisharp-mode-map)
  (define-key languages-map (kbd "o") 'omnisharp-mode-map)
  (push 'company-omnisharp company-backends)
  :hook
  ((csharp-mode . omnisharp-mode)))

(use-package powershell
  :defer t
  :ensure t)

(use-package kotlin-mode
  :defer t
  :ensure t
  :defines languages-map
  :bind (:map kotlin-mode-map
              ("z" . kotlin-repl))
  :config
  (define-prefix-command 'kotlin-mode-map)
  (define-key languages-map (kbd "k") 'kotlin-mode-map))

(use-package graphviz-dot-mode
  :defer t
  :ensure t)

(use-package tide
  :defer t
  :ensure t
  :defines languages-map
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . (lambda ()
                              (setq typescript-indent-level 2))))
  :bind (
         :map tide-mode-map
         ("r" . tide-rename-symbol)
         ("f" . tide-fix)
         ("." . #'tide-jump-to-definition)
         ("," . #'tide-jump-back)
         ("/" . #'tide-jump-to-implementation))
  :config
  (define-prefix-command 'tide-mode-map)
  (define-key languages-map (kbd "t") 'tide-mode-map)
  (setq tide-format-options '(:indentSize 2 :insertSpaceBeforeFunctionParenthesis t :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :insertSpaceAfterConstructor t)))

(use-package prettier-js
  :defer t
  :ensure t
  :diminish
  :hook ((typescript-mode . prettier-js-mode)))

(use-package tuareg
  :defer t
  :ensure t)

(use-package go-mode
  :defer t
  :ensure t)

(provide 'init-languages)
;;; init-languages.el ends here

