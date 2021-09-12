;;; package --- Summary
;;;Languages setup

;;; Commentary:
;;; Here we are setting up all the neccessary elements for
;;; the support of several languages.

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  ;; :defines tools-map
  ;; :bind (:map yas-minor-mode-map
  ;;             ("n" . yas-new-snippet)
  ;;             ("s" . yas-insert-snippet)
  ;;             ("v" . yas-visit-snippet-file))
  :config
  ;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)
  ;; (define-prefix-command 'yas-minor-mode-map)
  ;; (define-key tools-map (kbd "y") 'yas-minor-mode-map)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package auto-yasnippet
  :diminish yas-minor-mode
  :ensure t)

(use-package flycheck-pos-tip
  :ensure t)

(use-package flycheck
  :ensure t
  :after (flycheck-pos-tip-mode)
  :config
  (show-paren-mode 1)
  (flycheck-pos-tip-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :hook
  ((after-init . global-flycheck-mode)))

(use-package tagedit)

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package highlight-indentation)

(use-package aggressive-indent
  :hook
  ((emacs-lisp-mode . aggressive-indent-mode)))

(use-package company
  :diminish
  :bind (("C-S-i" . company-complete))
  :hook
  ((after-init . global-company-mode)))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

(use-package mmm-mode
  :ensure t
  :defer t  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php))

(use-package buttercup
  :ensure t)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package web-mode
  :defer t
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.tsx\\'" "\\.jsx\\'")
  :hook
  ((web-mode . my-web-mode-hook)
   (web-mode . (lambda ()
                 (when (string-equal "tsx" (file-name-extension buffer-file-name))
                   (setup-tide-mode))))))


(use-package vue-mode
  :defer t
  :ensure t)

(use-package eslintd-fix
  :defer t
  :ensure t
  :hook
  ((js-mode . eslintd-fix-mode)
   (vue-mode . eslintd-fix-mode)))

(use-package json-mode
  :ensure t
  :defer t)

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :hook ((elpy-mode . flycheck-mode))
;;   :bind (
;;          :map elpy-mode-map
;;          ("C-c ." . #'elpy-goto-definition)
;;          ("C-c /" . #'elpy-goto-implementation))
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))

(use-package blacken
  :ensure t
  :defer t
  :hook ((python-mode . blacken-mode)))

(use-package pyvenv
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
  :hook
  ((terraform-mode . terraform-format-on-save-mode)))

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

;; (use-package cargo
;;   :defer t
;;   :ensure t
;;   :hook
;;   ((rust-mode . cargo-minor-mode)))

(use-package rustic
  :ensure t
  :after rust-mode
  :hook ((rustic-mode . (lambda ()
                          (lsp-ui-doc-mode)
                          (company-mode))))
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
  (custom-set-faces
   '(rustic-compilation-column ((t (:inherit compilation-column-number))))
   '(rustic-compilation-line ((t (:foreground "LimeGreen"))))))

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
  :diminish
  :defer t
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :config
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc")))

(use-package adoc-mode
  :diminish t
  :defer t
  :ensure t)

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

(use-package flycheck-clj-kondo
  :ensure t)

(use-package cider
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
  :diminish
  :pin melpa-stable
  :config
  (require 'flycheck-clj-kondo)
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
   (clojure-mode . my-pretty-lambda-clojure)
   (clojure-mode . column-enforce-mode)))

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
  :diminish
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

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :defer t
  :defines lsp-highlight-symbol-at-point
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((csharp-mode . lsp)
         (python-mode . lsp)
         (java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init (setq lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil
              lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :defer t
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-update-mode 'point))

;; (use-package company-lsp
;;   :after company
;;   :ensure t
;;   :hook
;;   ((java-mode . (lambda () (push 'company-lsp company-backends))))
;;   :config
;;   (setq company-lsp-cache-candidates t)
;;   (push 'company-lsp company-backend))


(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; (use-package dap-java
;;   :after 'lsp-java)

(use-package lsp-java
  :ensure t
  :hook
  ((java-mode . lsp-java-enable)
   (java-mode . flycheck-mode)
   (java-mode . company-mode)
   (java-mode . (lambda () (lsp-ui-flycheck-enable t)))
   (java-mode . lsp-ui-mode)))

(use-package lsp-jedi
  :ensure t
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (add-to-list 'lsp-disabled-clients 'pyls)
  ;;   (add-to-list 'lsp-enabled-clients 'jedi))
  )

(use-package eglot
  :ensure t
  :hook
  ((fsharp-mode . eglot-ensure)))

(use-package fsharp-mode
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.fsproj\\'" . nxml-mode))
  (require 'eglot-fsharp))

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
  (setq slime-company-major-modes (quote (lisp-mode slime-repl-mode))))

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
  (setq slime-company-major-modes (quote (lisp-mode slime-repl-mode))))


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
  ((prog-mode . rainbow-delimiters-mode)))

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

(use-package geiser-mit
  :defer t
  :ensure t)

(use-package geiser-chez
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
  :bind (
         ("C-c o s s" . omnisharp-start-omnisharp-server)
         :map omnisharp-mode-map
         ("C-c o s p" . omnisharp-stop-server)
         ("C-C o a" . omnisharp-auto-complete)
         ("C-C o c r" . recompile)
         ("C-C o i t" . omnisharp-current-type-information)
         ("C-C o i d" . omnisharp-current-type-documentation)
         ("C-C o i s" . omnisharp-show-overloads-at-point)
         ("C-C o g d" . omnisharp-go-to-definition)
         ("C-C o g w" . omnisharp-go-to-definition-other-window)
         ("C-C o g u" . omnisharp-find-usages)
         ("C-C o g i" . omnisharp-find-implementations)
         ("C-C o g s" . omnisharp-navigate-to-solution-member)
         ("C-C o g m" . omnisharp-navigate-to-current-file-member)
         ("C-C o g f" . omnisharp-navigate-to-solution-file-then-file-member)
         ("C-C o k d" . omnisharp-code-format-entire-file)
         ("C-C o r r" . omnisharp-rename)
         ("C-C o r f" . omnisharp-fix-usings)
         ("C-C o r c" . omnisharp-fix-code-issue-at-point)
         ("C-C o r a" . omnisharp-run-code-action-refactoring))
  :config
  (push 'company-omnisharp company-backends)
  :hook
  ((csharp-mode . omnisharp-mode)))

(use-package powershell
  :defer t
  :ensure t)

(use-package kotlin-mode
  :defer t
  :ensure t)

(use-package graphviz-dot-mode
  :defer t
  :ensure t)

(use-package typescript-mode
  :defer t
  :ensure t
  :mode ("\\.ts\\'"))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :defer t
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide-mode)
         (typescript-mode . (lambda ()
                              (setq typescript-indent-level 2))))
  :bind (
         :map tide-mode-map
         ("C-c r" . tide-rename-symbol)
         ("C-c f" . tide-fix)
         ("C-c ." . #'tide-jump-to-definition)
         ("C-c ," . #'tide-jump-back)
         ("C-c /" . #'tide-jump-to-implementation))
  :config
  (setq tide-format-options '(:indentSize 2 :insertSpaceBeforeFunctionParenthesis t :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :insertSpaceAfterConstructor t)))

(use-package prettier-js
  :defer t
  :ensure t
  :diminish
  :hook ((typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package tuareg
  :defer t
  :ensure t)

(use-package go-mode
  :defer t
  :ensure t
  :hook
  ((go-mode . lsp-deferred)))

(use-package php-mode
  :defer t
  :ensure t
  :hook ((php-mode . company-mode)))

(use-package ac-php-core
  :defer t
  :ensure t)

(use-package ac-php
  :defer t
  :ensure t)

(use-package company-php
  :defer t
  :ensure t
  :config
  (push 'company-ac-php-backend company-backends)
  (ac-php-core-eldoc-setup)
  :bind (
         :map php-mode-map
         ("C-c ." . ac-php-find-symbol-at-point)
         ("C-c ," . ac-php-location-stack-back)))

(use-package flycheck-psalm
  :defer t
  :ensure t
  :config
  (flycheck-mode t))

(use-package php-refactor-mode
  :defer t
  :ensure t
  :hook  (php-mode . php-refactor-mode))

(use-package glsl-mode
  :defer t
  :ensure t)

(provide 'init-languages)
;;; init-languages.el ends here

