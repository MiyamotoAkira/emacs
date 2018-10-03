;;; package --- Summary
;;; Commentary:
;;; This is my configuration of Emacs.

;;; Code:
(setq custom-file "~/.emacs.d/custom.el")

(setq tab-width 4)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "languages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tools" user-emacs-directory))

(setq ansi-color-faces-vector
      [default default default italic underline success warning error])


;; Now selecting a region behaves as in most applications
;; you overwrite the region
(delete-selection-mode 1)

(require 'package)

(setq package-archives
      (quote
       (("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))))

(package-initialize)

;; less refresh the packages
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; we define the packages that we weant to upload
(defvar my-packages
  '())

;; macos special path info (shell and non-shell apps get different paths)
;; not sure if needed due to the below
;;(if (eq system-type 'darwin)
;;	(add-to-list 'my-packages 'exec-path-from-shell))

;; we upload the whole lot
(dolist (pa my-packages)
  (unless (package-installed-p pa)
	(package-install pa)))

(use-package exec-path-from-shell
  :ensure t)

(exec-path-from-shell-initialize)

;; general modifications
;; highlight current line
(global-hl-line-mode 1)

;; line numbers
(global-linum-mode)

;; Use tabs instead of spaces
(setq-default indent-tabs-mode nil)

;; column numbers
(setq column-number-mode t)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-c C-;") 'comment-region)

;; let's pretify those lambdas
(defun my-pretty-lambda (lambda-string)
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        ;; λ
        '((lambda-string . 955))))

(defun my-pretty-lambda-elixir ()
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        ;; λ
        '(("fn" . 955))))

(defun my-pretty-lambda-clojure ()
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        ;; λ
        '(("fn" . 955))))

(global-prettify-symbols-mode 1)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))

;; eldoc configuration
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(use-package flycheck
  :ensure t
  :config
  ;;(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (show-paren-mode 1)  (add-hook 'after-init-hook #'global-flycheck-mode))

(add-hook 'html-mode-hook
		  (lambda ()
			;; Default indentation is usually 2 spaces, changing to 4.
			(set (make-local-variable 'sgml-basic-offset) 4)))

(use-package tagedit
  :ensure t)

(use-package highlight-indentation
  :ensure t)

(use-package shut-up
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package company
  :ensure t
  :bind (([C-S-i] . company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package web-mode
  :defer t
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
  :config
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package gradle-mode
  :defer t
  :ensure t)

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package puppet-mode
  :defer t
  :ensure t)

(use-package scala-mode
  :defer t
  :ensure t)

(use-package robe
  :defer t
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
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

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package command-log-mode
  :ensure t)

(use-package projectile
  :ensure t)

(use-package flx
  :ensure t)

(use-package flx-ido
  :ensure t)

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :config
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc")))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-mode t)
  (ido-ubiquitous-mode 1)
  (setq ido-auto-merge-work-directories-length -1))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

(use-package org
  :defer t
  :ensure t)

(use-package org-plus-contrib
  :defer t
  :ensure t)

(use-package org-present
  :defer t
  :ensure t
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (global-linum-mode -1)
              (global-hl-line-mode -1)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (global-linum-mode)
              (global-hl-line-mode 1))))

(use-package elixir-mode
  :defer t
  :ensure t
  :config
  (add-hook 'elixir-mode-hook 'my-pretty-lambda-elixir)
  (add-hook 'elixir-mode-hook (lambda ()
                                (setq tab-width 2)
                                (setq indent-tabs-mode nil))))

(use-package ob-elixir
  :ensure t
  :defer t)

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

(use-package csharp-mode
  :defer t
  :ensure t)

(use-package java-imports
  :ensure t
  :defer t
  :config
  (add-hook 'java-mode-hook 'java-imports-scan-file))

(use-package lsp-mode
  :defer t
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
  :config 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))

(use-package haskell-mode
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package groovy-mode
  :defer t
  :ensure t)

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
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'java-mode-hook #'rainbow-delimiters-mode))

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle)))

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

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode))

(use-package lua-mode
  :defer t
  :ensure t)

(use-package company-lua
  :defer t
  :ensure t)

(use-package floobits
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

(use-package omnisharp
  :defer t
  :ensure t
  :bind (([C-c o s s] . omnisharp-start-omnisharp-server)
         ([C-c o s p] . omnisharp-stop-server)
         ([C-c o a] . omnisharp-auto-complete)
         ([C-c o c r] . recompile)
         ([C-c o i t] . omnisharp-current-type-information)
         ([C-c o i d] . omnisharp-current-type-documentation)
         ([C-c o i s] . omnisharp-show-overloads-at-point)
         ([C-c o g d] . omnisharp-go-to-definition)
         ([C-c o g w] . omnisharp-go-to-definition-other-window)
         ([C-c o g u] . omnisharp-find-usages)
         ([C-c o g i] . omnisharp-find-implementations)
         ([C-c o g s] . omnisharp-navigate-to-solution-member)
         ([C-c o g m] . omnisharp-navigate-to-current-file-member)
         ([C-c o g f] . omnisharp-navigate-to-solution-file-then-file-member)
         ([C-c o k d] . omnisharp-code-format-entire-file)
         ([C-c o r r] . omnisharp-rename)
         ([C-c o r f] . omnisharp-fix-usings)
         ([C-c o r c] . omnisharp-fix-code-issue-at-point)
         ([C-c o r a] . omnisharp-run-code-action-refactoring))
  :config
  (push 'company-omnisharp company-backends)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package ag
  :ensure t)

(defun fullscreen ()
  "Puts Emacs on fullscreen mode."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(use-package solarized-theme
  :ensure t)

(load-theme 'solarized-dark t)

(defvar current-dark t)

(defun toggle-theme ()
  "Change the theme used on Emacs between a dark and a light themes."
  (interactive)
  (if current-dark
      (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t)
    )
  (setq current-dark (not current-dark)))

(global-set-key (kbd "C-c C-.") 'toggle-theme)

(setq split-height-threshold nil)
(setq split-width-threshold 80)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (elixir . t)
   (clojure . t)
   (shell . t)
   (ruby . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
(fullscreen)
;;; init.el ends here
