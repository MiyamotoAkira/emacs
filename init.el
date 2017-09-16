;;; package --- Summary
;;; Commentary:
;;; This is the configuration of Emacs.

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(markdown-command "/usr/bin/pandoc")
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     ("marmalade" . "https://marmalade-repo.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (lfe-mode fsharp csharp cljr-refactor elixir-mode-hook aggresive-indent ido-completing-read+ flx flycheck cider org-present projectile-mode command-log-mode powerline use-package projectile yaml-mode which-key web-mode tagedit smex scala-mode rust-mode robe rainbow-delimiters puppet-mode omnisharp neotree markdown-mode magit jdee ido-ubiquitous highlight-indentation haskell-mode go-mode fsharp-mode flx-ido exec-path-from-shell erlang elm-mode clojure-mode-extra-font-locking clj-refactor alchemist aggressive-indent)))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:underline t))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "cornflower blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "peru"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "medium turquoise"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "gold")))))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "languages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tools" user-emacs-directory))

(require 'package)

(package-initialize)

;; less refresh the packages
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; we define the packages that we weant to upload
(defvar my-packages
  '(
	go-mode
	jdee
	omnisharp))

;; macos special path info (shell and non-shell apps get different paths)
;; not sure if needed due to the below
;;(if (eq system-type 'darwin)
;;	(add-to-list 'my-packages 'exec-path-from-shell))

;; we upload the whole lot
(dolist (pa my-packages)
  (unless (package-installed-p pa)
	(package-install pa)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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


;; Settings for different languages
(require 'init-go)
(require 'init-csharp)
(require 'init-java)

;; let's pretify those lambdas
(defun my-pretty-lambda (lambda-string)
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        '(
          (lambda-string . 955) ; λ
          )))


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

(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
  :config
  (add-hook 'web-mode-hook  'my-web-mode-hook))

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

(use-package yaml-mode
  :ensure t)

(use-package highlight-indentation
  :ensure t)

(use-package puppet-mode
  :ensure t)

(use-package shut-up
  :ensure t)

(use-package aggresive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package scala-mode
  :ensure t)

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package lfe-mode
  :ensure t)

(use-package erlang
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package command-log-mode
  :ensure t)

(use-package projectile-mode
  :ensure t)

(use-package flx
  :ensure t)

(use-package flx-ido
  :ensure t)

(use-package which-key
  :config
  (which-key-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :config
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc")))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-mode t)
  (ido-ubiquitous-mode 1)
  (setq ido-auto-merge-work-directories-length -1))

(use-package org-present
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

(use-package elixir-mode-hook
  :ensure t
  :config
  (add-hook 'elixir-mode-hook (lambda () (my-pretty-lambda "fn")))
  (add-hook 'elixir-mode-hook (lambda ()
                                (setq tab-width 2)
                                (setq indent-tabs-mode nil))))
(use-package alchemist
  :ensure t)

(use-package cljr-refactor
  :ensure t
  :pin melpa-stable
  :config
  (cljr-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode))

(use-package clojure-mode
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
  (add-hook 'clojure-mode-hook (lambda () (my-pretty-lambda "fn"))))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :pin melpa-stable)

(use-package csharp
  :ensure t)

(use-package fsharp-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))


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


(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle)))
;;; init.el ends here
