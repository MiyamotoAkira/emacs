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
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     ("marmalade" . "https://marmalade-repo.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (projectile yaml-mode which-key web-mode tagedit smex scala-mode rust-mode robe rainbow-delimiters puppet-mode omnisharp neotree markdown-mode magit jdee ido-ubiquitous highlight-indentation haskell-mode go-mode fsharp-mode flx-ido exec-path-from-shell erlang elm-mode clojure-mode-extra-font-locking clj-refactor alchemist aggressive-indent)))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode-extra-font-locking . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
;;(add-to-list 'package-pinned-packages '(omnisharp . "melpa-stable") t)


;; less refresh the packages
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; we define the packages that we weant to upload
(defvar my-packages
  '(
	aggressive-indent
	alchemist
	cider
	clj-refactor
	clojure-mode
	clojure-mode-extra-font-locking
	company
	elixir-mode
	elm-mode
	erlang
	exec-path-from-shell
    flx
    flx-ido
	flycheck
	fsharp-mode
	go-mode
	haskell-mode
	highlight-indentation
	ido-ubiquitous
	jdee
	magit
	neotree
	omnisharp
	paredit
    projectile
	puppet-mode
	rainbow-delimiters
    rust-mode
	robe
	scala-mode
	shut-up
	smex
	tagedit
	web-mode
    which-key
	yaml-mode
	;;flycheck-color-mode-line
	))

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
;; Settings for different tools
(require 'init-company)
(require 'init-ido)
(require 'init-paredit)
(require 'init-rainbow)
(require 'init-eldoc)
(require 'init-smex)
(require 'init-web-mode)

(which-key-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
;;(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

(show-paren-mode 1)
;; highlight current line
;;(global-hl-line-mode 1)

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

;;magit
(global-set-key (kbd "C-x g") 'magit-status)


(global-set-key (kbd "C-c C-;") 'comment-region)


(global-set-key [f8] 'neotree-toggle)

;; Settings for different languages
(require 'init-elisp)
(require 'init-go)
(require 'init-haskell)
(require 'init-clojure)
(require 'init-elixir)
(require 'init-lfe)
(require 'init-ruby)
(require 'init-csharp)
(require 'init-java)
(require 'init-html)


;; let's pretify those lambdas
;; (defun my-pretty-lambda-scheme ()
;;   "make some word or string show as pretty Unicode symbols"
;;   (setq prettify-symbols-alist
;;         '(
;;           ("lambda" . 955) ; λ
;;           )))

										;(add-hook 'scheme-mode-hook 'my-pretty-lambda-scheme)



(global-prettify-symbols-mode 1)

(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'utf-8)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package command-log-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc")))
;;; init.el ends here
