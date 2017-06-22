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
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "cornflower blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "orange")))))

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

; we define the packages that we weant to upload
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
	flycheck
	fsharp-mode
	go-mode
	haskell-mode
	highlight-indentation
	ido-ubiquitous
	jdee
	magit
	markdown-mode
	neotree
	omnisharp
	paredit
	puppet-mode
	rainbow-delimiters
	robe
	scala-mode
	shut-up
	smex
	tagedit
	web-mode
	yaml-mode
	;;flycheck-color-mode-line
	))

; macos special path info (shell and non-shell apps get different paths)
; not sure if needed due to the below
;(if (eq system-type 'darwin)
;	(add-to-list 'my-packages 'exec-path-from-shell))

; we upload the whole lot
(dolist (pa my-packages)
  (unless (package-installed-p pa)
	(package-install pa)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; general modifications
; Settings for different tools
(require 'init-company)
(require 'init-ido)
(require 'init-paredit)
(require 'init-rainbow)
(require 'init-eldoc)
(require 'init-smex)
(require 'init-web-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
;(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

(show-paren-mode 1)
; highlight current line
;(global-hl-line-mode 1)

; line numbers
(global-linum-mode)

; column numbers
(setq column-number-mode t)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

;;magit
(global-set-key (kbd "C-x g") 'magit-status)


(global-set-key (kbd "C-c C-;") 'comment-region)

;; Settings for different languages
(require 'init-elisp)
(require 'init-markdown)
(require 'init-go)
(require 'init-haskell)
(require 'init-clojure)
(require 'init-elixir)
(require 'init-lfe)
(require 'init-ruby)
(require 'init-csharp)
(require 'init-java)


;; let's pretify those lambdas
;; (defun my-pretty-lambda-scheme ()
;;   "make some word or string show as pretty Unicode symbols"
;;   (setq prettify-symbols-alist
;;         '(
;;           ("lambda" . 955) ; λ
;;           )))

										;(add-hook 'scheme-mode-hook 'my-pretty-lambda-scheme)



(global-prettify-symbols-mode 1)

;; (defadvice ido-find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; init.el ends here
