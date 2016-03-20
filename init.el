;;; This is the configuration of emacs. At the moment, most of it is in this file
;;; thought the idea is to move things out.


;;;


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
	 ("marmalade" . "https://marmalade-repo.org/packages/"))))
 
'(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "languages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tools" user-emacs-directory))

(require 'package)

(package-initialize)

; less refresh the packages
(unless package-archive-contents
  (package-refresh-contents))

; we define the packages that we weant to upload
(defvar my-packages
  '(
   auto-complete
   paredit
   clojure-mode
   clojure-mode-extra-font-locking
   cider
   ac-cider
   clj-refactor
   ido-ubiquitous
   magit
   smex
   rainbow-delimiters
   tagedit
   magit
   elixir-mode
   company
   alchemist
   go-mode
   haskell-mode
   yaml-mode
   markdown-mode
   puppet-mode
   flycheck
;   flycheck-color-mode-line
   scala-mode2
   web-mode
   fsharp-mode))

; macos special path info (shell and non-shell apps get different paths)
; not sure if needed due to the below
;(if (eq system-type 'darwin)
;	(add-to-list 'my-packages 'exec-path-from-shell))

; we upload the whole lot
(dolist (pa my-packages)
  (unless (package-installed-p pa)
	(package-install pa)))


; general modifications
; Settings for different tools
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

; auto complete
(ac-config-default)


;; Settings for different languages
(require 'init-markdown)
(require 'init-go)
(require 'init-haskell)
(require 'init-clojure)
(require 'init-fsharp)
(require 'init-elixir)
(require 'init-lfe)

; let's pretify those lambdas
;; (defun my-pretty-lambda-scheme ()
;;   "make some word or string show as pretty Unicode symbols"
;;   (setq prettify-symbols-alist
;;         '(
;;           ("lambda" . 955) ; λ
;;           )))

;(add-hook 'scheme-mode-hook 'my-pretty-lambda-scheme)



(global-prettify-symbols-mode 1)
