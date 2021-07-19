;;; package --- Summary
;;; Commentary:
;;; This is my configuration of Emacs.

;;; Code:
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "submodules" user-emacs-directory))

(require 'package)

(setq package-archives
      (quote
       (("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ;; ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))))

(package-initialize)

;; lets refresh the packages
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)
;; ;; macos special path info (shell and non-shell apps get different paths)
;; ;; not sure if needed due to the below
;; ;;(if (eq system-type 'darwin)
;; ;;	(add-to-list 'my-packages 'exec-path-from-shell))

;; This is to get the path variable read from
;; the shell environment.
(if (memq window-system '(mac ns))
    (setenv "SHELL" "/bin/zsh"))

(if (memq window-system '(mac ns x))
    (require 'init-nix)
  (require 'init-windows))

(when (daemonp)
  (require 'init-nix))

(require 'init-tools)
(require 'init-lookandfeel)
(require 'init-languages)
(require 'init-org)
(require 'init-eldoc)
;; (require 'init-helm)

(add-to-list 'load-path "~/code/personal/structurizr-mode")
(require 'structurizr-mode)

(put 'upcase-region 'disabled nil)

;;; init.el ends here
