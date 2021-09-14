;;; package --- Summary
;;; Commentary:
;;; This is my configuration of Emacs.

;;; Code:
;; Less garbage collection to speed up the thing
(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; we want custom setup on a separate file. This is set because all the variables
;; that appear there are set on the different .el setup files I have
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; We are organizing files in a sub-directory
(add-to-list 'load-path (expand-file-name "submodules" user-emacs-directory))

(require 'package)

(setq package-archives
      (quote
       (("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/"))))

(package-initialize)

;; lets refresh the packages
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; (unless (package-installed-p 'diminish)
;;   (package-install 'diminish))

(eval-when-compile
  (require 'use-package))

;; By default we ensure everything
(custom-set-variables '(use-package-always-ensure t))

;; By default we defer everything
(custom-set-variables '(use-package-always-defer t))

;; t when we need to debug
(custom-set-variables '(use-package-verbose nil))

;; If the .el version is newer, compile even with .elc present
;; This is mostly to deal with no-packaged versions ... i believe
(custom-set-variables '(load-prefer-newer t))
(use-package auto-compile
  :defer nil
  :config (auto-compile-on-load-mode))

;; Installing quelpa so I can download packages from git
(use-package quelpa
  :defer nil
  :init
  (setq quelpa-update-melpa-p nil)
  :config
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
  (require 'quelpa-use-package))
(require 'quelpa)
(quelpa-use-package-activate-advice)

;; by default highlight the matching paren
(show-paren-mode)

(use-package diminish
  :defer 1)

(use-package esup
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)

(require 'bind-key)

(setq org-roam-v2-ack t)

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

;; If we start the daemon we are on a nix system
(when (daemonp)
  (require 'init-nix))

(require 'init-tools)
(require 'init-lookandfeel)
(require 'init-languages)
(require 'init-org)
(require 'init-eldoc)

(put 'upcase-region 'disabled nil)

;; Back to standard gc threshold
(setq gc-cons-threshold (* 2 1000 1000))
;;; init.el ends here
