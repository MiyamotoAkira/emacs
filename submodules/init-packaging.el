;;; package --- Summary
;;; Setup of the packaging system
;;; Commentary:
;;; The setup of the packaging system that we use. Combo of package.el/quelpa/use-package

;;; Code:

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


(provide 'init-packaging)
;;; init-packaging.el ends here
