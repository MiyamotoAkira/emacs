;; package --- Summary
;; Org mode setup
;;; Commentary:
;;; Code:

(use-package org
  :defer t
  :ensure t
  :bind (("C-c m p" . org-mobile-push)
         ("C-c m f" . org-mobile-pull))
  :defines org-mobile-directory
  :defines org-mobile-inbox-for-pull
  :config
  (setq org-startup-truncated nil)
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-directory "~/org")
  (setq org-mobile-inbox-for-pull "~/org/flagged.org")
  (setq org-agenda-files '("~/org/agendas/")))

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

(use-package ob-elixir
  :ensure t
  :defer t)

(use-package ob-fsharp
  :ensure t
  :defer t)

(use-package ob-rust
  :ensure t
  :defer t)

(use-package ob-typescript
  :ensure t
  :defer t)

;; Setting up babel for running code  in org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (elixir . t)
   (clojure . t)
   (shell . t)
   (ruby . t)
   (fsharp . t)
   (js . t)
   (typescript . t)
   (rust . t)
   (plantuml . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(provide 'init-org)
;;; init-org.el ends here
