;;; package --- Summary
;;;Additional tools for  Emacs

;;; Commentary:

;;; Code:
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package monky
  :ensure t
  :bind (("C-x C-g" . monky-status)))

(use-package ag
  :ensure t)

(use-package evil
  :ensure t
  :hook
  ((prog-mode . evil-mode))
  :config
  (define-prefix-command 'spaces-map)
  (define-key evil-normal-state-map (kbd "SPC") 'spaces-map)
  (define-prefix-command 'languages-map)
  (define-key spaces-map (kbd "l") 'languages-map)
  )

(use-package command-log-mode
  :ensure t
  :custom
  (command-log-mode-key-binding-open-log "C-c C-o"))

(use-package projectile
  :ensure t
  :diminish
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :bind (([f8] . dired-sidebar-toggle-sidebar)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package floobits
  :ensure t)

(use-package shut-up
  :ensure t)

(use-package org
  :defer t
  :ensure t
  :bind (("C-c m p" . org-mobile-push)
         ("C-c m f" . org-mobile-pull))
  :defines org-mobile-directory
  :defines org-mobile-inbox-for-pull
  :config
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

;; Setting up babel for running code  in org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (elixir . t)
   (clojure . t)
   (shell . t)
   (ruby . t)
   (fsharp . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(use-package undo-tree
  :ensure t
  :defer t)

(use-package goto-chg
  :ensure t
  :defer t)

(use-package multiple-cursors
  :ensure t)

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80))

(provide 'init-tools)
;;; init-tools.el ends here
