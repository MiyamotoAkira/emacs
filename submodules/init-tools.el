;; package --- Summary
;; Additional tools for  Emacs
;;; Commentary:
;;; Code:

(use-package async
  :ensure t)

(use-package monky
  :ensure t)

(defun nothing())

(use-package ag
  :ensure t
  :bind (("C-c a a" . ag)
         ("C-c a f" . ag-files)
         ("C-c a d" . ag-dired)
         ("C-c a r" . ag-regex)
         ("C-c a p" . ag-project)))

(use-package magit
  :ensure t)

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

(defun insert-line-below (universal)
  "Insert an empty line below the current line.
The behaviour change if you pass the default UNIVERSAL argument.  Without it, a new line below the current one will be created, but the point will not change its location.  With the default UNIVERSAL argument, the point will change to the beginning of the new line created."
  (interactive "P")
  (if (equal universal '(4))
      (progn
        (end-of-line)
        (open-line 1)
        (forward-line))
    (save-excursion
      (end-of-line)
      (open-line 1))))

(defun insert-line-above (universal)
  "Insert an empty line above the current line.
The behaviour change if you pass the default UNIVERSAL argument.  Without it, a new line above the current one will be created, but the point will not change its location.  With the default UNIVERSAL argument, the point will change to the beginning of the new line created."
  (interactive "P")
  (if (equal universal '(4))
      (progn
        (end-of-line 0)
        (open-line 1)
        (forward-line))
    (save-excursion
      (end-of-line 0)
      (open-line 1))))

(global-set-key (kbd "C-c M-n") 'insert-line-above)

(global-set-key (kbd "C-c n") 'insert-line-below)

(provide 'init-tools)
;;; init-tools.el ends here
