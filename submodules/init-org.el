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

(use-package org-ref
  :ensure t
  :defer t)

(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Sync/slip-box"))
  (org-roam-dailies-directory "journal/")
  (org-roam-complete-everywhere t)
  (org-roam-db-autosync-mode)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %\n")
      :unnarrowed t)
     ("l" "literary notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Idea: %?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %\n#+filetags: LiteraryNote\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" plain
      "* %<%H:%M>\n  %?\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d\n")
      :unnarrowed t)
     ("m" "meeting" plain
      "* %<%H:%M>\n  Reason: %^{Reason}\n  Participants: %^{Participants}\n  Decisions: %?\n  Improvements:\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n#+filetags: Meeting\n")
      :unnarrowed t)
     ("l" "literary entry" plain
      "* %<%H:%M>\n  Author: %^{Author}\n  Title: %^{Title}\n  Year: %^{Year}\n  Page Reference:%^{Page Reference}\n\n  %?\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n#+filetags: LiteraryEntry \n")
      :unnarrowed t)))
  :bind (("C-c z l" . org-roam-buffer-toggle)
         ("C-c z f" . org-roam-node-find)
         ("C-c z i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c z d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-setup))

(provide 'init-org)
;;; init-org.el ends here
