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
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package command-log-mode
  :ensure t
  :custom
  (command-log-mode-key-binding-open-log "C-c C-o"))

(use-package projectile
  :ensure t
  :diminish
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/code/"
                                         "~/code/codurance/"
                                         "~/code/personal/"
                                         "~/code/externals/")))

(use-package perspective
  :ensure t
  :bind (("C-x x x" . persp-switch-last)
         ("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :init (persp-mode +1)
  :config
  (setq persp-interactive-completion-function #'ivy-completing-read))

(use-package persp-projectile
  :ensure t
  :bind ("C-x x P" . projectile-persp-switch-project))

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

(use-package ivy
  :ensure t
  :diminish ivy
  :bind (("C-c v" . 'ivy-push-view)
         ("C-c V" .  'ivy-pop-view))
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus))))

(use-package counsel
  :ensure t
  :bind (("M-x" . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file)
         ("M-y" . 'counsel-yank-pop)
         ;; :map counsel-mode-map
         ;; ("f" . 'counsel-describe-function)
         ;; ("v" . 'counsel-describe-variable)
         ;; ("l" . 'counsel-find-library)
         ;; ("i" . 'counsel-info-lookup-symbol)
         ;; ("u" . 'counsel-unicode-char)
         ;; ("j" .  'counsel-set-variable)
         )
  :config
  (setq counsel-find-file-ignore-regexp "(?:‘[#.])|(?:[#~]’)|(?:[~]’)"))

(use-package swiper
  :ensure t
  :bind (("C-s" . 'swiper-isearch)
         ("C-r" . 'swiper-isearch-backward)))

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

(global-set-key (kbd "C-c C-n") 'insert-line-above)

(global-set-key (kbd "C-c n") 'insert-line-below)

;; We put all backup files on a single place
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))
;; Make sure that tramp uses it as well
(setq tramp-backup-directory-alist backup-directory-alist)
;; And even if the files are in version control
(setq vc-make-backup-files t)

(use-package pos-tip
  :ensure t
  :pin melpa)

(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  (dolist (hook '(change-log-mode-hook log-edit-mode-hook org-agenda-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  :config
  (setq ;;ispell-program-name "/usr/local/bin/aspell"
        ispell-local-dictionary "en_GB"
        ispell-dictionary "english" ; better for aspell
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")
        ispell-list-command "--list"
        ispell-local-dictionary-alist '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "['‘’]"
                                         t ; Many other characters
                                         ("-d" "en_GB") nil utf-8))))

(provide 'init-tools)
;;; init-tools.el ends here
