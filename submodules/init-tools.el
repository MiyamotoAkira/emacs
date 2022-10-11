;; package --- Summary
;; Additional tools for  Emacs
;;; Commentary: Nearly everything over here has to load. There is no autoload system
;;; Code:

(use-package async)

(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))

(use-package no-littering
  :defer nil
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Automatically save the last place we were on files when closing buffers
(use-package saveplace
  :defer nil
  :config
  (save-place-mode))

(use-package monky
  :bind (("C-x M-g" . monky-status)))

(defun nothing())

(use-package ag
  :bind (("C-c a a" . ag)
         ("C-c a f" . ag-files)
         ("C-c a d" . ag-dired)
         ("C-c a r" . ag-regex)
         ("C-c a p" . ag-project))
  :config
  (setq ag-reuse-buffers 't)
  (setq ag-highlight-search 't))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package command-log-mode
  :custom
  (command-log-mode-key-binding-open-log "C-c C-o"))

(use-package projectile
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
  :bind (("C-c M-p x" . persp-switch-last)
         ("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :init (persp-mode)
  :config
  (setq persp-interactive-completion-function #'ivy-completing-read)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p")))

(use-package persp-projectile
  :bind ("C-c M-p P" . projectile-persp-switch-project))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :bind (([f8] . dired-sidebar-toggle-sidebar)))

(add-hook 'mhtml-mode-hook (lambda ()
                             (define-key html-mode-map (kbd "M-o") nil)
                             (define-key html-mode-map (kbd "C-c C-p") 'facemenu-keymap)
                             (define-key html-mode-map (kbd "M-o") 'ace-window)))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package all-the-icons
  :defer 2)

(use-package all-the-icons-dired
  :after (dired-sidebar all-the-icons)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package shut-up
  :defer 2)

(use-package undo-tree
  :defer 2)

(use-package goto-chg
  :defer 2)

(use-package multiple-cursors
  :defer 2)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80))

(use-package ivy
  :diminish
  :bind (("C-c v" . 'ivy-push-view)
         ("C-c V" .  'ivy-pop-view))
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus))))

(use-package counsel
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
  :bind (("C-s" . 'swiper-isearch)
         ("C-r" . 'swiper-isearch-backward)))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([rempa describe-key] . helpful-key))

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
  :pin melpa)

(use-package flyspell
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

(use-package column-enforce-mode
  :defer 2)

(add-to-list 'load-path "~/code/personal/structurizr-mode")
(require 'structurizr-mode)

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "~/bin/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

;; Package management visual improvements
;; (use-package paradox
;;   :defer nil
;;   :custom
;;   (paradox-github-token t)
;;   :config
;;   (paradox-enable))

(use-package esup
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)

(use-package elfeed
  :commands elfeed
  :bind (("C-x w" . elfeed))
  :config
  (setq elfeed-db-directory "~/Sync/elfeed/db"
        elfeed-enclosure-default-dir "~/Sync/elfeed/enclosures/")
  (make-directory elfeed-db-directory t))

(use-package vterm
  :ensure t
  :bind (("C-q" . vterm-send-next-key)))

(use-package pomm
  :commands (pomm pomm-third-time)
  :custom
  (alert-default-style 'libnotify)
  (pomm-audio-enabled t))

(provide 'init-tools)
;;; init-tools.el ends here
