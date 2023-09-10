;;; package --- Summary
;;;Look and feel of Emacs

;;; Commentary:

;;; Code:
;; general modifications

(defun set-size-font (size)
  (set-face-attribute 'default nil :font (concat "Fira Code-" (number-to-string size))))

(defun set-standard-font ()
  (set-size-font 10))

(defun set-sharing-font ()
  (set-size-font 16))

(defun switch-font (universal)
  "Switches the font between my normal one and the one used to share screen"
  (interactive "P")
  (cond ((equal universal nil) (set-standard-font))
        ((equal universal '(4)) (set-sharing-font))
        (t (set-size-font universal))))

(set-standard-font)

(setq default-tab-width 4)

;; highlight current line
(global-hl-line-mode 1)

;; line numbers
(global-display-line-numbers-mode)

;; by default highlight the matching paren
(show-paren-mode)

(dolist (mode '(eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use tabs instead of spaces
(setq-default indent-tabs-mode nil)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

(global-set-key [remap comment-dwim] #'comment-line)




(winner-mode 1)

(use-package solarized-theme
  :defer nil)

(load-theme 'solarized-dark t)

(defvar current-dark t)

(defun toggle-theme ()
  "Change the theme used on Emacs between a dark and a light themes."
  (interactive)
  (if current-dark
      (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t))
  (setq current-dark (not current-dark)))

;; Doesn't work under Cider. Need to investigate.
(global-set-key (kbd "C-c C-.") 'toggle-theme)

(setq ansi-color-faces-vector
      [default default default italic underline success warning error])

;; Now selecting a region behaves as in most applications
;; you overwrite the region
(delete-selection-mode 1)

(use-package diminish
  :defer nil)

;; let's pretify those lambdas
(defun my-pretty-lambda (lambda-string)
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        ;; λ
        '((lambda-string . 955))))

(defun my-pretty-lambda-elixir ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        ;; λ
        '(("fn" . 955))))

(defun my-pretty-lambda-clojure ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        ;; λ
        '(("fn" . 955))))

(global-prettify-symbols-mode 1)

(use-package spaceline
  :defer nil
  :config
  (spaceline-emacs-theme))

(use-package flx
  :defer 2)

(use-package which-key
  :defer nil
  :diminish
  :config
  (which-key-mode))

;; Line by line, instead of half-screen at a time.
(use-package smooth-scrolling
  :defer 2
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

(use-package dimmer
  :defer 2
  :config
  (dimmer-mode))

(use-package disable-mouse
  :defer 2
  :diminish disable-mouse-global-mode
  :config
  (global-disable-mouse-mode))

;; Favour vertical split over horizontal split
(setq split-height-threshold nil)
(setq split-width-threshold 120)

(defun shell-horizontal ()
  "This function is to display the shell on a horizontal split, whcih is usually more adequate."
  (interactive)
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    (progn
      (shell)
      (setq current (selected-window))
      (setq window (get-buffer-window "*shell*"))
      (select-window window)
      (setq height (window-height window))
      (shrink-window (- height 10))
      (select-window current))))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package page-break-lines
  :defer nil)

(use-package dashboard
  :ensure t
  :defer nil
  :hook
  ((dashboard-mode . page-break-lines-mode))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "May the Force be with you")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5))))

(provide 'init-lookandfeel)
;;; init-lookandfeel.el ends here

