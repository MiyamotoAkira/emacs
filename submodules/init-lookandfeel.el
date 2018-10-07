;;; package --- Summary
;;;Look and feel of Emacs

;;; Commentary:

;;; Code:
;; general modifications

(setq tab-width 4)

;; highlight current line
(global-hl-line-mode 1)

;; line numbers
(global-linum-mode)

;; Use tabs instead of spaces
(setq-default indent-tabs-mode nil)

;; column numbers
(setq column-number-mode t)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-c C-;") 'comment-region)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(defun fullscreen ()
  "Puts Emacs on fullscreen mode."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(use-package solarized-theme
  :ensure t)

(load-theme 'solarized-dark t)

(defvar current-dark t)

(defun toggle-theme ()
  "Change the theme used on Emacs between a dark and a light themes."
  (interactive)
  (if current-dark
      (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t)
    )
  (setq current-dark (not current-dark)))

(global-set-key (kbd "C-c C-.") 'toggle-theme)

(fullscreen)

(setq ansi-color-faces-vector
      [default default default italic underline success warning error])


;; Now selecting a region behaves as in most applications
;; you overwrite the region
(delete-selection-mode 1)

;; let's pretify those lambdas
(defun my-pretty-lambda (lambda-string)
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        ;; λ
        '((lambda-string . 955))))

(defun my-pretty-lambda-elixir ()
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        ;; λ
        '(("fn" . 955))))

(defun my-pretty-lambda-clojure ()
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        ;; λ
        '(("fn" . 955))))

(global-prettify-symbols-mode 1)

(use-package flx
  :ensure t)

(use-package flx-ido
  :ensure t)

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-mode t)
  (ido-ubiquitous-mode 1)
  (setq ido-auto-merge-work-directories-length -1))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(setq split-height-threshold nil)
(setq split-width-threshold 80)

(provide 'init-lookandfeel)
;;; init-lookandfeel.el ends here
