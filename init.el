(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(package-archives
   (quote
	(("gnu" . "http://elpa.gnu.org/packages/")
	 ("melpa-stable" . "http://stable.melpa.org/packages/")
	 ("org" . "http://orgmode.org/elpa/"))))
'(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/vendor")

(require 'package)

(package-initialize)

; less refresh the packages
(unless package-archive-contents
  (package-refresh-contents))

; we define the packages that we weant to upload
(defvar my-packages
  '(
   auto-complete
   paredit
   clojure-mode
   clojure-mode-extra-font-locking
   cider
   ac-cider
   ido-ubiquitous
   smex
   rainbow-delimiters
   tagedit
   magit
   elixir-mode
   company
   alchemist
   go-mode
   haskell-mode
   yaml-mode
   markdown-mode))

; macos special path info (shell and non-shell apps get different paths)
; not sure if needed due to the below
;(if (eq system-type 'darwin)
;	(add-to-list 'my-packages 'exec-path-from-shell))

; we upload the whole lot
(dolist (pa my-packages)
  (unless (package-installed-p pa)
	(package-install pa)))


; general modifications
(require 'ido)
(ido-mode t)
(ido-ubiquitous-mode 1)
(setq ido-auto-merge-work-directories-length -1)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(show-paren-mode 1)
; highlight current line
;(global-hl-line-mode 1)

; line numbers
(global-linum-mode)

; column numbers
(setq column-number-mode t)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

; auto complete
(ac-config-default)


;; markdown settings
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; Go modifications
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
						  "[ \t\n]*$"
						  ""
						  (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
	(setenv "PATH" path-from-shell)
	(setq eshell-path-env path-from-shell) ; for eshell users
	(setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setenv "GOPATH" "~/code")

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (setq exec-path (cons "c:/go/bin" exec-path)))
  (t (setq exec-path (cons "/usr/local/go/bin" exec-path))))
(add-to-list 'exec-path "~/code/bin")

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

; Haskell modifications
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

; Clojure modifications
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

; cider hooks
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(add-hook 'cider-mode-hook #'eldoc-mode)
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (add-to-list 'ac-modes 'cider-mode)
;;      (add-to-list 'ac-modes 'cider-repl-mode)))
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)


; lisp modifications
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'schem-mode-hook #'rainbow-delimiters-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


; let's pretify those lambdas
;; (defun my-pretty-lambda-scheme ()
;;   "make some word or string show as pretty Unicode symbols"
;;   (setq prettify-symbols-alist
;;         '(
;;           ("lambda" . 955) ; λ
;;           )))

;(add-hook 'scheme-mode-hook 'my-pretty-lambda-scheme)

(defun my-pretty-lambda-clojure ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("fn" . 955) ; λ
          )))

(add-hook 'clojure-mode-hook 'my-pretty-lambda-scheme)

(global-prettify-symbols-mode 1)
