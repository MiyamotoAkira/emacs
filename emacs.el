(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(package-archives
   (quote
	(("elpa" . "http://elpa.gnu.org/packages/")
	 ("melpa-stable" . "http://stable.melpa.org/packages/"))))
'(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package-x)


(package-initialize)

(require 'ido)
(ido-mode t)

(add-to-list 'load-path "~/Misc/emacs/go-mode.el")
(require 'go-mode-load)

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
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)  
