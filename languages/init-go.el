;;; package --- Summary
;;; Commentary:
;;; Code:
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
  "Set some default behaviour for Go."
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(provide 'init-go)
;;; init-go.el ends here
