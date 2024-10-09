;;; package --- Summary
;;; Commentary:
;;; This is my configuration of Emacs.

;;; Code:

;; The whole let doesn't seem to be needed anymore?
;; Normally file-name-handler-alist is set to

;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;;  ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;;  ("\\`/:" . file-name-non-special))

;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
;; (let ((file-name-handler-alist nil)))


(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; we want custom setup on a separate file. This is set because all the variables
;; that appear there are set on the different .el setup files I have


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; We are organizing files in a sub-directory


(add-to-list 'load-path (expand-file-name "submodules" user-emacs-directory))

(require 'init-packaging)

(require 'bind-key)

;; This is to move setup into an org file
(require 'org)
(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
;; ;; macos special path info (shell and non-shell apps get different paths)
;; ;; not sure if needed due to the below
;; ;;(if (eq system-type 'darwin)
;; ;;	(add-to-list 'my-packages 'exec-path-from-shell))

;; This is to get the path variable read from
;; the shell environment.


(if (memq window-system '(mac ns))
    (setenv "SHELL" "/bin/zsh"))

(if (memq window-system '(mac ns x))
    (require 'init-nix)
  (require 'init-windows))

;; If we start the daemon we are on a nix system


(when (daemonp)
  (require 'init-nix))


(put 'upcase-region 'disabled nil)

(setq read-process-output-max (* 512 1024))
;;; init.el ends here

