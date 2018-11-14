;;; package --- Summary
;;;Windows specific code

;;; Commentary:
;;; This code should only be executed on a windows machine
;;; It is the responsibility of the caller to assert that

;;; Code:
(use-package exec-path-from-shell
  :ensure t)

(exec-path-from-shell-initialize)

(provide 'init-nix)
;;; init-nix.el ends here
