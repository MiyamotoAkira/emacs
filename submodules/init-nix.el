;;; package --- Summary
;;;Windows specific code

;;; Commentary:
;;; This code should only be executed on a windows machine
;;; It is the responsibility of the caller to assert that

;;; Code:

(setenv "SHELL" "/bin/zsh")

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH"
                                         "ZSH"
                                         "PYENV_ROOT"
                                         "VIRTUALENVWRAPPER_PYTHON"
                                         "PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV"
                                         "WORKON_HOME"
                                         "PROJECT_HOME"
                                         "ANDROID_HOME"
                                         "JAVA_HOME"
                                         "SDKMAN_DIR"))
  (exec-path-from-shell-initialize))

(provide 'init-nix)
;;; init-nix.el ends here
