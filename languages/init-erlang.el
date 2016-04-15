(add-to-list 'load-path (expand-file-name "vendor/erlang" user-emacs-directory))

 (setq erlang-root-dir "/usr/local/otp")
 (setq exec-path (cons "/usr/local/otp/bin" exec-path))
 (require 'erlang-start)

