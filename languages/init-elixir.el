(defun my-pretty-lambda-elixir ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("fn" . 955) ; λ
          )))

(add-hook 'elixir-mode-hook 'my-pretty-lambda-elixir)

(provide 'init-elixir)
