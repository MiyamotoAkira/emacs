(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(provide 'init-haskell)
