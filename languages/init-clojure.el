;;; pakage --- Summary
;Clojure modifications

;;; Commentary:

;;; Code:
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(require 'clojure-mode-extra-font-locking)

(require 'clj-refactor)
(defun clj-refactor-setup ()
  "Set Emacs to be able to use refactor for clojure."
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'clj-refactor-setup)

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

;;cider hooks
(add-hook 'clojure-mode-hook #'cider-mode)
;;(add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)

(defun my-pretty-lambda-clojure ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(
          ("fn" . 955) ; λ
          )))

(add-hook 'clojure-mode-hook 'my-pretty-lambda-clojure)

(provide 'init-clojure)
;;; init-clojure.el ends here
