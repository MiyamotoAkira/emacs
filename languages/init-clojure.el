; Clojure modifications
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(require 'clj-refactor)
(defun clj-refactor-setup ()
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

(defun my-pretty-lambda-clojure ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("fn" . 955) ; λ
          )))

(add-hook 'clojure-mode-hook 'my-pretty-lambda-clojure)

(provide 'init-clojure)

