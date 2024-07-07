;;; package --- Summary
;;;Languages setup

;;; Commentary:
;;; Here we are setting up all the neccessary elements for
;;; the support of several languages.

;;; Code:

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package yasnippet
  :diminish yas-minor-mode
  ;; :defines tools-map
  ;; :bind (:map yas-minor-mode-map
  ;;             ("n" . yas-new-snippet)
  ;;             ("s" . yas-insert-snippet)
  ;;             ("v" . yas-visit-snippet-file))
  :config
  ;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)
  ;; (define-prefix-command 'yas-minor-mode-map)
  ;; (define-key tools-map (kbd "y") 'yas-minor-mode-map)
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package auto-yasnippet
  :diminish yas-minor-mode)

(use-package flycheck-pos-tip)

(use-package eldoc
  :diminish
  :hook
  (prog-mode . turn-on-eldoc-mode)
  (cider-repl-mode . turn-on-eldoc-mode))

(use-package flycheck
  :after (flycheck-pos-tip-mode)
  :config
  (show-paren-mode 1)
  (flycheck-pos-tip-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :hook
  ((after-init . global-flycheck-mode)))

(use-package tagedit)

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

;; (use-package highlight-indentation
;;   :defer nil
;;   :hook
;;   ((prog-mode . highlight-indentation-mode)))

;; (use-package highlight-sexp
;;   :quelpa (abc-mode :fetcher github :repo "daimrod/highlight-sexp")
;;   :hook
;;   ((clojure-mode lisp-mode emacs-lisp-mode) . highlight-sexp-mode))

(use-package aggressive-indent
  :hook
  ((emacs-lisp-mode . aggressive-indent-mode)))

(use-package company
  :defer nil
  :diminish
  :bind (("C-S-i" . company-complete)
         ;; :map company-mode-map
	 ;; ("<tab>". tab-indent-or-complete)
	 ;; ("TAB". tab-indent-or-complete)
         :map company-active-map
         ("C-n". company-select-next)
	 ("C-p". company-select-previous)
	 ("M-<". company-select-first)
	 ("M->". company-select-last))
  :hook
  ((after-init . global-company-mode)))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  ;; (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
  )

(use-package buttercup)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.tsx\\'" "\\.jsx\\'")
  :hook
  ((web-mode . my-web-mode-hook)
   (web-mode . (lambda ()
                 (when (string-equal "tsx" (file-name-extension buffer-file-name))
                   (setup-tide-mode))))))


(use-package json-mode)

;; Copied from https://ddavis.io/posts/emacs-python-lsp/
(defun dd/py-workon-project-venv ()
  "Call pyenv-workon with the current projectile project name.
This will return the full path of the associated virtual
environment found in $WORKON_HOME, or nil if the environment does
not exist."
  (let ((pname (projectile-project-name)))
    (pyvenv-workon pname)
    (if (file-directory-p pyvenv-virtual-env)
        pyvenv-virtual-env
      (pyvenv-deactivate))))

(defun dd/py-auto-lsp ()
  "Turn on lsp mode in a Python project with some automated logic.
Try to automatically determine which pyenv virtual environment to
activate based on the project name, using
`dd/py-workon-project-venv'. If successful, call `lsp'. If we
cannot determine the virtualenv automatically, first call the
interactive `pyvenv-workon' function before `lsp'"
  (interactive)
  (let ((pvenv (dd/py-workon-project-venv)))
    (if pvenv
        (lsp)
      (progn
        (call-interactively #'pyvenv-workon)
        (lsp)))))

(use-package python-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
  :bind (:map python-mode-map
              ("C-C C-a" . #'dd/py-auto-lsp)))

(use-package poetry)

(autoload 'python-mode "python-mode")

(use-package python-docstring)

(use-package apheleia
  :diminish
  :init (apheleia-global-mode +1))

(use-package blacken
  :hook ((python-mode . blacken-mode)))

(use-package company-jedi
  :hook ((python.mode . (lambda () (add-to-list 'company-backends 'company-jedi)))))

;; (use-package pyenv
;;   :quelpa (pyenv :fetcher github :repo "aiguofer/pyenv.el"))

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package dockerfile-mode
  :mode "\\.Dockerfile\\'")

(use-package yaml-mode)

(use-package terraform-mode
  :hook
  ((terraform-mode . terraform-format-on-save-mode)))

(use-package company-terraform
  :config
  (company-terraform-init))

(use-package go-mode)

(use-package markdown-mode
  :diminish
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :config
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc")))

(use-package adoc-mode
  :diminish)

(use-package flycheck-clj-kondo)

(use-package cider
  :pin melpa-stable
  :hook
  ((cider-repl-mode . paredit-mode)
   (cider-mode . paredit-mode)
   (cider-mode . eldoc-mode)
   (cider-mode . company-mode)
   (cider-repl-mode . company-mode))
  :bind (("C-c M-a" . cider-insert-last-sexp-in-repl))
  :config
  (unbind-key "C-c M-p" cider-mode-map)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-enable-completion nil)
  (setq lsp-enable-indentation nil))

(use-package clojure-mode
  :diminish
  :pin melpa-stable
  :config
  (require 'flycheck-clj-kondo)
  :hook
  ((clojure-mode . subword-mode)
   (clojure-mode . aggressive-indent-mode)
   (clojure-mode . (lambda ()
                     (setq inferior-lisp-program "lein repl")
                     (font-lock-add-keywords
                      nil
                      '(("(\\(facts?\\)"
                         (1 font-lock-keyword-face))
                        ("(\\(background?\\)"
                         (1 font-lock-keyword-face))))
                     (define-clojure-indent (fact 1))
                     (define-clojure-indent (facts 1))))
   (clojure-mode . cider-mode)
   (clojure-mode . my-pretty-lambda-clojure)
   (clojure-mode . column-enforce-mode)
   (clojure-mode . flycheck-mode)))

(use-package clojure-ts-mode)

;; (use-package midje-mode
;;   :defer t
;;   :ensure t
;;   :pin melpa-stable
;;   :config
;;   (add-hook 'clojure-mode-hook 'midje-mode))

;; (use-package clojure-jump-to-file
;;   :defer t
;;   :ensure t)

(defun clj-clojure-setup ()
  "Functionality to be added for Clojure."
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :diminish
  :pin melpa-stable
  :hook
  (clojure-mode . clj-clojure-setup)
  :init
  (setq cljr-add-ns-to-blank-clj-files nil))

(use-package clojure-mode-extra-font-locking
  :pin melpa-stable)

(use-package kaocha-runner
  :init
  (bind-keys :prefix-map ar-emacs-kaocha-prefix-map
             :prefix "C-c k"
             ("t" . kaocha-runner-run-test-at-point)
             ("r" . kaocha-runner-run-tests)
             ("a" . kaocha-runner-run-all-tests)
             ("w" . kaocha-runner-show-warnings)
             ("h" . kaocha-runner-hide-windows)))

(setq lsp-keymap-prefix "C-c l")

(use-package mermaid-mode
  :mode ("\\.mmd\\'")
  ;; Uncomment when testing improvements
  ;; :load-path "/home/akira/code/external/mermaid-mode"
  :config
  (setq mermaid-mmdc-location "/home/akira/node_modules/.bin/mmdc"))

(use-package lsp-mode
  :defines lsp-highlight-symbol-at-point
  :commands (lsp lsp-deferred)
  :hook (;; (csharp-mode . lsp)
         ;; (python-mode . lsp)
         (clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init (setq lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil
              lsp-keymap-prefix "C-c l"

              lsp-lens-enable t
              lsp-signature-auto-activate nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-to-list 'lsp-disabled-clients 'omnisharp))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-update-mode 'point)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 0.5
              lsp-ui-doc-position 'bottom
	      lsp-ui-doc-max-width 100)
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)


(use-package dap-mode
  :after lsp-mode
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug))
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  (slime-setup '(slime-fancy slime-company))
  (setq slime-lisp-implementations
        '((sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))
  :config
  (unbind-key "C-c M-p" slime-mode-indirect-map)
  :bind (:map slime-mode-indirect-map
              ("C-c P" . slime-repl-set-package)))

(use-package slime-company
  :config
  (setq slime-company-major-modes (quote (lisp-mode slime-repl-mode))))


;; This one has to happen after all modes that use parens are loaded
(use-package paredit
  :diminish
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)
   (eval-expression-minibuffer-setup . enable-paredit-mode)
   (ielm-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (lisp-interaction-mode . enable-paredit-mode)
   (scheme-mode . enable-paredit-mode)
   (clojure-mode . enable-paredit-mode)
   (lfe-mode . enable-paredit-mode)))

(use-package rainbow-delimiters
  :diminish
  :hook
  ((prog-mode . rainbow-delimiters-mode)))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  :hook
  ((latex-mode . turn-on-reftex)
   (LaTeX-mode . turn-on-reftex)))

(use-package company-auctex)

(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable))

(use-package geiser-mit)

(use-package geiser-chez)

(use-package lua-mode)

(use-package company-lua)

(use-package luarocks)

(use-package powershell)

(use-package graphviz-dot-mode)


(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package merlin
  :hook ((tuareg-mode . merlin-mode)
         (caml-mode . merlin-mode))
  :config
  (setq merlin-command 'opam)
  (setq merlin-error-after-save nil))

(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

(use-package dune)

(use-package merlin-company)

(use-package merlin-eldoc
  :hook ((tuareg-mode caml-mode) . merlin-eldoc-setup))

(use-package ocp-indent
  :hook ((tuareg-mode . (lambda () (setq ocp-setup-indent t)))
         (caml-mode . (lambda () (setq ocp-indent-caml-mode-setup t)))))

(use-package opam-switch-mode
  :hook
  ((tuareg.mode . opam-switch-mode)))


(use-package glsl-mode)

(provide 'init-languages)
;;; init-languages.el ends here

