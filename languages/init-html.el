;;; package --- Sumary
;;; Commentary:
;;; Code:
(add-hook 'html-mode-hook
		  (lambda ()
			;; Default indentation is usually 2 spaces, changing to 4.
			(set (make-local-variable 'sgml-basic-offset) 4)))

(provide 'init-html)
;;; init-html.el ends here