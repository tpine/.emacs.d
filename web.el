;;; web.el --- Summary
;;; 
;;; Commentary:
;;; A set of configurations to make Emacs behave more like webstorm so that Ihsan
;;; feels more at home :)
;;;
;;; We currently do not include project management features
;;; 
;;; Code:

(use-package indium
  ;; A javascript ide and repl
  ;; This will pull in js2 as well
  :ensure t)

(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package company-tern
  :ensure t
  :init
  (add-to-list 'company-backends 'company-tern))

;;; We also want paredit and rainbow-delimiters to load
(add-hook 'js2-mode-hook 'company-mode)
(add-hook 'js2-mode-hook 'paredit-mode)
(add-hook 'js2-mode-hook 'rainbow-delimiters-mode)

(provide 'web)
;;; web.el ends here
