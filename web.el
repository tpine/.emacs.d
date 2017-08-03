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

(use-package smartparens
  :ensure t
  :init
  (use-package smartparens-config)
  (smartparens-global-mode 1))

(use-package js2-mode
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook 'company-mode)
    (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)))

(use-package company-tern
  :ensure t
  :init
  (add-to-list 'company-backends 'company-tern))

(provide 'web)
;;; web.el ends here
