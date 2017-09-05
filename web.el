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
  :bind (("C-<right>" . sp-forward-slurp-sexp)
	 ("C-<left>" . sp-forward-barf-sexp))
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

(use-package js2-refactor
  :ensure t
  :init
  (progn
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (define-key js2-mode-map (kbd "C-k") #'js2r-kill)))

(use-package xref-js2
  :ensure t
  :init
  (progn
    (define-key js-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook
	      (lambda ()
		(add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

  (use-package company-tern
    :ensure t
    :init
    (add-to-list 'company-backends 'company-tern)))

(use-package php-mode
  :ensure t
  :init
  (progn
    (use-package ac-php
      :ensure t)
    (use-package company-php
      :ensure t)
     (ac-php-core-eldoc-setup)
     (make-local-variable 'company-backends)
     (add-to-list 'company-backends 'company-ac-php-backend)))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode)))

(provide 'web)
;;; web.el ends here
