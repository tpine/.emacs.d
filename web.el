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
  :bind
  (("C-c p" . php-mode)
   ("C-c w" . web-mode))
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook 'company-mode)
    (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'js2-mode-hook (lambda () (setq indent-tabs-mode nil))))
  :config
  (progn
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (setq flycheck-javascript-standard-executable "semistandard")))

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

(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (rainbow-delimiters-mode)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; formats the buffer before saving

(use-package tide
  :ensure t
  :init
  (progn
    ;; (add-hook 'before-save-hook 'tide-format-before-save)
    ;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-hook 'typescript-mode-hook 'setup-tide-mode)
    
    (add-hook 'web-mode-hook
              (lambda ()
		(when (string-equal "tsx" (file-name-extension buffer-file-name))
		  (setup-tide-mode))))))

(use-package php-mode
  :ensure t
  :bind
  (("C-c w" . web-mode)
   ("C-c j" . js2-mode))
  :init
  (progn
    (use-package ac-php
      :ensure t)
    (use-package company-php
      :ensure t)
     (ac-php-core-eldoc-setup)
     (make-local-variable 'company-backends)
     (add-to-list 'company-backends 'company-ac-php-backend)
     (add-hook 'php-mode-hook (lambda () (setq indent-tabs-mode nil)))))

(use-package web-mode
  :ensure t
  :bind
  (("C-c p" . php-mode)
   ("C-c j" . js2-mode))
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.styl\\'" . web-mode))
    (add-hook 'web-mode-hook (lambda ()
			       (setq web-mode-enable-auto-pairing nil)))

    (defun sp-web-mode-is-code-context (id action context)
      (and (eq action 'insert)
	   (not (or (get-text-property (point) 'part-side)
		    (get-text-property (point) 'block-side)))))

    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))))

(use-package css-mode
  :init (setf flycheck-scss-stylelint-executable "stylelint --config stylelint-config-recommended-scss"))

(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb$")

(use-package rvm
  :ensure t)

(use-package robe
  :ensure t
  :hook enh-mode-hook
  :init
  (progn
    (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
      (rvm-activate-corresponding-ruby)))
  :config (robe-start))

(provide 'web)
;;; web.el ends here
