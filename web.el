;;; web.el --- Summary
;;; 
;;; Commentary:
;;; A set of configurations to make Emacs behave more like webstorm so that Ihsan
;;; feels more at home :)
;;;
;;; We currently do not include project management features
;;; 
;;; Code:
(require 'cl-lib)

(defun beginning-of-string ()
  "Moves to the beginning of a syntactic string"
  (interactive)
  (unless (in-string-p)
    (error "You must be in a string for this command to work"))
  (while (in-string-p)
    (forward-char -1))
  (point))

(defun swap-quotes-to-template ()
  "Swaps the quote symbols in a string"
  (interactive)
  (save-excursion
    (let ((bos (save-excursion
                 (beginning-of-string)))
          (eos (save-excursion
                 (beginning-of-string)
                 (forward-sexp)
                 (point)))
          (replacement-char ?\'))
      (goto-char bos)
      ;; if the following character is a single quote then the
      ;; `replacement-char' should be a double quote.
      (when (or (eq (following-char) ?\') (eq (following-char) ?\"))
          (setq replacement-char ?`))
      (delete-char 1)
      (insert replacement-char)
      (goto-char eos)
      (delete-char -1)
      (insert replacement-char))))

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
  :mode ("\\.js\\'" . js2-mode) 
  :bind (("C-c p" . php-mode)
	 ("C-c w" . web-mode)
	 ("C-c t" . swap-quotes-to-template))
  :hook ((js2-mode . company-mode)
	 (js2-mode . rainbow-delimiters-mode)
	 (js2-mode . (lambda () (setq indent-tabs-mode nil))))
  :config (setq js2f-mode-show-parse-errors nil
		js2-mode-show-strict-warnings nil
		flycheck-javascript-standard-executable "semistandard"))

(use-package js2-refactor
  :ensure t
  :hook ((js2-mode . js2-refactor-mode))
  :init
  (progn
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (define-key js2-mode-map (kbd "C-k") #'js2r-kill)))

(use-package xref-js2
  :ensure t
  :hook ((js2-mode . (lambda ()
		       (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
  :init
  (progn
    (define-key js-mode-map (kbd "M-.") nil)))

(use-package company-tern
  :ensure t
  :init
  (add-to-list 'company-backends 'company-tern))

(use-package rjsx-mode
  :ensure t
  :mode ("\\.tsx\\'" . rjsx-mode))

;; formats the buffer before saving
(cl-flet ((setup-tide-mode ()
			   (tide-setup)
			   (flycheck-mode +1)
			   (setq flycheck-check-syntax-automatically '(save mode-enabled))
			   (rainbow-delimiters-mode)
			   (eldoc-mode +1)
			   (tide-hl-identifier-mode +1)
			   (company-mode +1)))
  (use-package tide
    :ensure t
    :after (typescript-mode company flycheck)
    :mode ("\\.tsx\\'" . web-mode)
    :bind (("C-c r" . tide-rename-symbol))
    :hook ((typescript-mode-hook . setup-tide-mode)
	   (before-save . tide-format-before-save)
	   (web-mode-hook . (lambda ()
			      (when (string-equal "tsx" (file-name-extension buffer-file-name))
				(setup-tide-mode)))))))

(use-package php-mode
  :ensure t
  :bind
  (("C-c w" . web-mode)
   ("C-c j" . js2-mode))
  :init
  (progn
     (ac-php-core-eldoc-setup)
     (make-local-variable 'company-backends)
     (add-to-list 'company-backends 'company-ac-php-backend)
     (add-hook 'php-mode-hook (lambda () (setq indent-tabs-mode nil)))))

(use-package ac-php :ensure t)
(use-package company-php :ensure t)

(use-package web-mode
  :ensure t
  :bind
  (("C-c p" . php-mode)
   ("C-c j" . js2-mode))
  :mode (("\\.ctp\\'" . web-mode)
	 ("\\.html\\'" . web-mode)
	 ("\\.twig\\'" . web-mode)
	 ("\\.styl\\'" . web-mode))
  :hook ((web-mode . (lambda ()
			(setq web-mode-enable-auto-pairing nil))))
  :init
  (progn
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
