;;; Commentary:
;;; Entry Point for Emacs customization

;;; Code:

;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))
;; Add Line numbers onto a sidebar
(global-display-line-numbers-mode)

;; Backup File Directory
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;; Load Custom Set Variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defun internet-up-p (&optional host)
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" 
		     (if host host "www.google.com"))))

;;; Themeing
(use-package doom-themes
  :ensure t
  :init
  (progn 
    (require 'doom-themes)

    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	  doom-themes-enable-italic t
	  doom-vibrant-brighter-modeline nil
	  org-hide-leading-stars nil) ; if nil, italics is universally disabled
    (load-theme 'doom-gruvbox t)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(use-package nyan-mode
  :ensure t
  :init
  ;; Fix up Nyan Cat cause she's pretty
  (setq nyan-animate-nyancat t
	nyan-wavy-trail t)
  (nyan-mode))

;;; Rainbow Delimeters
(use-package rainbow-delimiters
  :ensure t
  :hook (rustic-mode
	 terraform-mode
	 emacs-lisp-mode))

;;; Dired
(use-package dired-narrow
  :ensure t
  :config
  (bind-keys :map dired-mode-map
	     ("f" . dired-narrow-fuzzy)))

;;; Undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

;;; Expand Region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))

;;; Ivy and Consel
(use-package ivy
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-S-s" . isearch-forward))
  :diminish ivy-mode
  :init (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("C-c g" . counsel-rg)))

;;; SmartParens
(use-package smartparens-mode
  :ensure smartparens  ;; install the package
  :hook (prog-mode
	 text-mode
	 markdown-mode
	 rustic-mode
	 terraform-mode) ;; add `smartparens-mode` to these hooks
  :bind ("C-<left>" . sp-forward-slurp-sexp)
  :config
  ;; load default config
  (require 'smartparens-config))

;;; Magit
(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

;;; Rust
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

;;; Terraform
(use-package terraform-mode
  :ensure t)

;;; web-mode
(use-package web-mode
  :ensure t
  :mode (".svelte$"))

;;; yaml
(use-package yaml-ts-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;;; Prettier
(use-package prettier-js
  :ensure t
  :mode (("\\.tsx\\'" . prettier-js-mode)
	 ("\\.json\\'" . prettier-js-mode)))

(use-package tsx-ts-mode
  :mode "\\.tsx\\'")

;;; Lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((terraform-mode . lsp)
	 (tsx-ts-mode . lsp))
  :magic (".svelte$" . lsp)
  :commands lsp)

(use-package lsp-ui
  :ensure t)

(provide 'init.el)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
