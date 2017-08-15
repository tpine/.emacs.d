;;; init.el --- Summary

;;; Commentary:
;;; Entry Point for Emacs customization

;;; Code:
;;; Custom Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(package-selected-packages
   (quote
    (notmuch org slime wanderlust nyan-mode markdown-mode markdown-mode+ markdown-preview-eww stumpwm-mode magit flycheck flycheck-cython flycheck-pyflakes flycheck-rust powerline spaceline auctex auctex-latexmk auctex-lua web-mode rust-mode rainbow-delimiters polymode paredit inflections fill-column-indicator enh-ruby-mode autopair)))
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight bold :height 98 :width normal)))))


;;;  Extra Package Repositorys

;; MELPA
;; Add package repositorys to emacs
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;; Setup use-package so it is always installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; Utility Functions
(defun add-hooks (fn hooks)
  "Call 'add-hook' adding FN as hook for all HOOKS."
  (dolist (hook hooks)
    (add-hook hook fn)))

;;; Further config is here

;;; User Interface changes
(load-file "~/.emacs.d/ui.el")
;;; Global changes to emacs
(load-file "~/.emacs.d/global.el")
;;; Lisp Config
(load-file "~/.emacs.d/lisp.el")
;;; Latex Config
(load-file "~/.emacs.d/latex.el")
;;; org-mode config
(load-file "~/.emacs.d/org.el")
;;; email config
(load-file "~/.emacs.d/email.el")
;;; web development config
(load-file "~/.emacs.d/web.el")

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(provide 'init.el)
;;; init.el ends here
