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
    (php-mode robe inf-ruby rvm forge mark-tools tide xref-js2 js2-refactor js2-mode company-tern elfeed-org elfeed org-plus-contrib sly smartparens ranger multiple-cursors ace-window avy yasnippet undo-tree move-text expand-region company counsel ivy company-terraform terraform-mode ansible ansible-doc ansible-vault editorconfig editorconfig-charset-extras editorconfig-domain-specific docker docker-compose-mode docker-tramp dockerfile-mode airline-themes doom-themes darktooth-theme yasnippet-snippets yasnippets ob-mongo ob-php company-php ac-php notmuch org slime wanderlust nyan-mode markdown-mode markdown-mode+ markdown-preview-eww stumpwm-mode magit flycheck flycheck-cython flycheck-pyflakes flycheck-rust powerline spaceline auctex auctex-latexmk auctex-lua web-mode rust-mode rainbow-delimiters polymode paredit inflections fill-column-indicator enh-ruby-mode autopair)))
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight bold :height 98 :width normal)))))

;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))
(package-initialize)

;;; Bootstrap quelpa
;; Install quelpa if it's not already installed.
;; quelpa is used to configure the rest of the packages.
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://framagit.org/steckerhalter/quelpa/raw/master/bootstrap.el")
    (eval-buffer)))

;;; Load the config
(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))

(provide 'init.el)
;;; init.el ends here
