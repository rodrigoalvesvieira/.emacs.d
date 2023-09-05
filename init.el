;;; package --- Summary
;;; Commentary:

;;; Code:
(setq inhibit-startup-message t)

;;; Packages
(package-initialize)

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :config
  (setq use-package-always-ensure t))

;;; Cursor style
(setq-default cursor-type '(bar . 10))

;;; Line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Autoparentheses
(electric-pair-mode 1)

;;; Disabling things
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;;; Font style
(set-frame-font "Berkeley Mono 14" nil t)
(let ((font "Berkeley Mono 14"))
  (set-frame-font font)
  (add-to-list 'default-frame-alist
               `(font . ,font)))

(global-visual-line-mode t)

;;; Window style
(add-to-list 'default-frame-alist '(height . 44))
(add-to-list 'default-frame-alist '(width . 172))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;;;(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;;; THEME SETUP
;;; PACKAGE: robin-hood
 (load-theme 'robin-hood t t)
 (enable-theme 'robin-hood)

;;; Trailing Whitespaces
(setq require-final-newline t)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6a86067ea3608f4a53f25d11c2fc585677202bcd9cfd75090b90ad798a1c9605" default))
 '(package-selected-packages
   '(orderless marginalia green-phosphor-theme smartscan org-mode speed-type jsonnet-mode yaml-mode tuareg org-sticky-header org-sticky-header-mode julia-mode elpy py-autopep8 goflymake go-flymake ## which-key dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode git-gutter origami auto-complete editorconfig markdown-mode gitignore-mode gitginore gitginore-mode gitconfig-mode go-mode org-bullets poet-theme poet magit-find-file company-mode magit ido-completing-read+ ido-ubiquitous-mod ein fira-code-mode q-mode flycheck ample-theme use-package projectile parinfer cyberpunk-theme ampc ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "~/.emacs.d/key-bindings")
(load "~/.emacs.d/ido-preferences")
(load "~/.emacs.d/bells")
(load "~/.emacs.d/backups")
(load "~/.emacs.d/packages")

(add-to-list 'auto-mode-alist '("\\.cls\\'" . java-mode))

(setq byte-compile-warnings '(cl-functions))

;; C-u C-c C-q
(setq org-tags-column 148)

(setq org-image-actual-width nil)

(provide 'init)
;;; init.el ends here
