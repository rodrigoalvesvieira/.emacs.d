;;; package --- Summary
;;; Commentary:

;;; Code:

;;; PACKAGE: magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit)))

(use-package magit-find-file
  :ensure t
  :config
  (global-set-key (kbd "C-c p") 'magit-find-file-completing-read))

;;; PACKAGE: gitconfig-mode
(use-package gitconfig-mode
  :ensure t)

;;; PACKAGE: gitignore-mode
(use-package gitignore-mode
  :ensure t)

;;; PACKAGE: markdown-mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; PACKAGE: ag.el
(use-package ag
  :ensure t)

;;; PACKAGE: company
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;; PACKAGE: dimmer.el
(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-mode t))

;;; PACKAGE: moody
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;; PACKAGE: emacs-jupyter
(use-package jupyter
  :ensure t)

;;; PACKAGE: emacs-ipython-notebook
(use-package ein
  :ensure t)

;;; PACKAGE: EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; PACKAGE: Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; PACKAGE: ido-completing-read+
(use-package ido-completing-read+
  :ensure t
  :init (require 'ido-completing-read+)
  (ido-ubiquitous-mode 1))

;;; PACKAGE: q-mode
(use-package q-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.[kq]\\'" . q-mode)))

;;; PACKAGE: fira-code-mode
(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

;;; PACKAGE: org-bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; PACKAGE: go-mode
(use-package go-mode
  :ensure t
  :config (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(provide 'packages)
;;; packages.el ends here
