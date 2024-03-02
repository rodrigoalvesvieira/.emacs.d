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

;;; PACKAGE: origami
(use-package origami
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


;;; PACKAGE: EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; PACKAGE: Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; PACKAGE: auto-complete
(use-package auto-complete
  :ensure t
  :init (ac-config-default))

;;; PACKAGE: ido
(use-package ido
  :disabled
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1))

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

;;; PACKAGE: org-bullets
;;;(use-package org-bullets
;;;  :ensure t
;;;  :config
;;;  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; PACKAGE: org-side-tree
(use-package org-side-tree
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local outline-regexp ";;;\\(;* [^   \t\n]\\)")))
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))

;;; PACKAGE: elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;;; PACKAGE: py-autopep8
(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

;;; PACKAGE: tuareg (OCaml mode)
(use-package tuareg
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook
	    ;; Turn on auto-fill minor mode.
	    #'auto-fill-mode))

;;; PACKAGE: go-mode
(use-package go-mode
  :ensure t
  :config
  (autoload 'go-mode "go-mode" nil t)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;;; PACKAGE: yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;; PACKAGE: jsonnet-mode
(use-package jsonnet-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsonnet\\'" . jsonnet-mode)))

(setq lsp-keymap-prefix "s-l")

;;; PACKAGE: lsp-mode
(use-package lsp-mode
  :hook ((go-mode ruby-mode Z-mode) . lsp-deferred)
  :commands lsp)

;;; PACKAGE: lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

;;; PACKAGE: helm-lsp
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;;; PACKAGE: lsp-ivy
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;;; PACKAGE: lsp-treemacs
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;;; PACKAGE: dap-mode
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;;; PACKAGE: which-key
(use-package which-key
    :config
    (which-key-mode))

;;; PACKAGE: speed-type
(use-package speed-type
  :ensure t
  :config)

(provide 'packages)
;;; packages.el ends here

(use-package smartscan
  :ensure t)

;;; Enable font ligatures
(when (window-system)
  (set-frame-font "JetBrains Mono"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))


;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
