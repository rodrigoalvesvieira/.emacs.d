;;; package --- Summary
;;; Commentary:

;;; Code:
(global-set-key "\M-(" (lambda () (interactive) (insert "{")))
(global-set-key "\M-)" (lambda () (interactive) (insert "}")))

(global-set-key "\M-8" (lambda () (interactive) (insert "[")))
(global-set-key "\M-9" (lambda () (interactive) (insert "]")))

(global-set-key [s-backspace] 'kill-whole-line)

(provide 'key-bindings)
;;; key-bindings.el ends here
