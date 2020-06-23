;;; package --- Summary
;;; Commentary:

;;; Code:

(defun bw/add-to-load-path (dir)
  "Add `DIR' to 'load-path'."
  (add-to-list 'load-path dir))

(defun bw/join-dirs (prefix suffix)
  "Join `PREFIX' and `SUFFIX' into a directory."
  (file-name-as-directory (concat prefix suffix)))

(defconst bw/dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customized Emacs configuration.")

(make-directory
 (setq bw/tmp-local-dir
       (bw/join-dirs bw/dotfiles-dir ".tmp"))
 t)

(make-directory
 (setq bw/tmp-backups-dir
       (bw/join-dirs bw/tmp-local-dir "backups"))
 t)

(make-directory
 (setq bw/tmp-autosaves-dir
       (bw/join-dirs bw/tmp-local-dir "autosaves"))
 t)

(setq backup-directory-alist `((".*" . ,bw/tmp-backups-dir))
      auto-save-file-name-transforms `((".*" ,bw/tmp-autosaves-dir)))

(setq backup-by-copying t)

(setq delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2)

(setq version-control t)

(provide 'backups)
;;; backups.el ends here
