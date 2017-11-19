;;; private/xwu157/+myFindFile.el -*- lexical-binding: t; -*-

;; I need this because need to work with svn report over TRAMP.
;; counsel-git only work with git. and projectile takes ages to index the project.
;; used in cygwin case

(autoload 'find-file-in-project "find-file-in-project" nil t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
(autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
(autoload 'ffip-show-diff "find-file-in-project" nil t)
;; (autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
;; (autoload 'ffip-ivy-resume "find-file-in-project" nil t)

(if (eq system-type 'windows-nt)
    (setq ffip-find-executable "c:\\\\cygwin64\\\\bin\\\\find"))

(defun ffip-diff-mode-hook-setup ()
    (evil-local-set-key 'normal "p" 'diff-hunk-prev)
    (evil-local-set-key 'normal "n" 'diff-hunk-next)
    (evil-local-set-key 'normal "P" 'diff-file-prev)
    (evil-local-set-key 'normal "N" 'diff-file-next)
    (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
    (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)
