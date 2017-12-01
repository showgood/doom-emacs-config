;;; private/xwu157/autoload/+me.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +xwu/install-snippets ()
  "Install my snippets from https://github.com/showgood/emacs-snippets into
private/user-login-name/snippets."
  (interactive)
  (doom-fetch :github "showgood/emacs-snippets"
              (expand-file-name "snippets" (doom-module-path :private user-login-name ))))

(defmacro +hlissner-def-finder! (name dir)
  "Define a pair of find-file and browse functions."
  `(progn
     (defun ,(intern (format "+xwu/find-in-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir)
             projectile-project-name
             projectile-require-project-root
             projectile-cached-buffer-file-name
             projectile-cached-project-root)
         (call-interactively (command-remapping #'projectile-find-file))))
     (defun ,(intern (format "+xwu/browse-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'find-file))))))

;;;###autoload (autoload '+xwu/find-in-templates (format "private/%s/autoload/xwu" user-login-name) nil t)
;;;###autoload (autoload '+xwu/browse-templates (format "private/%s/autoload/xwu" user-login-name) nil t)
(+hlissner-def-finder! templates +file-templates-dir)

;;;###autoload (autoload '+xwu/find-in-snippets (format "private/%s/autoload/xwu" user-login-name) nil t)
;;;###autoload (autoload '+xwu/browse-snippets (format "private/%s/autoload/xwu" user-login-name) nil t)
(+hlissner-def-finder! snippets +xwu-snippets-dir)

;;;###autoload (autoload '+xwu/find-in-dotfiles (format "private/%s/autoload/xwu" user-login-name) nil t)
;;;###autoload (autoload '+xwu/browse-dotfiles (format "private/%s/autoload/xwu" user-login-name) nil t)
(+hlissner-def-finder! dotfiles (expand-file-name "dotfiles" "~"))

;;;###autoload (autoload '+xwu/find-in-emacsd (format "private/%s/autoload/xwu" user-login-name) nil t)
;;;###autoload (autoload '+xwu/browse-emacsd (format "private/%s/autoload/xwu" user-login-name) nil t)
;; (+hlissner-def-finder! emacsd doom-emacs-dir)
(+hlissner-def-finder! emacsd +xwu-dir)

;;;###autoload (autoload '+xwu/find-in-notes (format "private/%s/autoload/xwu" user-login-name) nil t)
;;;###autoload (autoload '+xwu/browse-notes (format "private/%s/autoload/xwu" user-login-name) nil t)
(+hlissner-def-finder! notes +org-dir)
