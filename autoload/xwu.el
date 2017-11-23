;;; private/xwu157/autoload/+me.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +xwu/install-snippets ()
  "Install my snippets from https://github.com/xwu157/emacs-snippets into
private/xwu157/snippets."
  (interactive)
  (doom-fetch :github "xwu/emacs-snippets"
              (expand-file-name "snippets" (doom-module-path :private 'hlissner))))

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

;;;###autoload (autoload '+xwu/find-in-templates "private/xwu157/autoload/xwu" nil t)
;;;###autoload (autoload '+xwu/browse-templates "private/xwu157/autoload/xwu" nil t)
(+hlissner-def-finder! templates +file-templates-dir)

;;;###autoload (autoload '+xwu/find-in-snippets "private/xwu157/autoload/xwu" nil t)
;;;###autoload (autoload '+xwu/browse-snippets "private/xwu157/autoload/xwu" nil t)
(+hlissner-def-finder! snippets +xwu-snippets-dir)

;;;###autoload (autoload '+xwu/find-in-dotfiles "private/xwu157/autoload/xwu" nil t)
;;;###autoload (autoload '+xwu/browse-dotfiles "private/xwu157/autoload/xwu" nil t)
(+hlissner-def-finder! dotfiles (expand-file-name "dotfiles" "~"))

;;;###autoload (autoload '+xwu/find-in-emacsd "private/xwu157/autoload/xwu" nil t)
;;;###autoload (autoload '+xwu/browse-emacsd "private/xwu157/autoload/xwu" nil t)
(+hlissner-def-finder! emacsd doom-emacs-dir)

;;;###autoload (autoload '+xwu/find-in-notes "private/xwu157/autoload/xwu" nil t)
;;;###autoload (autoload '+xwu/browse-notes "private/xwu157/autoload/xwu" nil t)
(+hlissner-def-finder! notes +org-dir)
