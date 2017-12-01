;;; private/xwu157/autoload/+funcs.el -*- lexical-binding: t; -*-

;;;###autoload
;; from AbcDef ==> Abc_Def
(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))
(defun underscore-string (s) (mapconcat 'downcase   (split-name s) "_"))

;;;###autoload
(defun run-command-anywhere (command infile)
  (process-file-shell-command command infile
                              (get-buffer-create "*run-cmd-anywhere*"))
  (switch-to-buffer-other-window "*run-cmd-anywhere*")
)

;;soure:https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-clipboard.el
;; only copy file name (not including path)
;;;###autoload
(defun cp-filename-of-current-buffer ()
  "Copy file name (NOT full path) into the yank ring and OS clipboard"
  (interactive)
  (let (filename)
    (when buffer-file-name
      ;; (setq filename (file-name-nondirectory buffer-file-name))
      (setq filename (file-name-nondirectory buffer-file-name))
      (kill-new filename)
      (message "filename %s => clipboard & yank ring" filename))))

;;;###autoload
(defun get-local-file-name (file-name)
  (interactive)
  (if (file-remote-p default-directory)
      (tramp-file-name-localname (tramp-dissect-file-name
                                  (expand-file-name file-name)))
    (expand-file-name file-name))
)

;;;###autoload
(defun xml-reformat()
  "reformat the xml file using xmllint"
  (interactive)

  (shell-command
   (format "xmllint --format %s"
           (shell-quote-argument (buffer-file-name)))

   ;; name of output buffer
   (current-buffer)
   ;; name of the error buffer
   "*XMl reformat Error Buffer*"
))

;;;###autoload
(defun get-kill-ring()
  "get top of kill ring as plain text"
  (interactive)

  (substring-no-properties (current-kill 0))
)

;; https://superuser.com/questions/546619/clear-the-kill-ring-in-emacs
;;;###autoload
(defun clear-kill-ring ()
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect))
  )

;;;###autoload
(defun tramp/xml-reformat()
  "reformat the xml file using xmllint"
  (interactive)

  (run-command-anywhere
   (format "xmllint --format %s"
           (file-name-nondirectory
              (get-local-file-name (buffer-file-name)))) nil))

;; replace current word or selection using vim style for evil mode
;; author: showgood
;;;###autoload
(defun evil-replace-word-selection()
  "replace current word or selection using vim style for evil mode"
  (interactive)
  (if (use-region-p)
      (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
          (message "empty string")
          (evil-ex (concat "'<,'>s/" selection "/"))
        ))
    (evil-ex (concat "%s/" (thing-at-point 'word) "/"))))

;;;###autoload
(defun Open ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-06-12"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "cygwin")
    (w32-shell-execute "explore" default-directory ))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))

;; insert current time
;; source: http://emacswiki.org/emacs/InsertingTodaysDate
;;;###autoload
(defun nnow ()
  (interactive)
  (insert (format-time-string "%Y-%b-%d %H:%M:%S")))

;; insert today date
;;;###autoload
(defun ddate ()
  (interactive)
  (insert (format-time-string "%Y-%b-%d")))

;; quickly dupliate a line without changing the kill-ring
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
;;;###autoload
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  (pop kill-ring)
)

;;;###autoload
(defun +hlissner/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun open-scratch()
  (interactive)
  (doom-popup-buffer (get-buffer-create  "*scratch*")))

;; create an org mode link
;;;###autoload
(defun bbg-link(link-name)
  (interactive "sEnter link name: ")
  (insert (format "[[bbg: %s][%s]]"
                  (get-kill-ring) link-name))
)

;;;###autoload
(defun bbg-bookmark()
  (interactive)
  "create a bookmark for bb function"
  (bmkp-url-target-set (format "bbg://screens/%s" (get-kill-ring))
                       nil (bmkp-completing-read-lax "Bookmark name"))
)

;; from prelude
;;;###autoload
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; +workspace/new does NOT take the name from user input,
;; this solve that issue
;;;###autoload
(defun +workspace/me/new (name)
    (interactive "sEnter workspace name: ")
    (+workspace/new name)
)

;; https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
;;;###autoload
(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

;;;###autoload
(defun doom/jump-to-last-workspace ()
  "Open the previously selected workspace, if it exists."
  (interactive)
  (unless (eq 'non-existent
              (gethash doom-last-selected-workspace
                       *persp-hash* 'non-existent))
    (persp-switch doom-last-selected-workspace)))

;;;###autoload
(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)
))
