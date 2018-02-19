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
;; (defun +workspace/me/new (name)
;;     (interactive "sEnter workspace name: ")
;;     (+workspace/new name)
;; )

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
;; (defun doom/jump-to-last-workspace ()
;;   "Open the previously selected workspace, if it exists."
;;   (interactive)
;;   (unless (eq 'non-existent
;;               (gethash doom-last-selected-workspace
;;                        *persp-hash* 'non-existent))
;;     (persp-switch doom-last-selected-workspace)))

;;;###autoload
(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)
))

;; print out xpath and copy to clipboard
;;;###autoload
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a
path. from http://www.emacswiki.org/emacs/NxmlMode"
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while
            (and (< (point-min) (point)) ;; Doesn't error if point is at
                                         ;; beginning of buffer
                 (condition-case nil
                     (progn
                       (nxml-backward-up-element) ; always returns nil
                       t)
                   (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (progn
                (message "/%s" (mapconcat 'identity path "/"))
                (kill-new (format "/%s" (mapconcat 'identity path "/"))))
          (format "/%s" (mapconcat 'identity path "/")))))))

;;;###autoload
(defun me/A ()
  "open the corresponding header/cpp file, like A plugin in vim"
  (interactive)
  (split-window-right-and-focus)
  (ff-find-other-file)
)

;;;###autoload
(defun me/a ()
  "open the corresponding header/cpp file in current buffer"
  (interactive)
  (ff-find-other-file)
)

;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
;;;###autoload
(defun my-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let* ((collection (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions))))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        ;; compatible with ivy 9+ and ivy 8
                        (unless (string-match-p "^[a-z0-9]*$" (car rev))
                          (setq rev (cdr rev)))
                        (git-timemachine-show-revision rev)))))

;;;###autoload
(defun my-git-timemachine ()
  "Open git snapshot with the selected version.  Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'my-git-timemachine-show-selected-revision))

;; http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
;;;###autoload
(defun my-reshape-git-gutter (gutter)
  "Re-shape gutter for `ivy-read'."
  (let* ((linenum-start (aref gutter 3))
         (linenum-end (aref gutter 4))
         (target-line "")
         (target-linenum 1)
         (tmp-line "")
         (max-line-length 0))
    (save-excursion
      (while (<= linenum-start linenum-end)
        (goto-line linenum-start)
        (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                 (buffer-substring (line-beginning-position)
                                                                   (line-end-position))))
        (when (> (length tmp-line) max-line-length)
          (setq target-linenum linenum-start)
          (setq target-line tmp-line)
          (setq max-line-length (length tmp-line)))

        (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1)) "-" "+")
                  target-linenum target-line)
          target-linenum)))

;;;###autoload
(defun my-goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (ivy-read "git-gutters:"
                (mapcar 'my-reshape-git-gutter git-gutter:diffinfos)
                :action (lambda (e)
                          ;; ivy9+ keep `(car e)'
                          ;; ivy8- strip the `(car e)'
                          ;; we handle both data structure
                          (unless (numberp e) (setq e (cdr e)))
                          (goto-line e)))
    (message "NO git-gutters!")))

;; http://blog.binchen.org/posts/complete-line-with-ivy-mode.html
;;;###autoload
(defun counsel-escape (keyword)
  (setq keyword (replace-regexp-in-string "\\$" "\\\\\$" keyword))
  (replace-regexp-in-string "\"" "\\\\\"" keyword))

;;;###autoload
(defun counsel-replace-current-line (leading-spaces content)
  (beginning-of-line)
  (kill-line)
  (insert (concat leading-spaces content))
  (end-of-line))

;;;###autoload
(defun counsel-git-grep-complete-line ()
  (interactive)
  (let* (cmd
        (cur-line (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position)))
        (default-directory (locate-dominating-file
                            default-directory ".git"))
        keyword
        (leading-spaces "")
        collection)
    (setq keyword (counsel-escape (if (region-active-p)
                                      (buffer-substring-no-properties (region-beginning)
                                                                      (region-end))
                                    (replace-regexp-in-string "^[ \t]*" "" cur-line))))

    ;; grep lines without leading/trailing spaces
    (setq cmd (format "git --no-pager grep -I -h --no-color -i -e \"^[ \\t]*%s\" | sed s\"\/^[ \\t]*\/\/\" | sed s\"\/[ \\t]*$\/\/\" | sort | uniq" keyword))
    (when (setq collection (split-string (shell-command-to-string cmd) "\n" t))
      (if (string-match "^\\([ \t]*\\)" cur-line)
          (setq leading-spaces (match-string 1 cur-line)))
      (cond
       ((= 1 (length collection))
        (counsel-replace-current-line leading-spaces (car collection)))
       ((> (length collection) 1)
        (ivy-read "lines:"
                  collection
                  :action (lambda (l)
                            (counsel-replace-current-line leading-spaces l))))))
    ))
;; (global-set-key (kbd "C-x C-l") 'counsel-git-grep-complete-line)

;; http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
;;;###autoload
(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Bash history:") collection))))
        (kill-new val)
        (message "%s => kill-ring" val))))

;;;###autoload
(defun counsel-yank-zsh-history ()
    "Yank the zsh history"
    (interactive)
    (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
    (nreverse
    (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
    (buffer-string))
    "\n"
    t)))

    (setq collection (mapcar (lambda (it) (replace-regexp-in-string ".*;" "" it)) collection)) ;; for zsh

    (when (and collection (> (length collection) 0)
    (setq val (if (= 1 (length collection)) (car collection)
    (ivy-read (format "Zsh history:") collection))))
    ;; (setq val (replace-regexp-in-string "^:[^;]*;" "" val))
    ;; (setq val (replace-regexp-in-string ".*;" "" val))
    (kill-new val)
(message "%s => kill-ring" val))))

;;;###autoload
(defun me/create-org-table-from-clipboard ()
  (interactive)
  (let* ((buf (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer) nil t)
      (insert (get-kill-ring))
      (mark-whole-buffer)
      (org-table-create-or-convert-from-region nil)
      (org-table-insert-hline)
      (goto-char (point-min))
      (open-line 1)
      (insert "#+tblname:")
      (append-to-buffer buf (point-min) (point-max))
      )))

;;;###autoload
(defun search-current-line(pattern)
  "search current line using pattern, return captured group, should
   be only 1."
  (interactive)
  (re-search-forward pattern (line-end-position))
  (match-string 1)
)

;;;###autoload
(defun extract-from-org-heading (pattern)
  (interactive)
  "go to top level heading from current position and try to extract
   using pattern"
  (let ((result nil))
    (save-excursion
      (while (org-up-heading-safe))
      (setq result (search-current-line pattern))
      (kill-new result))
      (if result
        (message "saved to kill-ring: %s" result)
        (message "no match found!"))
))
