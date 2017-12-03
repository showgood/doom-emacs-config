;; I don't need linenum most of the time
;; hopefully can speed up a bit for large file
(setq doom-line-numbers-style nil)

(load! +bindings)  ; my key bindings
(load! +myorg)  ; org configs
(load! +alias)  ; emacs alias
(load! +bb)  ; bb specific
(load! +commands)  ; my custom ex commands
(load! +myabbrev)

(require 'vlf)
(require 'vlf-setup)

(setq +org-dir (concat (substitute-in-file-name "$HOME/") "org"))

(defvar +xwu-dir (file-name-directory load-file-name))
(defvar +xwu-snippets-dir (expand-file-name "snippets/" +xwu-dir))

(setq epa-file-encrypt-to user-mail-address
      auth-sources (list (expand-file-name ".authinfo.gpg" +xwu-dir))
      +doom-modeline-buffer-file-name-style 'relative-from-project)

(defun +hlissner*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+hlissner*no-authinfo-for-tramp)

(after! smartparens
  ;; Auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

(after! evil-mc
  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;; Don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs
        (append (list '+xwu-snippets-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))

;; very important to me
(setq-default evil-escape-key-sequence "jf")
(setq bookmark-default-file (concat (substitute-in-file-name "$HOME/") "bookmarks"))
(setq bookmark-file (concat (substitute-in-file-name "$HOME/") "bookmarks"))

;; settings needed for irony-mode, disabled it since it cause slowness
;; (setq irony-server-install-prefix "~/tools/irony-server")
;; (setq irony-cdb-search-directory-list '("." "src" "build"))
;; (setenv "LD_LIBRARY_PATH" "/opt/bb/lib/llvm-5.0/lib64")

;; (setq debug-on-error t)

(require 'bookmark+)
;; fix the error that bmkp-info-cp is void
(defalias 'bmkp-info-cp 'bmkp-info-node-name-cp)

;; (set-frame-font "Fira Code:pixelsize=16:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")

;; those settings are useful, but already set by doom-core
;; keep here for future note
;; http://sachachua.com/blog/2017/04/emacs-pasting-with-the-mouse-without-moving-the-point-mouse-yank-at-point/
;; (setq mouse-yank-at-point t)
;; Save whatever’s in the current (system) clipboard before replacing it with the Emacs’ text.
;; (setq save-interprogram-paste-before-kill t)

;; It’s much easier to move around lines based on how they are
;; displayed, rather than the actual line. This helps a ton with
;; long log file lines that may be wrapped:
(setq line-move-visual t)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq display-time-world-list
        '(("America/New_York" "New York")
          ("Asia/Shanghai" "Shanghai")
          ("Australia/Sydney" "Sydney")
          ("Europe/London" "London")
          ("Europe/Berlin" "Germany")
          ("Europe/Rome" "Italy")
          ("Europe/Paris" "Paris")))

;; quick way to dispaly world time clock
(defalias 'wc 'display-time-world)

(require 'eacl)

;;-------------------------------
;; rtags related settings
;;-------------------------------
(require 'rtags)
(setq rtags-socket-file (concat (substitute-in-file-name "$HOME/") ".rdm"))
;; (setq rtags-path "/opt/bb/bin")
;; (setq rtags-completions-enabled t)

;; show autocomplete popup, disable by default
;; disable it since it's too slow
(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

;; (require 'company-rtags)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)

;;-------------------------------
;; flycheck related settings
;;-------------------------------
;; (setq flycheck-c/c++-clang-executable "/opt/bb/bin/clang++")
(setq flycheck-c/c++-clang-executable "/usr/local/opt/llvm/bin/clang++")
(setq flycheck-clang-args '("-m32" "-Dlint" "-D_REENTRANT"
      "-D_THREAD_SAFE" "-DBB_THREADED" "-DBSL_OVERRIDES_STD"))

(defun my-flycheck-setup ()
  (flycheck-select-checker 'c/c++-clang))
(add-hook 'c-mode-common-hook #'my-flycheck-setup)

; this does not work, not sure why
;; (require 'flycheck-rtags)
;; ;; http://syamajala.github.io/c-ide.html
;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (add-hook 'c-mode-hook 'flycheck-mode)

;; (defun my-flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-check-syntax-automatically nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

(defun bb-c-mode ()
  (interactive)
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (modify-syntax-entry ?_ "w")
  (c-set-offset 'innamespace 0)
)

(add-hook 'c-mode-common-hook 'bb-c-mode)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(setq eshell-aliases-file (concat +xwu-dir "eshell_alias"))

(fset 'evil-visual-update-x-selection 'ignore)

;; support large file size
(setq tramp-inline-compress-start-size 10000000)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Password-handling.html
;; store the password for a period of time, helpful in the TRAMP case
(setq password-cache-expiry nil)

;; http://emacs.stackexchange.com/questions/15208/using-tramp-for-logs?rq=1
;; auto-revert-tail-mode is great, but it has its limits. Therefore
;; I prefer to use an asynchronous shell command. Open the remote
;; directory in dired, position the cursor to the file you want to
;; watch, and apply ! tail -f * &.

(setq ivy-count-format "(%d/%d) ")
;; http://oremacs.com/2017/04/09/ivy-0.9.0/
(setq counsel-yank-pop-separator "\n-------------------------------------------------------\n")
(setq counsel-bookmark-avoid-dired nil)

;; http://oremacs.com/2017/11/30/ivy-0.10.0/
(setq ivy-use-selectable-prompt t)

(load! toolkit-tramp)
(require 'toolkit-tramp)

;; http://emacs.stackexchange.com/questions/27/how-can-i-use-my-local-emacs-client-as-the-editor-for-remote-machines-i-access
;; (require 'with-editor)

;; (add-hook 'shell-mode-hook  'with-editor-export-editor)
;; (add-hook 'term-mode-hook   'with-editor-export-editor)
;; (add-hook 'eshell-mode-hook 'with-editor-export-editor)

;; use web-mode instead of nxml for xml
(add-to-list 'auto-mode-alist '("\\.xml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$" . web-mode))

(add-hook  'vc-dir-mode-hook
             (lambda ()
               ;; Hide up-to-date and unregistered files.
               (define-key vc-dir-mode-map
                 (kbd "x") #'leuven-vc-dir-hide-up-to-date-and-unregistered)
               (define-key vc-dir-mode-map
                 (kbd "E") #'vc-ediff)
               (define-key vc-dir-mode-map
                 (kbd "#") #'vc-ediff-ignore-whitespace)
                                         ; ediff-windows-wordwise?
               ))

(add-hook  'vc-svn-log-view-mode-hook
           (lambda ()
             (define-key vc-svn-log-view-mode-map
               (kbd "E") #'vc-ediff)
             (define-key vc-svn-log-view-mode-map
               (kbd "#") #'vc-ediff-ignore-whitespace)
                                        ; ediff-windows-wordwise?
             ))

;; https://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

; ignore white space
(csetq ediff-diff-options "-w")

;; http://oremacs.com/2015/01/13/dired-options/
;;http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
;; not working for mac
;; (setq dired-listing-switches "-lah")

;; this --group-directories-first doesn't work on mac os natively,
;; also -G option doesn't work
;; (setq dired-listing-switches "-laGh1v --group-directories-first")
(setq dired-recursive-deletes 'always)

;; when using find-dired, also list the result with size etc
;; (setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))
(setq find-ls-option '("-print0 | xargs -0 ls -alh" . ""))

;;http://irreal.org/blog/?p=3341
;; display file details for dired
;; this needs to happen before loading dired+
(setq diredp-hide-details-initially-flag nil)

;; delete file permanently, do not move to trash bin
(setq delete-by-moving-to-trash nil)

;; https://www.reddit.com/r/emacs/comments/1493oa/emacsmovies_season_2_dired/
;; Make df output in dired buffers easier to read
(setq dired-free-space-args "-Pm")

;; Try suggesting dired targets
(setq dired-dwim-target t)

;; Understand .zip the way it does tarballs, letting the Z key decompress it:
;; Handle zip compression
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

;; maximize emacs upon startup
(toggle-frame-maximized)

;; set this so search is performed on all buffers,
;; not just current buffer
(setq avy-all-windows t)

(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

;; allow to select from kill-ring history while in minibuffer
(setq enable-recursive-minibuffers t)

(defvar doom-default-workspace-name "main"
  " Name of the default layout.")

(defvar doom-last-selected-workspace doom-default-workspace-name
  "Previously selected layout.")

(defun +workspace/save-name(name frame)
  (setq doom-last-selected-workspace persp-last-persp-name)
  (message (format "persp-last: %s" persp-last-persp-name))
)

(add-hook 'persp-before-switch-functions #'+workspace/save-name)

(defun setup-my-term-mode()
  (setq-local global-hl-line-mode nil)
  ;; (term-line-mode)
)

(add-hook 'term-mode-hook #'setup-my-term-mode)
;; (add-hook 'evil-visual-state-exit-hook #'hl-line-mode)
