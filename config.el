;; -*- origami-fold-style: triple-braces -*-

(setq debug-on-error t)
(require 'counsel)
(require 'general)
(general-evil-setup)

;; === NOTE: load org-pdfview before +myorg====
(load! org-pdfview)

(load! +bindings)  ; my key bindings
(load! +myorg)  ; org configs
(load! +alias)  ; emacs alias
(load! +commands)  ; my custom ex commands
(load! +myabbrev)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/private/showgood/evil-collection/"))
(with-eval-after-load 'dired (require 'evil-collection-dired) (evil-collection-dired-setup))

;; ==== general settings {{{ ====
;; it’s much easier to move around lines based on how they are
;; displayed, rather than the actual line. this helps a ton with
;; long log file lines that may be wrapped:
(setq line-move-visual t)

;; i don't need linenum most of the time
;; hopefully can speed up a bit for large file
(setq doom-line-numbers-style nil)

 ; proper line wrapping
(global-visual-line-mode 1)

;; allow to select from kill-ring history while in minibuffer
(setq enable-recursive-minibuffers t)

;; https://writequit.org/articles/working-with-logs-in-emacs.html
(setq auto-revert-tail-mode t)

(setq debug-on-error t)

;; (set-frame-font "fira code:pixelsize=16:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")

;; those settings are useful, but already set by doom-core
;; keep here for future note
;; http://sachachua.com/blog/2017/04/emacs-pasting-with-the-mouse-without-moving-the-point-mouse-yank-at-point/
;; (setq mouse-yank-at-point t)
;; save whatever’s in the current (system) clipboard before replacing it with the emacs’ text.
;; (setq save-interprogram-paste-before-kill t)

;; ==== end general settings }}} ====

;; ==== evil settings {{{ ====
;; very important to me
(setq-default evil-escape-key-sequence "jf")
(fset 'evil-visual-update-x-selection 'ignore)

(evil-add-command-properties #'rtags-find-symbol-at-point :jump t)
(evil-add-command-properties #'rtags-find-references-at-point :jump t)
(evil-add-command-properties #'counsel-imenu :jump t)
;; ==== end evil settings }}} ====

;; ==== frequently used packages {{{ ====
(require 'hl-anything)
(hl-highlight-mode)

(require 'origami)
(global-origami-mode 1)

;; ==== END frequently used packages }}} ====

;; ==== elpy settings {{{ ====
(require 'elpy)
(elpy-enable)
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")

(setq elpy-rpc-python-command "/Users/showgood/anaconda2/bin/python")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; ==== END elpy settings }}} ====

(require 'vlf)
(require 'vlf-setup)

(setq +org-dir (concat (substitute-in-file-name "$HOME/") "org"))

(defvar +xwu-dir (file-name-directory load-file-name))
(defvar +xwu-snippets-dir (expand-file-name "snippets/" +xwu-dir))
(setq epa-file-encrypt-to user-mail-address
      auth-sources (list (expand-file-name ".authinfo.gpg" +xwu-dir))
      +doom-modeline-buffer-file-name-style 'relative-from-project)

(defun +hlissner*no-authinfo-for-tramp (orig-fn &rest args)
  "don't look into .authinfo for local sudo tramp buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+hlissner*no-authinfo-for-tramp)

(after! smartparens
  ;; auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "ret") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "ret") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

(after! evil-mc
  ;; if i'm in insert mode, chances are i want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;; don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs
        (append (list '+xwu-snippets-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))

;; ==== bookmark settings {{{ ====
(require 'bookmark+)
;; fix the error that bmkp-info-cp is void
;; (defalias 'bmkp-info-cp 'bmkp-info-node-name-cp)

;; ==== end bookmark settings }}} ====

;; settings needed for irony-mode, disabled it since it cause slowness
;; (setq irony-server-install-prefix "~/tools/irony-server")
(setq irony-cdb-search-directory-list '("." "src" "build"))
;; (setenv "ld_library_path" "/opt/bb/lib/llvm-5.0/lib64")

;; ==== world clock {{{ ====
;; https://en.wikipedia.org/wiki/list_of_tz_database_time_zones
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
;; ==== end world clock }}} ====

(require 'eacl)

;; ==== rtags settings {{{ ====
(require 'rtags)
(setq rtags-socket-file (concat (substitute-in-file-name "$HOME/") ".rdm"))
;; (setq rtags-path "/opt/bb/bin")
;; (setq rtags-completions-enabled t)


;; (require 'company-rtags)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)
;; ==== END rtags settings }}} ====

;; ==== flycheck settings {{{ ====
;; (setq flycheck-c/c++-clang-executable "/opt/bb/bin/clang++")
(setq flycheck-c/c++-clang-executable "/usr/local/opt/llvm/bin/clang++")
(setq flycheck-clang-args '("-m32" "-Dlint" "-D_REENTRANT"
      "-D_THREAD_SAFE" "-DBB_THREADED" "-DBSL_OVERRIDES_STD"))

;; (defun my-flycheck-setup ()
;;   (flycheck-select-checker 'c/c++-clang))
;; (add-hook 'c-mode-common-hook #'my-flycheck-setup)

; this does not work, not sure why
;; (require 'flycheck-rtags)
;; ;; http://syamajala.github.io/c-ide.html
;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (add-hook 'c-mode-hook 'flycheck-mode)

;; (defun my-flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil) ;; rtags creates more accurate overlays.
;;   (setq-local flycheck-check-syntax-automatically nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;; ==== flycheck settings }}} ====

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
  "clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(setq eshell-aliases-file (concat +xwu-dir "eshell_alias"))

;; support large file size
(setq tramp-inline-compress-start-size 10000000)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; https://www.gnu.org/software/emacs/manual/html_node/tramp/password-handling.html
;; store the password for a period of time, helpful in the tramp case
(setq password-cache-expiry nil)

;; http://emacs.stackexchange.com/questions/15208/using-tramp-for-logs?rq=1
;; auto-revert-tail-mode is great, but it has its limits. therefore
;; i prefer to use an asynchronous shell command. open the remote
;; directory in dired, position the cursor to the file you want to
;; watch, and apply ! tail -f * &.

;; ==== ivy settings {{{ ====
(setq ivy-count-format "(%d/%d) ")
;; http://oremacs.com/2017/11/30/ivy-0.10.0/
(setq ivy-use-selectable-prompt t)
;; ==== end ivy settings }}} ====

;; ==== counsel settings {{{ ====
;; http://oremacs.com/2017/04/09/ivy-0.9.0/
(setq counsel-yank-pop-separator "\n-------------------------------------------------------\n")
(setq counsel-bookmark-avoid-dired nil)
;; ==== end counsel settings }}} ====

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

;; ==== version control settings {{{ ====
(add-hook  'vc-dir-mode-hook
             (lambda ()
               ;; hide up-to-date and unregistered files.
               (define-key vc-dir-mode-map
                 (kbd "x") #'leuven-vc-dir-hide-up-to-date-and-unregistered)
               (define-key vc-dir-mode-map
                 (kbd "e") #'vc-ediff)
               (define-key vc-dir-mode-map
                 (kbd "#") #'vc-ediff-ignore-whitespace)
                                         ; ediff-windows-wordwise?
               ))

(add-hook  'vc-svn-log-view-mode-hook
           (lambda ()
             (define-key vc-svn-log-view-mode-map
               (kbd "e") #'vc-ediff)
             (define-key vc-svn-log-view-mode-map
               (kbd "#") #'vc-ediff-ignore-whitespace)
                                        ; ediff-windows-wordwise?
             ))
;; ==== end version control settings }}} ====

;; ==== ediff settings {{{ ====
(require 'evil-ediff)
;; https://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

; ignore white space
(csetq ediff-diff-options "-w")
;; ==== end ediff settings }}} ====

;; ==== dired settings {{{ ====
;; http://oremacs.com/2015/01/13/dired-options/
;;http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
;; not working for mac
;; (setq dired-listing-switches "-lah")

;; this --group-directories-first doesn't work on mac os natively,
;; also -g option doesn't work
;; (setq dired-listing-switches "-lagh1v --group-directories-first")
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
;; make df output in dired buffers easier to read
(setq dired-free-space-args "-pm")

;; try suggesting dired targets
(setq dired-dwim-target t)

;; understand .zip the way it does tarballs, letting the z key decompress it:
;; handle zip compression
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))
;; ==== end dired settings }}} ====

;; set this so search is performed on all buffers,
;; not just current buffer
;; (setq avy-all-windows t)

(autoload 'dash-at-point "dash-at-point"
          "search the word at point with dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

;; ==== workspace settings {{{ ====
(defvar doom-default-workspace-name "main"
  " name of the default layout.")

(defvar doom-last-selected-workspace doom-default-workspace-name
  "previously selected layout.")

(defun +workspace/save-name(name frame)
  (setq doom-last-selected-workspace persp-last-persp-name)
  (message (format "persp-last: %s" persp-last-persp-name))
)

(add-hook 'persp-before-switch-functions #'+workspace/save-name)
;; ==== end workspace settings }}} ====

;; ==== term mode settings {{{ ====
(setq multi-term-dedicated-select-after-open-p t)

(defun me/paste-in-term-mode()
  (interactive)
  (term-paste)
  (evil-emacs-state)
)

(defun setup-my-term-mode()
  (setq-local global-hl-line-mode nil)
  (setq-local beacon-mode nil)
  (setq term-buffer-maximum-size 0)
  (define-key term-raw-map (kbd "<escape>") 'evil-normal-state)
  (define-key term-raw-map (kbd "C-;") 'evil-normal-state)

  ;; todo: needs more work for this to work
  ;; (define-key term-raw-map (kbd "jf") 'enter-evil-normal)
  ;; todo: not working due to c-y is defined globally
  ;; (define-key term-raw-map (kbd "c-y") 'term-paste)

  (define-key term-raw-map (kbd "C-s") 'counsel-grep-or-swiper)
  (define-key term-raw-map (kbd "M-v") 'me/paste-in-term-mode)
  ;; note: automatically switch to evil-emacs-state
  ;; after press *p* in normal mode which seems the case most of the time
  (evil-define-key 'normal term-raw-map
    ;; "p" 'term-paste
    "p" 'me/paste-in-term-mode
    "i" 'evil-emacs-state
    "i" 'evil-emacs-state
    "a" 'evil-emacs-state
    "a" 'evil-emacs-state)
)

(add-hook 'term-mode-hook #'setup-my-term-mode)
;; ==== end term mode settings }}} ====

;; ==== beacon settings {{{ ====
(require 'beacon)
(beacon-mode 1)
(setq beacon-color "#66cd00")
(setq beacon-size 50)
(setq beacon-blink-delay 0.7)
;; ==== end beacon settings }}} ====

;; ==== auto-complete settings {{{ ====
;; disable it if it's too slow
(require 'company)
(setq company-idle-delay 0.1
      company-minimum-prefix-length 3)

;; need this to complete something like berlin-crazy-cold-jupiter
;; or fifteen_mountain_massachusetts_nineteen
(setq company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")
;; ==== END auto-complete settings }}} ====

(require 'fancy-narrow)
(require 'evil-numbers)

;; i want to switch window across frame
(setq aw-scope 'global)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map
      [remap completion-at-point] 'counsel-irony)
  (define-key irony-mode-map
      [remap complete-symbol] 'counsel-irony))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)

(require 'evil-magit)
(require 'engine-mode)

(require 'ob-ipython)
(require 'lentic)
;; (require 'clean-aident-mode)

;; ==== clang-format settings {{{ ====
;; (require 'clang-format)
;; (global-set-key (kbd "c-c i") 'clang-format-region)
;; (global-set-key (kbd "c-c u") 'clang-format-buffer)

;; (setq clang-format-style-option "llvm")
;; ==== end clang-format settings }}} ====

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

;; ==== deft settings {{{ ====
(require 'deft)
(setq deft-default-extension "org")
(setq deft-extensions '("org"))
(setq deft-directory "~/org")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-file-naming-rules '((noslash . "-")
                               (nospace . "-")
                               (case-fn . downcase)))
(setq deft-text-mode 'org-mode)
(add-to-list 'evil-emacs-state-modes 'deft-mode)
;; ==== end deft settings }}} ====

;; maximize emacs upon startup
(toggle-frame-maximized)

;; ==== smooth workflow for capturing screenshot into org-mode {{{ ====
(require 'org-attach-screenshot)

(setq org-attach-screenshot-command-line
      "screencapture -i %f")

(setq org-attach-screenshot-dirfunction
		(lambda ()
		  (concat +org-dir "/files/")))
;; ==== END smooth workflow for capturing screenshot into org-mode }}} ====

(require 'interleave)

(require 'yankpad)
(setq yankpad-file (concat +xwu-dir "yankpad.org"))

(require 'tldr)

;; ==== NOTE: put this as last since (pdf-tools-install) throws error for some reason==
(require 'pdf-occur)
(require 'pdf-tools)
(pdf-tools-install)

