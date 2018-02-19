;; -*- origami-fold-style: triple-braces -*-

;; (setq debug-on-error t)
(require 'counsel)

(load! +alias)  ; emacs alias
(load! +commands)  ; my custom ex commands
(load! +myabbrev)

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/private/showgood/evil-collection/"))
;; (with-eval-after-load 'dired (require 'evil-collection-dired) (evil-collection-dired-setup))

;; ==== general settings {{{ ====
;; it’s much easier to move around lines based on how they are
;; displayed, rather than the actual line. this helps a ton with
;; long log file lines that may be wrapped:
;; (setq line-move-visual t)

;; i don't need linenum most of the time
;; hopefully can speed up a bit for large file
(setq doom-line-numbers-style nil)

 ; proper line wrapping
(global-visual-line-mode 1)

;; (set-frame-font "fira code:pixelsize=16:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")

;; ==== end general settings }}} ====

;; ==== evil settings {{{ ====
;; very important to me
(setq-default evil-escape-key-sequence "jf")
(fset 'evil-visual-update-x-selection 'ignore)

(evil-add-command-properties #'counsel-imenu :jump t)
;; ==== end evil settings }}} ====

;; ==== frequently used packages {{{ ====
(require 'hl-anything)
(hl-highlight-mode)

(require 'origami)
(global-origami-mode 1)

;; ==== END frequently used packages }}} ====

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

(autoload 'dash-at-point "dash-at-point"
          "search the word at point with dash." t nil)

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

;; https://oremacs.com/2017/12/27/company-numbers/
(setq company-show-numbers t)

(defun ora-company-number ()
  "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number
       (if (equal k "0")
           10
         (string-to-number k))))))

(let ((map company-active-map))
  (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
        (number-sequence 0 9))
  (define-key map " " (lambda ()
                        (interactive)
                        (company-abort)
                        (self-insert-command 1)))
  (define-key map (kbd "<return>") nil))
;; ==== END auto-complete settings }}} ====

(require 'fancy-narrow)
(require 'evil-numbers)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map
      [remap completion-at-point] 'counsel-irony)
  (define-key irony-mode-map
      [remap complete-symbol] 'counsel-irony))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)

(require 'evil-magit)
(require 'engine-mode)

(require 'lentic)

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

(require 'yankpad)
(setq yankpad-file (concat +xwu-dir "yankpad.org"))

(require 'tldr)

(load! +bindings)  ; my key bindings

(require 'visual-regexp)
(require 'visual-regexp-steroids)

(require 'atomic-chrome)
(atomic-chrome-start-server)

(require 'paperless)

(setq paperless-capture-directory "~/scan")
(setq paperless-root-directory "~/docs")

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "/usr/local/bin/clisp")
(require 'slime-autoloads)
;; (require 'slime)
;; (slime-setup '(slime-fancy slime-tramp slime-asdf))
;; (slime-setup '(slime-fancy slime-tramp))
;; (slime-require :swank-listener-hooks)

(require 'lispy)
(require 'lispyville)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lispy-mode-hook #'lispyville-mode)
