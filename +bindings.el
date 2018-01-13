(defmacro find-file-in! (path &optional project-p)
  "Returns an interactive function for searching files."
  `(lambda () (interactive)
     (let ((default-directory ,path))
       (call-interactively
        ',(command-remapping
           (if project-p
               #'projectile-find-file
             #'find-file))))))

(map!
 [remap evil-jump-to-tag] #'projectile-find-tag
 [remap find-tag]         #'projectile-find-tag
 ;; ensure there are no conflicts
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil)

(map!
 ;; --- Global keybindings ---------------------------
 ;; very important to me, smoothier workflow

 ;; TODO: conflict with term-mode
 :nvime "C-y" #'yank

 :vime "M-SPC"  #'+company/complete
 :vime "M-/"    #'dabbrev-expand
 :nvime "<f12>" #'org-todo
 :nvime "<f11>" #'org-agenda
 :nvime "<f10>" #'org-capture
 :nvime "<f2>"  #'org-clock-goto
 :nvime "<f3>"  #'org-clock-in
 :nvime "<f4>"  #'org-clock-out
 ;; :nvime "<f6>"  #'rtags-find-symbol-at-point
 :nvime "<f6> a"  #'bb/arm-report-status
 :nvime "<f6> r"  #'bb/report-status
 :nvime "C-;"   #'evil-normal-state

:nvime "<f5> a" #'org-archive-subtree
:nvime "<f5> c" #'calendar
:nvime "<f5> d" #'ace-delete-window
:nvime "<f5> l" (lambda () (interactive) (list-matching-lines (current-word)))
:nvime "<f5> r" 'org-refile

:nvime "<f7> b" #'counsel-projectile-switch-to-buffer
:nvime "<f7> c" #'projectile-compile-project
:nvime "<f7> d" #'counsel-projectile-find-dir
:nvime "<f7> e" #'eval-region
:nvime "<f7> f" #'counsel-projectile-find-file
;; open the file under cursor within project (C-c p g)
:nvime "<f7> g" #'projectile-find-file-dwim
:nvime "<f7> o" #'projectile-find-file-dwim-other-window
:nvime "<f7> s" #'counsel-rg

:nvime "<f8> c" #'counsel-git-grep-complete-line

:nvime "<f9> c" #'cp-filename-of-current-buffer
;; copy current line
:nvime "<f9> d" #'duplicate-line
:nvime "<f9> e" #'+eshell/open
:nvime "<f9> a" #'org-attach
:nvime "<f9> r" #'rename-buffer

:nvime "<f10>" #'org-capture
:nvime "<f11>" #'org-agenda
:nvime "<f12>" #'org-todo

 ;; TODO this one does not seem to work in minifbuffer find out why
:nvime "M-y" #'counsel-yank-pop

:nvime "C-c <left>" #'winner-undo
:nvime "C-c <right>" #'winner-redo

:nvime "C-c +" #'evil-numbers/inc-at-pt
:nvime "C-c -" #'evil-numbers/dec-at-pt

:nvime "C-x b" #'counsel-bookmark
:nvime "C-x c" #'cp-filename-of-current-buffer
:nvime "C-x f" #'counsel-git
:nvime "C-x g" #'counsel-git-grep
:nvime "C-x k" #'kill-this-buffer
:nvime "C-x j" #'evil-avy-goto-char-2
:nvime "C-x m" #'magit-status
:nvime "C-x o" #'ace-window
:nvime "C-x s" #'avy-pop-mark
;; quickly select the content for a xml element for replace
:nvime "C-x t" #'web-mode-element-content-select

:nvime "\C-ca" #'org-agenda
:nvime "\C-cc" #'org-capture
:nvime "\C-co" #'evil-replace-word-selection
:nvime "\C-cl" #'org-store-link
:nvime "\C-cr" #'org-refile
;; "\C-cj" #'org-journal-new-entry

:nvime "\C-cs" #'new-shell

 ;; "\C-xp") 'spacemacs/copy-clipboard-to-whole-buffer)
;; "\C-xY") 'spacemacs/copy-whole-buffer-to-clipboard)

 ;; Make M-x available everywhere
 :nvime "M-x" #'execute-extended-command
 :nvime "A-x" #'execute-extended-command

 ;; Emacs debug utilities
 "M-;"        #'eval-expression
 :nvime "M-;" #'eval-expression
 "M-:"        #'doom/open-scratch-buffer
 :nvime "M-:" #'doom/open-scratch-buffer
 ;; Text-scaling
 "M-+"    (λ! (text-scale-set 0))
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease
 ;; Simple window navigation/manipulation
 "C-`"    #'doom/popup-toggle
 "C-~"    #'doom/popup-raise
 "M-t"    #'+workspace/new
 "M-T"    #'+workspace/display
 "M-w"    #'delete-window
 "M-W"    #'+workspace/close-workspace-or-frame
 "M-n"    #'evil-buffer-new
 "M-N"    #'make-frame
 "M-1"    (λ! (+workspace/switch-to 0))
 "M-2"    (λ! (+workspace/switch-to 1))
 "M-3"    (λ! (+workspace/switch-to 2))
 "M-4"    (λ! (+workspace/switch-to 3))
 "M-5"    (λ! (+workspace/switch-to 4))
 "M-6"    (λ! (+workspace/switch-to 5))
 "M-7"    (λ! (+workspace/switch-to 6))
 "M-8"    (λ! (+workspace/switch-to 7))
 "M-9"    (λ! (+workspace/switch-to 8))
 "M-0"    #'+workspace/switch-to-last
 ;; Other sensible, textmate-esque global bindings
 "M-r"    #'+eval/buffer
 "M-R"    #'+eval/region-and-replace
 "M-b"    #'+eval/build
 "M-a"    #'mark-whole-buffer
 "M-c"    #'evil-yank
 "C-y"    #'yank
 "M-q"    (if (daemonp) #'delete-frame #'save-buffers-kill-emacs)
 "M-s"    #'save-buffer
 "M-v"    #'clipboard-yank
 "C-s"    #'swiper
 "C-M-f"  #'doom/toggle-fullscreen
 :m "A-j" #'+hlissner:multi-next-line
 :m "A-k" #'+hlissner:multi-previous-line
 :nv "C-SPC" #'+evil:fold-toggle
 ;; Easier window navigation
 :en "C-h"    #'evil-window-left
 :en "C-j"    #'evil-window-down
 :en "C-k"    #'evil-window-up
 :en "C-l"    #'evil-window-right

 (:prefix "C-x"
   "p" #'doom/other-popup)

 ;; --- <leader> -------------------------------------
 (:leader
   :desc "Ex command"  :nv ";"   #'evil-ex
   :desc "M-x"         :nv ":"   #'execute-extended-command
   :desc "Pop up scratch buffer"   :nv "x"  #'doom/open-scratch-buffer
   :desc "Org Capture"             :nv "X"  #'+org-capture/open

   ;; Most commonly used
   :desc "jump char 2"                :n "k"   #'evil-avy-goto-char-2
   :desc "Switch buffer in workspace" :n "q"   #'+ivy/switch-workspace-buffer
   :desc "Switch buffer"              :n "Q"   #'ivy-switch-buffer
   :desc "git grep"                   :n "d"   #'counsel-git-grep
   :desc "execute command"            :n "SPC" #'counsel-M-x
   :desc "switch to previous buffer"  :n "TAB" #'switch-to-previous-buffer
   :desc "Switch workspace buffer"    :n ","   #'persp-switch-to-buffer
   :desc "Browse files"               :n "."   #'find-file
   :desc "Toggle last popup"          :n "~"   #'doom/popup-toggle
   :desc "Eval expression"            :n "`"   #'eval-expression
   :desc "Blink cursor line"          :n "DEL" #'+doom/blink-cursor
   :desc "Jump to bookmark"           :n "RET" #'bookmark-jump

   ;; C-u is used by evil
   :desc "Universal argument"    :n "u"  #'universal-argument
   ;; :desc "window"                :n "w"  evil-window-map

   (:desc "previous..." :prefix "["
     :desc "Text size"           :nv "[" #'text-scale-decrease
     :desc "Buffer"              :nv "b" #'doom/previous-buffer
     :desc "Diff Hunk"           :nv "d" #'git-gutter:previous-hunk
     :desc "Todo"                :nv "t" #'hl-todo-previous
     :desc "Error"               :nv "e" #'previous-error
     :desc "Workspace"           :nv "w" #'+workspace/switch-left
     :desc "Smart jump"          :nv "h" #'smart-backward
     :desc "Spelling error"      :nv "s" #'evil-prev-flyspell-error
     :desc "Spelling correction" :n  "S" #'flyspell-correct-previous-word-generic)

   (:desc "next..." :prefix "]"
     :desc "Text size"           :nv "]" #'text-scale-increase
     :desc "Buffer"              :nv "b" #'doom/next-buffer
     :desc "Diff Hunk"           :nv "d" #'git-gutter:next-hunk
     :desc "Todo"                :nv "t" #'hl-todo-next
     :desc "Error"               :nv "e" #'next-error
     :desc "Workspace"           :nv "w" #'+workspace/switch-right
     :desc "Smart jump"          :nv "l" #'smart-forward
     :desc "Spelling error"      :nv "s" #'evil-next-flyspell-error
     :desc "Spelling correction" :n  "S" #'flyspell-correct-word-generic)

   (:desc "search" :prefix "/"
     :desc "Swiper"                :nv "/" #'swiper
     :desc "Imenu"                 :nv "i" #'imenu
     :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere
     :desc "Online providers"      :nv "o" #'+jump/online-select)

   (:desc "Error" :prefix "e"
     :desc "list errors"         :nv "l" #'flycheck-list-errors
     :desc "Next Error"          :nv "n" #'next-error
     :desc "Previous Error"      :nv "p" #'previous-error)

   (:desc "jump" :prefix "j"
     :desc "Imenu"                :nv "i" #'imenu
     :desc "jump back"            :nv "b" #'avy-pop-mark
     :desc "Imenu across buffers" :nv "I" #'imenu-anywhere
     :desc "show marks"           :nv "m" #'evil-show-marks
     :desc "show registers"       :nv "r" #'evil-show-registers
     :desc "Online providers"     :nv "o" #'+jump/online-select)

   (:desc "workspace/layout" :prefix "l"
     :desc "New workspace"            :n "n"   #'+workspace/me/new
     :desc "Save workspace to file"   :n "s"   #'+workspace/save
     :desc "Delete this workspace"    :n "d"   #'+workspace/delete
     :desc "Load workspace from file" :n "L"   #'+workspace/load
     :desc "rename workspace"         :n "r"   #'+workspace/rename
     :desc "toggle between workspace" :n "TAB" #'doom/jump-to-last-workspace
     :desc "switch layout"            :n "l"   #'+workspace/switch-to)

   (:desc "bookmark" :prefix "o"
     :desc "set bookmark"                 :n "m"     #'bookmark-set
     :desc "open bookmark buffer"         :n "l"     #'bookmark-bmenu-list
     :desc "set url bookmark"             :n "u"     #'bmkp-url-target-set
     :desc "jump to url bookmark"         :n "w"     #'bmkp-url-jump
     :desc "jump to file bookmark"        :n "f"     #'bmkp-non-dir-file-jump
     :desc "jump to dired bookmark"       :n "d"     #'bmkp-dired-jump
     :desc "jump to remote file bookmark" :n "r"     #'bmkp-remote-file-jump
     :desc "set snippet bookmark"         :n "s"     #'bmkp-set-snippet-bookmark
     :desc "annotate bookmark"            :n "a"     #'bmkp-annotate-bookmark
     :desc "copy snippet bookmark"        :n "k"     #'bmkp-snippet-to-kill-ring)

   (:desc "application" :prefix "a"
        :desc "align regexp" :nv "r"     #'align-regexp
        :desc "open terminal" :n "t"     #'+term/open)

   (:desc "window" :prefix "w"
     :desc "select previous window" :n "TAB" #'aw-flip-window
     :desc "maximize window"        :n "m"   #'delete-other-windows
     :desc "split horizontally"     :n "-"   #'evil-window-split
     :desc "split vertically"       :n "v"   #'evil-window-vsplit
     :desc "swap window"            :n "s"   #'ace-swap-window
     :desc "ace window"             :n "w"   #'ace-window
     :desc "window up"              :n "k"   #'evil-window-up
     :desc "window down"            :n "j"   #'evil-window-down
     :desc "window left"            :n "h"   #'evil-window-left
     :desc "window right"           :n "l"   #'evil-window-right
     :desc "delete window"          :n "d"   #'delete-window
     :desc "make frame"             :n "F"   #'make-frame
     :desc "ace delete window"      :n "D"   #'ace-delete-window
     :desc "toggle window layout"   :n "t"   #'window-split-toggle)

   (:desc "buffer" :prefix "b"
     :desc "New empty buffer"        :n "n" #'evil-buffer-new
     :desc "Switch workspace buffer" :n "b" #'persp-switch-to-buffer
     :desc "Switch buffer"           :n "B" #'switch-to-buffer
     :desc "Kill buffer"             :n "k" #'doom/kill-this-buffer
     :desc "Kill other buffers"      :n "o" #'doom/kill-other-buffers
     :desc "Pop scratch buffer"      :n "s" #'open-scratch
     :desc "Bury buffer"             :n "z" #'bury-buffer
     :desc "Next buffer"             :n "]" #'doom/next-buffer
     :desc "Previous buffer"         :n "[" #'doom/previous-buffer
     :desc "Sudo edit this file"     :n "S" #'doom/sudo-this-file)

   (:desc "code" :prefix "c"
     :desc "List errors"               :n  "x" #'flycheck-list-errors
     :desc "Evaluate buffer/region"    :n  "e" #'+eval/buffer
                                       :v  "e" #'+eval/region
     :desc "Evaluate & replace region" :nv "E" #'+eval:replace-region
     :desc "Build tasks"               :nv "b" #'+eval/build
     :desc "Jump to definition"        :n  "d" #'+jump/definition
     :desc "Jump to references"        :n  "D" #'+jump/references
     :desc "Open REPL"                 :n  "r" #'+eval/open-repl
                                       :v  "r" #'+eval:repl)

   (:desc "file" :prefix "f"
     :desc "Sudo find file"            :n ">" #'doom/sudo-find-file
     :desc "Find file in project"      :n "/" #'projectile-find-file
     :desc "Find file from here"       :n "?" #'counsel-file-jump
     :desc "Find other file"           :n "a" #'projectile-find-other-file
     :desc "Open project editorconfig" :n "c" #'editorconfig-find-current-editorconfig
     :desc "delete file"               :n "d" #'+evil:delete-this-file
     :desc "Find file in emacs.d"      :n "e" #'+xwu/find-in-emacsd
     :desc "Browse emacs.d"            :n "E" #'+xwu/browse-emacsd
     :desc "Find File"                 :n "f" #'counsel-find-file
     :desc "dired jump"                :n "j" #'dired-jump
     :desc "yank file name only"       :n "n" #'cp-filename-of-current-buffer
     :desc "Yank file full path"       :n "p" #'+hlissner/yank-buffer-filename
     :desc "Recent files"              :n "R" #'counsel-recentf
     :desc "Find file in dotfiles"     :n "." #'+xwu/find-in-dotfiles
     :desc "Browse dotfiles"           :n "T" #'+xwu/browse-dotfiles)

   (:desc "git" :prefix "g"
     :desc "Git status"        :n  "s" #'magit-status
     :desc "Git blame"         :n  "b" #'magit-blame
     :desc "Git time machine"  :n  "t" #'git-timemachine-toggle
     :desc "Git revert hunk"   :n  "r" #'git-gutter:revert-hunk
     :desc "Git revert buffer" :n  "R" #'vc-revert
     :desc "List gists"        :n  "g" #'+gist:list
     :desc "Next hunk"         :nv "]" #'git-gutter:next-hunk
     :desc "Previous hunk"     :nv "[" #'git-gutter:previous-hunk)

   (:desc "help / highlight" :prefix "h"
                                   :n "h" help-map
     :desc "Apropos"               :n "a" #'apropos
     :desc "Reload theme"          :n "R" #'doom/reload-theme
     ;; :desc "Find library"       :n "l" #'find-library
     ;; :desc "Command log"        :n "L" #'global-command-log-mode
     :desc "Toggle Emacs log"      :n "M" #'doom/popup-toggle-messages
     :desc "Describe mode"         :n "m" #'describe-mode
     :desc "Describe function"     :n "f" #'describe-function
     :desc "Describe key"          :n "k" #'describe-key
     :desc "Describe char"         :n "c" #'describe-char
     :desc "Describe variable"     :n "v" #'describe-variable
     :desc "Describe face"         :n "F" #'describe-face
     :desc "Describe DOOM setting" :n "s" #'doom/describe-setting
     :desc "Describe DOOM module"  :n "d" #'doom/describe-module
     :desc "Find definition"       :n "." #'+jump/definition
     :desc "Find references"       :n "/" #'+jump/references
     :desc "Find documentation"    :n "h" #'+jump/documentation
     :desc "What face"             :n "'" #'doom/what-face
     :desc "What minor modes"      :n ";" #'doom/what-minor-mode
     :desc "Info"                  :n "i" #'info
     :desc "prev highlight "       :n "N" #'hl-find-prev-thing
     :desc "next highlight "       :n "n" #'hl-find-next-thing
     :desc "highlight global"      :n "L" #'hl-highlight-thingatpt-global
     :desc "un highlight global"   :n "U" #'hl-unhighlight-all-global
     :desc "highlight local"       :n "l" #'hl-highlight-thingatpt-local
     :desc "un highlight local"    :n "u" #'hl-unhighlight-all-local
     :desc "Toggle profiler"       :n "p" #'doom/toggle-profiler)

   (:desc "insert" :prefix "i"
     :desc "From kill-ring" :nv "y" #'counsel-yank-pop
     :desc "From snippet"   :nv "s" #'yas-insert-snippet)

   (:desc "notes" :prefix "n"
     :desc "Find file in notes"    :n "n" #'+xwu/find-in-notes
     :desc "Browse notes"          :n "N" #'+xwu/browse-notes
     :desc "Org capture"           :n "x" #'+org-capture/open
     :desc "Browse mode notes"     :n "m" #'+org/browse-notes-for-major-mode
     :desc "Browse project notes"  :n "p" #'+org/browse-notes-for-project)

   ;;(:desc "open" :prefix "o"
   ;;  :desc "Default browser"     :n  "b" #'browse-url-of-file
   ;;  :desc "Debugger"            :n  "d" #'+debug/open
   ;;  :desc "REPL"                :n  "r" #'+eval/open-repl
   ;;                              :v  "r" #'+eval:repl
   ;;  :desc "Neotree"             :n  "n" #'+neotree/toggle
   ;;  :desc "Terminal"            :n  "t" #'+term/open-popup
   ;;  :desc "Terminal in project" :n  "T" #'+term/open-popup-in-project

   ;;  ;; applications
   ;;  :desc "APP: elfeed"  :n "E" #'=rss
   ;;  :desc "APP: email"   :n "M" #'=email
   ;;  :desc "APP: twitter" :n "T" #'=twitter
   ;;  :desc "APP: regex"   :n "X" #'=regex

   ;;  ;; macos
   ;;  (:when IS-MAC
   ;;    :desc "Reveal in Finder"          :n "o" #'+macos/reveal-in-finder
   ;;    :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project-in-finder
   ;;    :desc "Send to Transmit"          :n "u" #'+macos/send-to-transmit
   ;;    :desc "Send project to Transmit"  :n "U" #'+macos/send-project-to-transmit
   ;;    :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
   ;;    :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar))

   (:desc "project" :prefix "p"
     :desc "Browse project"          :n  "." (find-file-in! (doom-project-root))
     :desc "Find file in project"    :n  "/" #'projectile-find-file
     :desc "Find file in project"    :n  "f" #'counsel-projectile-find-file
     :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
     :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
     :desc "Switch project"          :n  "p" #'projectile-switch-project
     :desc "Recent project files"    :n  "r" #'projectile-recentf
     :desc "List project tasks"      :n  "t" #'+ivy/tasks
     :desc "Pop term in project"     :n  "o" #'+term/open-popup-in-project
     :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

   (:desc "snippets / switch" :prefix "s"
     :desc "New snippet"           :n  "n" #'yas-new-snippet
     :desc "Insert snippet"        :nv "i" #'yas-insert-snippet
     :desc "Find snippet for mode" :n  "s" #'yas-visit-snippet-file
     :desc "Find snippet"          :n  "S" #'+hlissner/find-in-snippets
     :desc "switch to nylx"        :n  "n" #'bb/nylx
     :desc "switch to trsl"        :n  "t" #'bb/trsl)

   (:desc "toggle" :prefix "t"
     :desc "Flyspell"                 :n "s" #'flyspell-mode
     :desc "Flycheck"                 :n "f" #'flycheck-mode
     :desc "Line numbers"             :n "l" #'doom/toggle-line-numbers
     :desc "multi term toggle"        :n "t" #'multi-term-dedicated-toggle
     ;; fci-mode seems have performance issue
     ;; :desc "Fill column indicator" :n "f" #'fci-mode
     :desc "Indent guides"            :n "i" #'highlight-indentation-mode
     :desc "Indent guides (column)"   :n "I" #'highlight-indentation-current-column-mode
     :desc "Impatient mode"           :n "h" #'+impatient-mode/toggle
     :desc "Big mode"                 :n "b" #'doom-big-font-mode
     :desc "Evil goggles"             :n "g" #'+evil-goggles/toggle))

 ;; --- Personal vim-esque bindings ------------------
 :n  "zx" #'doom/kill-this-buffer
 :n  "ZX" #'bury-buffer
 :n  "]b" #'doom/next-buffer
 :n  "[b" #'doom/previous-buffer
 :n  "]w" #'+workspace/switch-right
 :n  "[w" #'+workspace/switch-left
 :m  "gt" #'+workspace/switch-right
 :m  "gT" #'+workspace/switch-left
 :m  "gd" #'+jump/definition
 :m  "gD" #'+jump/references
 :m  "gh" #'+jump/documentation
 :n  "gp" #'+evil/reselect-paste
 :n  "gr" #'+eval:region
 :n  "gR" #'+eval/buffer
 :v  "gR" #'+eval:replace-region
 :v  "@"  #'+evil:macro-on-all-lines
 :n  "g@" #'+evil:macro-on-all-lines
 ;; repeat in visual mode (FIXME buggy)
 :v  "."  #'evil-repeat
 ;; don't leave visual mode after shifting
 :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
 :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv
 ;; paste from recent yank register (which isn't overwritten)
 :v  "C-p" "\"0p"

 ;; ----------- rtags bindings ----------------------
 (:prefix ","
   :n "d" #'deft
   :n "f" #'deft-find-file
   :n "s" #'rtags-find-symbol-at-point
   :n "r" #'rtags-find-references-at-point)

 (:map evil-window-map ; prefix "C-w"
   ;; Navigation
   "C-h"     #'evil-window-left
   "C-j"     #'evil-window-down
   "C-k"     #'evil-window-up
   "C-l"     #'evil-window-right
   "C-w"     #'ace-window
   ;; Swapping windows
   "H"       #'+evil/window-move-left
   "J"       #'+evil/window-move-down
   "K"       #'+evil/window-move-up
   "L"       #'+evil/window-move-right
   "C-S-w"   #'ace-swap-window
   ;; Window undo/redo
   "u"       #'winner-undo
   "C-u"     #'winner-undo
   "C-r"     #'winner-redo
   "o"       #'doom/window-enlargen
   ;; Delete window
   "c"       #'+workspace/close-window-or-workspace
   "C-C"     #'ace-delete-window)

 ;; --- Plugin bindings ------------------------------
 ;; auto-yasnippet
 :i  [C-tab] #'aya-expand
 :nv [C-tab] #'aya-create

 ;; company-mode (vim-like omnicompletion)
 :i "C-SPC"  #'+company/complete
 (:prefix "C-x"
   :i "C-l"   #'+company/whole-lines
   :i "C-k"   #'+company/dict-or-keywords
   :i "C-f"   #'company-files
   :i "C-]"   #'company-etags
   :i "s"     #'company-ispell
   :i "C-s"   #'company-yasnippet
   :i "C-o"   #'company-capf
   :i "C-n"   #'company-dabbrev-code
   :i "C-p"   #'+company/dabbrev-code-previous)
 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-w"        nil
     "C-o"        #'company-search-kill-others
     "C-n"        #'company-select-next
     "C-p"        #'company-select-previous
     "C-h"        #'company-quickhelp-manual-begin
     "C-S-h"      #'company-show-doc-buffer
     "C-S-s"      #'company-search-candidates
     "C-s"        #'company-filter-candidates
     "C-SPC"      #'company-complete-common
     "C-h"        #'company-quickhelp-manual-begin
     [tab]        #'company-complete-common-or-cycle
     [backtab]    #'company-select-previous
     [escape]     (λ! (company-abort) (evil-normal-state 1)))
   ;; Automatically applies to `company-filter-map'
   (:map company-search-map
     "C-n"        #'company-search-repeat-forward
     "C-p"        #'company-search-repeat-backward
     "C-s"        (λ! (company-search-abort) (company-filter-candidates))
     [escape]     #'company-search-abort))

 ;; counsel
 (:after counsel
   (:map counsel-ag-map
     [backtab]  #'+ivy/wgrep-occur  ; search/replace on results
     "C-SPC"    #'counsel-git-grep-recenter   ; preview
     "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

 ;; evil-commentary
 :n  "gc"  #'evil-commentary

 ;; evil-exchange
 :n  "gx"  #'evil-exchange

 ;; evil-matchit
 :nv [tab] #'+evil/matchit-or-toggle-fold

 ;; evil-magit
 (:after evil-magit
   :map (magit-status-mode-map magit-revision-mode-map)
   :n "C-j" nil
   :n "C-k" nil)

 ;; evil-mc
 (:prefix "gz"
   :nv "m" #'evil-mc-make-all-cursors
   :nv "u" #'evil-mc-undo-all-cursors
   :nv "z" #'+evil/mc-make-cursor-here
   :nv "t" #'+evil/mc-toggle-cursors
   :nv "n" #'evil-mc-make-and-goto-next-cursor
   :nv "p" #'evil-mc-make-and-goto-prev-cursor
   :nv "N" #'evil-mc-make-and-goto-last-cursor
   :nv "P" #'evil-mc-make-and-goto-first-cursor
   :nv "d" #'evil-mc-make-and-goto-next-match
   :nv "D" #'evil-mc-make-and-goto-prev-match)
 (:after evil-mc
   :map evil-mc-key-map
   :nv "C-n" #'evil-mc-make-and-goto-next-cursor
   :nv "C-N" #'evil-mc-make-and-goto-last-cursor
   :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
   :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

 ;; evil-multiedit
 :v  "R"     #'evil-multiedit-match-all
 :n  "M-d"   #'evil-multiedit-match-symbol-and-next
 :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
 :v  "M-d"   #'evil-multiedit-match-and-next
 :v  "M-D"   #'evil-multiedit-match-and-prev
 :nv "C-M-d" #'evil-multiedit-restore
 (:after evil-multiedit
   (:map evil-multiedit-state-map
     "M-d" #'evil-multiedit-match-and-next
     "M-D" #'evil-multiedit-match-and-prev
     "RET" #'evil-multiedit-toggle-or-restrict-region)
   (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
     "C-n" #'evil-multiedit-next
     "C-p" #'evil-multiedit-prev))

 ;; evil-snipe
 (:after evil-snipe
   ;; Binding to switch to evil-easymotion/avy after a snipe
   :map evil-snipe-parent-transient-map
   "C-;" (λ! (require 'evil-easymotion)
             (call-interactively
              (evilem-create #'evil-snipe-repeat
                             :bind ((evil-snipe-scope 'whole-buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight))))))

 ;; evil-surround
 :v  "S"  #'evil-surround-region
 :o  "s"  #'evil-surround-edit
 :o  "S"  #'evil-Surround-edit

 ;; expand-region
 :v  "v"  #'er/expand-region
 :v  "V"  #'er/contract-region

 ;; flycheck
 :m  "]e" #'next-error
 :m  "[e" #'previous-error
 (:after flycheck
   :map flycheck-error-list-mode-map
   :n "C-n" #'flycheck-error-list-next-error
   :n "C-p" #'flycheck-error-list-previous-error
   :n "j"   #'flycheck-error-list-next-error
   :n "k"   #'flycheck-error-list-previous-error
   :n "RET" #'flycheck-error-list-goto-error)

 ;; flyspell
 :m  "]S" #'flyspell-correct-word-generic
 :m  "[S" #'flyspell-correct-previous-word-generic

 ;; git-gutter
 :m  "]d" #'git-gutter:next-hunk
 :m  "[d" #'git-gutter:previous-hunk

 ;; git-timemachine
 (:after git-timemachine
   (:map git-timemachine-mode-map
     :nv "p" #'git-timemachine-show-previous-revision
     :nv "n" #'git-timemachine-show-next-revision
     :nv "g" #'git-timemachine-show-nth-revision
     :nv "q" #'git-timemachine-quit
     :nv "w" #'git-timemachine-kill-abbreviated-revision
     :nv "W" #'git-timemachine-kill-revision
     :nv "b" #'git-timemachine-blame))

 ;; gist
 (:after gist
   :map gist-list-menu-mode-map
   :n "RET" #'+gist/open-current
   :n "b"   #'gist-browse-current-url
   :n "c"   #'gist-add-buffer
   :n "d"   #'gist-kill-current
   :n "f"   #'gist-fork
   :n "q"   #'quit-window
   :n "r"   #'gist-list-reload
   :n "s"   #'gist-star
   :n "S"   #'gist-unstar
   :n "y"   #'gist-print-current-url)

 ;; hl-todo
 :m  "]t" #'hl-todo-next
 :m  "[t" #'hl-todo-previous

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   [escape] #'keyboard-escape-quit
   "M-v" #'yank
   "M-z" #'undo
   "C-r" #'evil-paste-from-register
   "C-k" #'ivy-previous-line
   "C-j" #'ivy-next-line
   "C-l" #'ivy-alt-done
   "C-w" #'ivy-backward-kill-word
   "C-u" #'ivy-kill-line
   "C-b" #'backward-word
   "C-f" #'forward-word)

 ;; neotree
 (:after neotree
   :map neotree-mode-map
   :n "g"         nil
   :n [tab]       #'neotree-quick-look
   :n "RET"       #'neotree-enter
   :n [backspace] #'evil-window-prev
   :n "c"         #'neotree-create-node
   :n "r"         #'neotree-rename-node
   :n "d"         #'neotree-delete-node
   :n "j"         #'neotree-next-line
   :n "k"         #'neotree-previous-line
   :n "n"         #'neotree-next-line
   :n "p"         #'neotree-previous-line
   :n "h"         #'+neotree/collapse-or-up
   :n "l"         #'+neotree/expand-or-open
   :n "J"         #'neotree-select-next-sibling-node
   :n "K"         #'neotree-select-previous-sibling-node
   :n "H"         #'neotree-select-up-node
   :n "L"         #'neotree-select-down-node
   :n "G"         #'evil-goto-line
   :n "gg"        #'evil-goto-first-line
   :n "v"         #'neotree-enter-vertical-split
   :n "s"         #'neotree-enter-horizontal-split
   :n "q"         #'neotree-hide
   :n "R"         #'neotree-refresh)

 ;; realgud
 (:after realgud
   :map realgud:shortkey-mode-map
   :n "j" #'evil-next-line
   :n "k" #'evil-previous-line
   :n "h" #'evil-backward-char
   :n "l" #'evil-forward-char
   :m "n" #'realgud:cmd-next
   :m "b" #'realgud:cmd-break
   :m "B" #'realgud:cmd-clear
   :n "c" #'realgud:cmd-continue)

 ;; rotate-text
 :n  "!"  #'rotate-text

 ;; smart-forward
 :nv "K"  #'smart-up
 :m  "g]" #'smart-forward
 :m  "g[" #'smart-backward

 ;; undo-tree -- undo/redo for visual regions
 :v "C-u" #'undo-tree-undo
 :v "C-r" #'undo-tree-redo

 ;; yasnippet
 (:after yasnippet
   (:map yas-keymap
     "C-e"           #'+snippets/goto-end-of-field
     "C-a"           #'+snippets/goto-start-of-field
     "<M-right>"     #'+snippets/goto-end-of-field
     "<M-left>"      #'+snippets/goto-start-of-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     [escape]        #'evil-normal-state
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field)
   (:map yas-minor-mode-map
     :i "<tab>" yas-maybe-expand
     :v "<tab>" #'+snippets/expand-on-region))


 ;; --- Major mode bindings --------------------------
 (:after markdown-mode
   (:map markdown-mode-map
     ;; fix conflicts with private bindings
     "<backspace>" nil
     "<M-left>"    nil
     "<M-right>"   nil))


 ;; --- Custom evil text-objects ---------------------
 :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
 :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
 :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
 :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
 :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

 ;; --- Built-in plugins -----------------------------
 (:after comint
   ;; TAB auto-completion in term buffers
   :map comint-mode-map
   [tab] #'company-complete
   [up] #'comint-previous-input
   [down] #'comint-next-input)

 (:after debug
   ;; For elisp debugging
   :map debugger-mode-map
   :n "RET" #'debug-help-follow
   :n "e"   #'debugger-eval-expression
   :n "n"   #'debugger-step-through
   :n "c"   #'debugger-continue)

 (:map help-mode-map
   :n "[["  #'help-go-back
   :n "]]"  #'help-go-forward
   :n "o"   #'ace-link-help
   :n "q"   #'quit-window
   :n "Q"   #'+ivy-quit-and-resume)

 (:after vc-annotate
   :map vc-annotate-mode-map
   :n "q"   #'kill-this-buffer
   :n "d"   #'vc-annotate-show-diff-revision-at-line
   :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
   :n "SPC" #'vc-annotate-show-log-revision-at-line
   :n "]]"  #'vc-annotate-next-revision
   :n "[["  #'vc-annotate-prev-revision
   :n "TAB" #'vc-annotate-toggle-annotation-visibility
   :n "RET" #'vc-annotate-find-revision-at-line))

;; --- Custom key functionality ---------------------
(defmacro do-repeat! (command next-func prev-func)
  "Repeat motions with ;/,"
  (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
    `(progn
       (defun ,fn-sym (&rest _)
         (define-key evil-motion-state-map (kbd ";") ',next-func)
         (define-key evil-motion-state-map (kbd ",") ',prev-func))
       (advice-add #',command :before #',fn-sym))))

;; n/N
(do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

;; */#
(after! evil-visualstar
  (do-repeat! evil-visualstar/begin-search-forward
    evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-visualstar/begin-search-backward
    evil-ex-search-previous evil-ex-search-next))

;; evil-easymotion
(after! evil-easymotion
  (let ((prefix (concat doom-leader-key " /")))
    ;; NOTE `evilem-default-keybinds' unsets all other keys on the prefix (in
    ;; motion state)
    (evilem-default-keybindings prefix)
    (evilem-define (kbd (concat prefix " n")) #'evil-ex-search-next)
    (evilem-define (kbd (concat prefix " N")) #'evil-ex-search-previous)
    (evilem-define (kbd (concat prefix " s")) #'evil-snipe-repeat
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))
    (evilem-define (kbd (concat prefix " S")) #'evil-snipe-repeat-reverse
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))))

;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.

(map! (:map input-decode-map
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; textmate-esque newline insertion
      :i [M-return]     #'evil-open-below
      :i [S-M-return]   #'evil-open-above
      ;; textmate-esque deletion
      [M-backspace]     #'doom/backward-kill-to-bol-and-indent
      :i [backspace]    #'delete-backward-char
      :i [M-backspace]  #'doom/backward-kill-to-bol-and-indent
      ;; Emacsien motions for insert mode
      :i "C-b" #'backward-word
      :i "C-f" #'forward-word

      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      :i "SPC"                          #'doom/inflate-space-maybe
      :i [remap delete-backward-char]   #'doom/deflate-space-maybe
      :i [remap newline]                #'doom/newline-and-indent

      (:after org-mode
        (:map org-mode-map
          :i [remap doom/inflate-space-maybe] #'org-self-insert-command
          :i "C-e" #'org-end-of-line
          :
          :i "C-a" #'org-beginning-of-line))

      ;; Restore common editing keys (and ESC) in minibuffer
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             evil-ex-completion-map
             evil-ex-search-keymap
             read-expression-map)
        [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register
        "C-a" #'move-beginning-of-line
        "C-w" #'doom/minibuffer-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom/minibuffer-undo)

      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      (:map tabulated-list-mode-map
        [remap evil-record-macro] #'doom/popup-close-maybe)

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))

;; https://emacs.stackexchange.com/questions/10856/how-do-i-set-up-key-bindings-for-modes-in-a-specific-evil-state
(evil-define-key 'normal web-mode-map
  (kbd "<tab>") 'web-mode-fold-or-unfold
  ;; <S-tab> doesn't work for shift-tab, needs to use <backtab>
  ;; (kbd "<S-tab>") 'web-mode-element-children-fold-or-unfold)
  (kbd "<backtab>") 'web-mode-element-children-fold-or-unfold
  (kbd "SPC ev") 'web-mode-element-content-select)

;; http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
(evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle
                                      (kbd "<return>") 'org-open-at-point)

(evil-define-key 'normal dired-mode-map
  (kbd "e") 'ora-ediff-files
  (kbd "SPC q") '+ivy/switch-workspace-buffer
  (kbd "SPC Q") 'ivy-switch-buffer
  (kbd "SPC SPC") 'counsel-M-x
  (kbd "SPC ff")  'counsel-find-file
  (kbd "SPC ws")  'evil-window-split
  (kbd "SPC bb")  'persp-switch-to-buffer
  (kbd "SPC nn")  '+xwu/find-in-notes
  (kbd "SPC pf")  'counsel-projectile-find-file
  (kbd "SPC hk")  'describe-key
  (kbd "SPC ll")  '+workspace/switch-to)

(evil-define-key 'normal nxml-mode-map (kbd "SPC xp") 'nxml-where)
(evil-define-key 'normal json-mode-map (kbd "SPC xp") 'jsons-print-path)
(define-key ivy-minibuffer-map (kbd "M-y") 'counsel-yank-pop)
(define-key evil-ex-map "\M-y" #'counsel-yank-pop)

;; (evil-define-key 'insert term-raw-map (kbd "C-c C-d") 'term-send-eof)
;; (evil-define-key 'insert term-raw-map (kbd "C-c C-z") 'term-stop-subjob)

;; (eval-after-load 'multi-term
;;   '(progn
;;      (dolist (p '(("C-p" . term-senjd-up)
;;                   f("C-n" . term-send-down)
;;                   ("C-s" . swiper)
;;                   ("C-r" . term-send-reverse-search-history)
;;                   ("C-m" . term-send-raw)
;;                   ("C-k" . term-send-kill-whole-line)
;;                   ("C-y" . yank)
;;                   ("C-_" . term-send-raw)
;;                   ("M-f" . term-send-forward-word)
;;                   ("M-b" . term-send-backward-word)
;;                   ("M-K" . term-send-kill-line)
;;                   ("M-p" . previous-line)
;;                   ("M-n" . next-line)
;;                   ("M-y" . yank-pop)
;; (setq term-bind-key-alist (delq (assoc (car p) term-bind-key-alist) jterm-bind-key-falist))
;; (add-to-list 'term-bind-key-alist p))))))
