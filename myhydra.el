;; most hydra comes from https://github.com/sam217pa/emacs-config

(defhydra hydra-git
  (:body-pre (git-gutter-mode 1)
   ;; :post (progn (kill-diff-buffers)
   ;;              (message "killed diff buffers"))
   :hint nil)
  "
^NAV^               ^HUNK^            ^FILES^        ^ACTIONS^
  _n_: next hunk    _s_tage hunk      _S_tage        _c_ommit
  _p_: prev hunk    _r_evert hunk     _R_evert       _b_lame
  _g_: first hunk   _P_opup hunk      _d_iff         _C_heckout
  _G_: last hunk    _R_evision start  _t_imemachine
"
  ("n" git-gutter:next-hunk)
  ("p" git-gutter:previous-hunk)
  ("g" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)))
  ("G" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)))
  ("j" my-goto-git-gutter)
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("P" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("S" magit-stage-file)
  ("R" magit-revert)
  ("d" magit-diff-unstaged :color blue)
  ("t" git-timemachine :color blue)
  ("c" magit-commit :color blue)
  ("b" magit-blame)
  ("C" magit-checkout)
  ("v" magit-status "status" :color blue)
  ("q" nil "quit" :color blue)
  ("Q" (progn
         (git-gutter-mode -1)
         ;; git-gutter-fringe doesn't seem to
         ;; clear the markup right away
         (sit-for 0.1)
         (git-gutter:clear))
   "quit git-gutter"
   :color blue))

(defhydra hydra-window-enlarge (:hint nil)
  "
^SIZE^ ^^^^
^ ^ ^ ^ _S_ ^ ^
^ ^ ^ ^ _s_ ^ ^
_C_ _c_ ^ ^ _r_ _R_
^ ^ ^ ^ _t_ ^ ^
^ ^ ^ ^ _T_ ^ ^
^ ^ ^ ^ ^ ^ ^ ^
"
  ("s" (lambda () (interactive) (enlarge-window -1)))
  ("S" (lambda () (interactive) (enlarge-window -10)))
  ("c" enlarge-window-horizontally)
  ("C" (lambda () (interactive) (enlarge-window-horizontally 10)))
  ("r" (lambda () (interactive) (enlarge-window-horizontally -1)))
  ("R" (lambda () (interactive) (enlarge-window-horizontally -10)))
  ("t" (lambda () (interactive) (enlarge-window 1)))
  ("T" (lambda () (interactive) (enlarge-window 10)))
  ("q" nil :color blue))

(defhydra hydra-window
  (:hint nil
   :color amaranth
   :columns 4
   :pre (winner-mode 1)
   :post (balance-windows))
  "
^MOVE^ ^^^^   ^SPLIT^          ^SIZE^   ^COMMAND^   ^WINDOW^
^ ^ _s_ ^ ^   _-_ : split H    ^ ^     _d_elete    ^1^ ^2^ ^3^ ^4^
_c_ _E_ _r_   _|_ : split V    _e_     _m_aximize  ^5^ ^6^ ^7^ ^8^
^ ^ _t_ ^ ^   _h_ : split H    ^ ^     _u_ndo      ^9^ ^0^
^ ^ ^ ^ ^ ^   _v_ : split V    ^ ^     _R_edo
"
  ("c" windmove-left :color blue)
  ("r" windmove-right :color blue)
  ("t" windmove-down :color blue)
  ("s" windmove-up :color blue)

  ;; splt
  ("-" split-window-vertically)
  ("|" split-window-horizontally)
  ("v" split-window-horizontally :color blue)
  ("h" split-window-vertically :color blue)

  ;; size
  ("e" hydra-window-enlarge/body :color blue)

  ("u" winner-undo)
  ("R" winner-redo)
  ("m" delete-other-windows)
  ("d" delete-window)

  ;; change height and width
  ("0" select-window-0 :color blue)
  ("1" select-window-1 :color blue)
  ("2" select-window-2 :color blue)
  ("3" select-window-3 :color blue)
  ("4" select-window-4 :color blue)
  ("5" select-window-5 :color blue)
  ("6" select-window-6 :color blue)
  ("7" select-window-7 :color blue)
  ("8" select-window-8 :color blue)
  ("9" select-window-9 :color blue)

  ("=" balance-windows)
  ("E" ace-window)
  ("." hydra-buffer/body "buffers" :color blue)
  ("'" hydra-tile/body "tile" :color blue)
  ("q" nil "quit" :color blue))

(defhydra elpy-nav-errors (:color red)
  "
  Navigate errors
  "
  ("n" next-error "next error")
  ("p" previous-error "previous error")
  ("s" (progn
         (switch-to-buffer-other-window "*compilation*")
         (goto-char (point-max))) "switch to compilation buffer" :color blue)
  ("w" (venv-workon) "Workon venv…")
  ("q" nil "quit")
  ("Q" (kill-buffer "*compilation*") "quit and kill compilation buffer" :color blue)
  )

(defhydra elpy-hydra (:color red)
  "
  "
  ("d" (progn (call-interactively 'elpy-test-django-runner) (elpy-nav-errors/body)) "current test, Django runner" :color blue)
  ("t" (progn (call-interactively 'elpy-test-pytest-runner) (elpy-nav-errors/body)) "current test, pytest runner" :color blue)
  ("w" (venv-workon) "workon venv…")
  ("q" nil "quit")
  ("Q" (kill-buffer "*compilation*") "quit and kill compilation buffer" :color blue)
  )

(defhydra hydra-folding (:color red)
  "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
  ("o" origami-open-node)
  ("c" origami-close-node)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("f" origami-forward-toggle-node)
  ("q" nil "quit")
  ("a" origami-toggle-all-nodes))
