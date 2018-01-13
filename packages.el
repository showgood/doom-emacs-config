;; -*- no-byte-compile: t; -*-

(package! rtags)
(package! bookmark+ :recipe (:fetcher github :repo "emacsmirror/bookmark-plus"))
(package! eacl :recipe (:fetcher github :repo "redguardtoo/eacl"))
(package! dired+ :recipe (:fetcher github :repo "emacsmirror/dired-plus"))
(package! vlfi :recipe (:fetcher github :repo "m00natic/vlfi"))
(package! dash-at-point :recipe (:fetcher github :repo "stanaka/dash-at-point"))
(package! json-snatcher :recipe (:fetcher github :repo "Sterlingg/json-snatcher"))
(package! logview :recipe (:fetcher github :repo "doublep/logview"))
(package! beacon :recipe (:fetcher github :repo "Malabarba/beacon"))
(package! fancy-narrow :recipe (:fetcher github :repo "Malabarba/fancy-narrow"))
(package! solarized-theme)
(package! color-theme-sanityinc-tomorrow)
(package! zenburn-theme)
(package! monokai-theme)

(package! evil-magit :recipe (:fetcher github :repo "emacs-evil/evil-magit"))
(package! evil-ediff :recipe (:fetcher github :repo "emacs-evil/evil-ediff"))
(package! hl-anything :recipe (:fetcher github :repo "hl-anything/hl-anything-emacs"))
(package! engine-mode :recipe (:fetcher github :repo "hrs/engine-mode"))
(package! evil-replace-with-register :recipe (:fetcher github :repo "Dewdrops/evil-ReplaceWithRegister"))
;; (package! clang-format :recipe (:fetcher github :repo "sonatard/clang-format"))

(package! ob-ipython)
(package! origami)
(package! lentic)
(package! clean-aindent-mode)
(package! nov)
(package! org-ehtml)
(package! imenu-anywhere)
(package! move-text)
(package! pcre2el)
(package! evil-visual-mark-mode)
(package! auto-yasnippet)
(package! wand)

(package! deft)

;; https://github.com/phillord/lentic
;; https://github.com/wasamasa/nov.el

;; https://github.com/bmag/emacs-purpose

;; https://github.com/proofit404/anaconda-mode
;; https://github.com/jorgenschaefer/elpy

;; https://github.com/abo-abo/make-it-so
;; https://github.com/vspinu/imenu-anywhere
;; https://github.com/realgud/realgud
;; https://github.com/jorgenschaefer/pyvenv
;; https://github.com/paetzke/py-yapf.el
;; https://github.com/ionrock/pytest-el

;; https://github.com/google/yapf

;; not very useful, buffer disappeared after window close and can't get back
;; (package! scratch-el :recipe (:fetcher github :repo "ieure/scratch-el"))
;; seems have some issue with evil mode
;; (package! json-navigator :recipe (:fetcher github :repo "DamienCassou/json-navigator"))

;; not quite working for me, ob-ipython seems better
;; https://github.com/millejoh/emacs-ipython-notebook
;; (package! ein)
