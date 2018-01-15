;; -*- no-byte-compile: t; -*-

(package! rtags)
(package! bookmark+)
(package! dired+)
(package! vlf)
(package! engine-mode)
(package! eacl)
(package! beacon)
(package! fancy-narrow)
(package! dash-at-point)
(package! json-snatcher)
(package! evil-magit)
(package! evil-ediff)
(package! hl-anything)
(package! evil-replace-with-register)
(package! logview)
(package! ob-ipython)
(package! origami)
(package! lentic)
(package! clean-aindent-mode)
(package! nov)
(package! imenu-anywhere)
(package! move-text)
(package! pcre2el)
(package! evil-visual-mark-mode)
(package! auto-yasnippet)
(package! wand)
(package! deft)
(package! org-attach-screenshot)
(package! suggest)
(package! interleave)
(package! counsel-etags)
(package! counsel-dash)
(package! ox-gfm)
(package! pdf-tools)
(package! interleave)

(package! org-pdfview :recipe (:fetcher github :repo "markus1189/org-pdfview"))

;; (package! solarized-theme)
;; (package! color-theme-sanityinc-tomorrow)
;; (package! zenburn-theme)
;; (package! monokai-theme)
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

;; this does not work..
;;(package! org-ehtml)

;; couldn't get it installed on emacs 26
;; (package! org-preview-html)
;; (package! org-preview-html :recipe (:fetcher github :repo "lujun9972/org-preview-html"))
