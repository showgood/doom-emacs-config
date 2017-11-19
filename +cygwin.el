;;; private/xwu157/+cygwin.el -*- lexical-binding: t; -*-

(load! toolkit-tramp)
(require 'toolkit-tramp)
;; http://emacs.stackexchange.com/questions/27/how-can-i-use-my-local-emacs-client-as-the-editor-for-remote-machines-i-access
(require 'with-editor)

(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'term-mode-hook   'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)

(load! w32-browser)
(require 'w32-browser)
(load! +myFindFile)

(setq org-file-apps org-file-apps-defaults-windowsnt)

