;;; private/xwu157/+myxml.el -*- lexical-binding: t; -*-

;; use web-mode instead of nxml for xml
(add-to-list 'auto-mode-alist '("\\.xml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$" . web-mode))

(defun add-xml-tag-from-kill-ring ()
  (interactive)
  (let ((x (substring-no-properties (car kill-ring))))
    (insert (concat "<" x ">" "</" x ">"))
    (goto-char (- (point) (length (concat "</" x ">"))))
))

(defun add-xml-tag (input)
  (insert (concat "<" input ">" "</" input ">"))
  (insert "\n")
)

(defun add-xml-tag-from-whole-kill-ring ()
  (interactive)
  (mapcar 'add-xml-tag kill-ring)
)
