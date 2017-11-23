;;; private/xwu157/autoload/+xml.el -*- lexical-binding: t; -*-

;;;###autoload
;; create a tag(open and close) with the top of kill-ring
;; for eg, if kill ring has ['ab', 'cd']
;; then this will create:
;; <ab></ab>
(defun add-xml-tag-from-kill-ring ()
  (interactive)
  (let ((x (substring-no-properties (car kill-ring))))
    (insert (concat "<" x ">" "</" x ">"))
    (goto-char (- (point) (length (concat "</" x ">"))))
))

;;;###autoload
(defun add-xml-tag (input)
  (insert (concat "<" input ">" "</" input ">"))
  (insert "\n")
)

;;;###autoload
;; create a bunch of tags(open and close) with all the things from kill ring
;; for eg, if kill ring has ['ab', 'cd']
;; then this will create:
;; <ab></ab>
;; <cd></cd>
(defun add-xml-tag-from-whole-kill-ring ()
  (interactive)
  (mapcar 'add-xml-tag kill-ring)
)
