;;; private/xwu157/autoload/+ediff.el -*- lexical-binding: t; -*-

;; https://oremacs.com/2017/03/18/dired-ediff/
;; -*- lexical-binding: t -*-
;;;###autoload
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Commands-of-GUD.html

;; https://emacs.stackexchange.com/questions/3494/how-to-count-all-of-the-windows-in-a-frame
;;;###autoload
(defun unique-visible-buffers (&optional frame)
  (delete-dups (mapcar #'window-buffer (window-list frame))))

;;;###autoload
(defun count-unique-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown.  Defaults to
selected frame."
  (length (unique-visible-buffers)))

;;;###autoload
(defun me/quick-ediff-two-buffers()
  "quickly ediff two unique visible buffers in current frame"
  (interactive)
  (if (not (equal (count-unique-visible-buffers) 2))
      (message "should have two unique buffers for ediff.")
      (let ((buffer1 (buffer-name (car (unique-visible-buffers))))
           (buffer2 (buffer-name (car (last (unique-visible-buffers))))))
           (ediff-buffers buffer1 buffer2))))
