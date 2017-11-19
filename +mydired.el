;;; private/xwu157/+mydired.el -*- lexical-binding: t; -*-

;; http://oremacs.com/2015/01/13/dired-options/
;;http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
;; not working for mac
;; (setq dired-listing-switches "-lah")

;; this --group-directories-first doesn't work on mac os natively,
;; also -G option doesn't work
;; (setq dired-listing-switches "-laGh1v --group-directories-first")
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
;; Make df output in dired buffers easier to read
(setq dired-free-space-args "-Pm")

;; Try suggesting dired targets
(setq dired-dwim-target t)

;; Understand .zip the way it does tarballs, letting the Z key decompress it:
;; Handle zip compression
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(defun xah-dired-sort () "Sort dired dir listing in different ways. Prompt for a choice.
  URL `http://ergoemacs.org/emacs/dired_sort.html' Version 2015-07-30"
       (interactive)
       (let (ξsort-by ξarg)
         (setq ξsort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
         (cond ((equal ξsort-by "date") (setq ξarg "-alht"))
               ((equal ξsort-by "size") (setq ξarg "-alhS"))
               ((equal ξsort-by "extension") (setq ξarg "-alhX"))
               ((equal ξsort-by "reverse name") (setq ξarg "-alhr"))
               ((equal ξsort-by "reverse size") (setq ξarg "-alhrS"))
               ((equal ξsort-by "reverse date") (setq ξarg "-alhrt"))
               ((equal ξsort-by "dir") (setq ξarg "-aBhl --group-directories-first"))
               (t (error "logic error 09535" )))
         (dired-sort-other ξarg )))

;; http://pragmaticemacs.com/emacs/quickly-preview-images-and-other-files-with-peep-dired/
;; (load "~/dotspacemacs/peep-dired/peep-dired.el")
;; (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
;;   (kbd "C-<SPC>") 'peep-dired-scroll-page-up
;;   (kbd "<backspace>") 'peep-dired-scroll-page-up
;;   (kbd "j") 'peep-dired-next-file
;;   (kbd "k") 'peep-dired-prev-file)
;; (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "pdf" "dmg"))
;; (setq peep-dired-cleanup-on-disable t)

;; https://oremacs.com/2015/01/12/dired-file-size/
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

;; enable it later
;; (define-key dired-mode-map (kbd "z") 'dired-get-size)
