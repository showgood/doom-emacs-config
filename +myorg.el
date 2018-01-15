;; -*- origami-fold-style: triple-braces -*-
;; (require 'org)

;; ==== org general settings {{{ ====
(setq org-directory "~/org")
(setq org-default-notes-file "~/org/Inbox.org")
(setq org-archive-location "~/org/logbook.org::* Archived")

(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
;; (setq org-image-actual-width (quote (500)))

;; always use relative path link, very important
;; https://emacs.stackexchange.com/questions/16652/change-the-behavior-of-org-mode-auto-expand-relative-path-in-link
(setq org-link-file-path-type 'relative)

(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=")
        ("google-map" . "http://maps.google.com/maps?q=%s")
        ("stack-exchange" . "https://emacs.stackexchange.com/questions")
        ))

;; make org mode recgonize link using pdf-tools
(eval-after-load 'org '(require 'org-pdfview))
(add-to-list 'org-file-apps
             '("\\.pdf\\'" . (lambda (file link)
                                     (org-pdfview-open link))))
;; ==== end org general settings }}} ====

;; ==== org agenda settings {{{ ====
(setq org-agenda-files '("~/org/gtd/"
                               "~/org/Inbox.org" ))

(setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                          (sequence "⚑ WAITING(w)" "|")
                          (sequence "|" "✘ CANCELED(c)")))

(setq org-agenda-start-on-weekday 0)
;; make agenda show on current window
(setq org-agenda-window-setup 'current-window)
;; highlight current in agenda
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

; Targets include this file and any file contributing to the agenda - up to 9
; levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path 'file)

;;;; ==== end org agenda settings }}} ====

;; ==== org babel settings {{{ ======
;; https://2li.ch/home/discovering-emacs-in-2017-part-2
;; enable the correct intdentation for source code blocks
(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)
;;; Preserve indentation in code blocks
(setq org-src-preserve-indentation t)

;; Use current window when switch to source block
(setq org-src-window-setup 'current-window)

(setq org-src-fontify-natively t)

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook
          'org-display-inline-images 'append)

;; Disable prompting to evaluate babel blocks
(setq org-confirm-babel-evaluate nil)

(setq org-ditaa-jar-path "~/soft/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/soft/plantuml.8033.jar")
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (perl . t)
    (ruby . t)
    (plantuml . t)
    (ditaa . t)
    (shell . t)
    (python . t)
    (dot . t)
    (C . t)
    (js . t)
    (ipython . t)
    (emacs-lisp . t)
    (scheme . t)
    ))
;;; === end org babel settings }}} ====

;; ==== org export settings {{{ =====
(setq org-html-table-default-attributes
      '(:border "2" :rules "all" :frame "border"))
(setq org-export-with-properties t)
(setq org-export-with-clocks t)

;; Disable add validation link when export to HTML
(setq org-html-validation-link nil)

;; do NOT treat _ as subscript and ^ as superscript
(setq org-export-with-sub-superscripts nil)

;; export to github flavor markdown
(require 'ox-gfm)

;;; ==== end org export settings }}} ====

;; ==== org capture templates {{{ ====
(setq org-capture-templates
      (quote (("t" "todo" entry (file "Inbox.org")
               "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n")
              ("r" "respond" entry (file "Inbox.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
              ("n" "note" entry (file "Inbox.org")
               "* %? :NOTE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n")
              ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
               "* %?\n%U\n")
              ;; ("w" "org-protocol" entry (file "~/org/Inbox.org")
              ;;  "* TODO Review %c\n%U\n" :immediate-finish t)
              ("w" "Web site" entry
               (file "")
               "* %a :website:\n\n%U %?\n\n%:initial")
              ("m" "Meeting" entry (file "Inbox.org")
               "* MEETING with %? :MEETING:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
              ("a" "Appointment" entry (file "Inbox.org")
               "* Appointment with %? :APPOINTMENT:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
              ("p" "Phone call" entry (file "Inbox.org")
               "* PHONE %? :PHONE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
              ("c" "Contacts" entry (file "contacts.org")
                "* %(org-contacts-template-name) \n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:END:\n")
              ("l" "Link" entry
                 (file "~/org/rss.org")
                 "* %a\n%U")
              ("h" "Habit" entry (file "Inbox.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
;; ==== end of org capture templates }}} ====

;; ==== keep this last {{{  ====
;; https://emacs.stackexchange.com/questions/5889/how-to-highlight-text-permanently-in-org-mode
;; for some reason, this line has to be kept last otherwise
;; will get symbol org-emphasis-alist is void error
;; (add-to-list 'org-emphasis-alist
;;              '("*" (:emphasis t :foreground "red")
;;                ))
;; === end of special section }}} ====

;; ==== obsolete section {{{ ====
;; http://dept.stat.lsa.umich.edu/~jerrick/org_agenda_calendar.html
;; (add-hook 'org-finalize-agenda-hook
;;           (lambda ()
;;             (save-excursion
;;               (color-org-header "Personal:"  "green")
;;               (color-org-header "Birthdays:" "gold")
;;               (color-org-header "Holidays:" "chocolate"))))

;; (defun color-org-header (tag col)
;;   ""
;;   (interactive)
;;   (goto-char (point-min))
;;   (while (re-search-forward tag nil t)
;;     (add-text-properties (match-beginning 0) (point-at-eol)
;;                          `(face (:foreground ,col)))))
; follow the link using enter
;; (setq org-return-follows-link t)

;; (require 'org-contacts)
;; (setq org-contacts-files (quote ("~/org/contacts.org")))

;;; this not work on mac, only on windows
;; http://orgmode.org/manual/Adding-hyperlink-types.html
;; (org-add-link-type "bbg" 'org-bbg-open)
;; (add-hook 'org-store-link-functions 'org-bbg-store-link)

;; This uses the BbgProtocolHandler set up by the terminal;
;; bbg: links should be supported
;; (defun org-bbg-open (link)
;;   "Launch the Bloomberg Terminal with the given function"
;;   (browse-url (concat "bbg://screens/" link)))

;; not working, not sure why
;; http://sachachua.com/blog/2012/12/emacs-strike-through-headlines-for-done-tasks-in-org/
;; (setq org-fontify-done-headline t)
;; (custom-set-faces
;;  '(org-done ((t (:foreground "PaleGreen"
;;                              :weight normal
;;                              :strike-through t))))
;;  '(org-headline-done
;;    ((((class color) (min-colors 16) (background dark))
;;      (:foreground "LightSalmon" :strike-through t)))))

;; ---------------------
;; org capture in elfeed
;; ---------------------
;; http://dsdshcym.github.io/blog/2016/01/28/add-org-store-link-entry-for-elfeed/
;; store the link to elfeed in org mode
;; (defun private/org-elfeed-entry-store-link ()
;;   (when elfeed-show-entry
;;     (let* ((link (elfeed-entry-link elfeed-show-entry))
;;            (title (elfeed-entry-title elfeed-show-entry)))
;;       (org-store-link-props
;;        :link link
;;        :description title)
;;       )))

;; (add-hook 'org-store-link-functions
;;           'private/org-elfeed-entry-store-link)

;; org-drill become really slow when org file is slightly large
;; not very useful compared to Anki
;(require 'org-drill)
;(setq org-drill-learn-fraction 0.3)
;; (setq org-drill-spaced-repetition-algorithm 'Simple8)
;; https://punchagan.muse-amuse.in/posts/org-drill-for-making-it-stick.html

;; export to Markdown
;; (require 'ox-md)

;; export to github flavor markdown
;; (require 'ox-gfm)

;; Setup files for agenda
;; Always use `C-g' to exit agenda
;; (add-hook 'org-agenda-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "C-g") 'org-agenda-exit)))

;; ==== end of obsolete section }}} ====

;; (eval-after-load "org"
;;   '(require 'ox-gfm nil t))
