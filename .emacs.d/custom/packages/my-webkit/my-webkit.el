;;; my-webkit.el --- fixes and functionalities for xwidget-webkit -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Andrew De Angelis ( bobodeangelis@gmail.com )
;; Version: 0.1.0
;; Created: 8 Oct 2022

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License version 3.

;;; Commentary:
;; most of these are minor fixes to (hopefully temporary)
;; bugs and issues in the official xwidget.el in Emacs 29.0.50
;; plus some useful functionalities to make it easier to
;; use a search engine and navigate browsing history

;;; Code:
(eval-when-compile (require 'xwidget))
(eval-when-compile (require 'eww))
(eval-when-compile (require 'image-mode))
(eval-when-compile (require 'csv-mode))

(defgroup my-webkit nil
  "Additional functionalities and fixes for xwidget-webkit."
  :group 'widgets) ; leisure/art/something like that?

(declare-function eww-current-url "eww.el")
(declare-function xwidget-webkit-current-session "xwidget.el")
(declare-function xwidget-webkit-buffer-kill "xwidget.el")
(declare-function xwidget-webkit-bookmark-make-record "xwidget.el")
(declare-function csv-mode "csv-mode.el")

;;;; web search
(defvar my-search-engine "search.brave.com")

;;;###autoload
(defun websearch (&optional new-session)
  "Prompt for a string to search for using `my-search-engine'.
Build a query string and run `xwidget-webkit-browse-url'
with the resulting url, and the optional NEW-SESSION argument"
  (interactive "P")
  (let ((url
	 (thread-last
	   (read-from-minibuffer
	    (format "use %s to search: " my-search-engine))
	   (string-replace " " "+")
	   (concat "https://" my-search-engine "/search?q="))))
    (message "xwidgeting this: %s" url)
    (xwidget-webkit-browse-url url new-session)))

;;;; eww integration
(defun my-xwidget-browse (&optional new-session)
  "From a `eww' session, open an xwidget session with the current url.
Use `xwidget-webkit-browse-url' with `eww-current-url' and NEW-SESSION"
  (interactive "P")
  (let ((url (eww-current-url)))
    (xwidget-webkit-browse-url url new-session)))

(advice-add 'eww-mode :after
	    (lambda ()
	      (define-key eww-mode-map "x" #'my-xwidget-browse)))


;;;; web history
(defvar web-history-file "~/.emacs.d/custom/datasets/web_history.csv")
(defvar web-history-file-header "day,time,title,url\n")
(defvar web-history-file-session-separator
  "__________,__________,__________,__________\n")
(defvar web-history-amt-days 60
  "Integer corresponding to the amount of days recorded in `web-history-file'.
Entries that are older than the current date minus this amount will be deleted")

(defun webkit-add-current-url-to-history (&optional arg)
  "Get the current url and add it to `web-history-file'.
Also add date, time, and widget title (when provided, use ARG for title)"
  (with-temp-file web-history-file
    (if (file-exists-p web-history-file)
	(insert-file-contents-literally web-history-file)
      (insert web-history-file-header))
    (let* ((title (or arg
		      (xwidget-webkit-title (xwidget-webkit-current-session))))
	   (url (xwidget-webkit-uri (xwidget-webkit-current-session)))
	   (time-as-list (split-string (current-time-string)))
	   (date
	    (string-join
	     (nconc (take 3 time-as-list) (last time-as-list)) " "))
	   (hour (nth 3 time-as-list))
	   (web-history-line (concat date "," hour "," title "," url "\n")))
      ;; (goto-char (point-min))
      (forward-line 1) ; add at the top, under the header: most recent first
      (insert web-history-line))))

(defun webkit-history-add-session-separator (&rest args)
  "Insert `web-history-file-session-separator' in `web-histoy-file'.
This helps visualize different sessions in the csv file.
ARGS are ignored, but included in the definition so that this
function can be added as advice before `xwidget-webkit-new-session'.
Side effect: delete old entries by calling `webkit-history-clear-older-entries'"
  (with-suppressed-warnings ((lexical args))
    (with-temp-file web-history-file
      (if (file-exists-p web-history-file)
	  (insert-file-contents-literally web-history-file)
	(insert web-history-file-header))
      (forward-line 1)
      (insert web-history-file-session-separator)
      (webkit-history-clear-older-entries))))

(advice-add 'xwidget-webkit-new-session :before
	    #'webkit-history-add-session-separator)

(defun webkit-history-clear-older-entries ()
  "Delete entries older than `web-history-amt-days' ago from `web-history-file'"
  (let* ((date
	  (thread-last
	    (* 60 60 24 web-history-amt-days)
	    (time-subtract (current-time))
	    (current-time-string)
	    (replace-regexp-in-string
	     " [[:digit:]][[:digit:]]:[[:digit:]][[:digit:]]:[[:digit:]][[:digit:]]" "")
	    (replace-regexp-in-string "[[:space:]]+" " "))))
    (if (search-forward-regexp date nil 'no-error)
	;; if found, delete every following entry,
	;; and the preceding newline
	(progn
	  (move-beginning-of-line 1)
	  (let ((start (point))
		(end (point-max)))
	    (delete-region start end)
	    (delete-char -1))))))

;;;###autoload
(defun my-webkit-display-web-history ()
  "Open `web-history-file' in another window."
  (interactive)
  (find-file-other-window web-history-file))

;;;;; web history mode
;; to display the history file
(defvar web-history-highlights
  '(
    ("," . 'csv-separator-face)
    ("[[:digit:]][[:digit:]]:[[:digit:]][[:digit:]]:[[:digit:]][[:digit:]]" .
     'font-lock-string-face)
    ("__________" . 'font-lock-builtin-face)
    ("https:.*" . 'link)))

(defun web-history-open-url (arg)
  "If called when point is at a link, open that url.
Else, parse the line at point to find the link, prompt for confirmation,
and open it.  ARG is used when calling `xwidget-webkit-browse-url',
as the value of NEW-SESSION"
  (interactive "P")
  (let ((at-point (thing-at-point 'sexp 'no-properties)))
    (if (string-match-p "http" at-point)
	(progn
	  (xwidget-webkit-browse-url at-point arg))
      (let* ((line (thing-at-point 'line 'no-properties))
	     (link (string-trim (substring line (string-match-p "[^,]+$" line))))
	     (url (read-from-minibuffer "xwidget-webkit URL: " link)))
	(xwidget-webkit-browse-url url arg)))))

(defun web-history-mouse-open-url (event)
  "If called when point is at a link, open that url.
Else, parse the line at point to find the link, prompt for confirmation,
and open it.  ARG is used when calling `xwidget-webkit-browse-url',
as the value of NEW-SESSION"
  (interactive "e")
  (mouse-set-point event)
  (let ((at-point (thing-at-point 'sexp 'no-properties)))
    (if (string-match-p "http" at-point)
        (xwidget-webkit-browse-url at-point))))

(defvar web-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'web-history-open-url)
    (define-key map (kbd "<mouse-3>") #'web-history-mouse-open-url)
    map))

;;;###autoload
(define-derived-mode web-history-mode csv-mode "web-history"
  "Major mode for displaying web history.
\\{web-history-mode-map}"
  (setq font-lock-defaults '(web-history-highlights))
  (setq-local buffer-read-only t)
  (auto-revert-mode 1))
(add-to-list 'auto-mode-alist '("web_history.csv" . web-history-mode))

;;;; xwidget configuration
;; overriding this function
(defun my-xwidget-log (&rest msg)
  "Log MSG to a buffer.  This overwrites `xwidget-log' in xwidget.el.
It adds the functionality that wen webkit finished loading,
the url is added to our browsing history
\(by calling `webkit-add-current-url-to-history').
Also the space at the beginning of the xwidget-log buffer-name
prevented the buffer from persisting for some reason"
  (let ((buf (get-buffer-create "*xwidget-log*")))
    (if (string-match-p (regexp-quote "webkit finished loading: %s") (car msg))
	(webkit-add-current-url-to-history
	 (nth 1 msg)))
    (with-current-buffer buf
      (insert (apply #'format msg))
      (insert "\n"))))

(advice-add 'xwidget-log :override #'my-xwidget-log)

(defun my-current-url ()
  "Display the current xwidget webkit URL and place it on the `kill-ring'.
This is used in place of `xwidget-webkit-current-url',
because that one uses `kill-new' improperly,
expecting it to return the just-killed string."
  (interactive nil xwidget-webkit-mode)
  (let ((url (or (xwidget-webkit-uri (xwidget-webkit-current-session)) "")))
    (kill-new url)
    (message "%s" url)))

;; eww is useful when text-navigation is needed
;; as it preserves all the emacs key-bindings
(defun eww-this ()
  "Open a new eww buffer with `my-current-url'."
  (interactive)
  (eww-browse-url (my-current-url)))

;; overriding this mode definition
(defun my-xwidget-webkit-fix-mode ()
  "Xwidget webkit view mode.
This overrides the original definition in xwidget.el.
Because it tried to call the undefined function
`xwidget-webkit-estimated-load-progress'."
  (define-key xwidget-webkit-mode-map "w" #'my-current-url)
  (define-key xwidget-webkit-mode-map "s" #'websearch)
  (define-key xwidget-webkit-mode-map "t" #'eww-this)
  (define-key xwidget-webkit-mode-map "H" #'my-webkit-display-web-history)
  (setq-local header-line-format
	      (list "WebKit: "
                    '(:eval
		      (xwidget-webkit-title (xwidget-webkit-current-session)))
		    ;; this will hopefully be fixed soon
                    ;; '(:eval
		    ;; 	(when xwidget-webkit--loading-p
                    ;;     (let ((session (xwidget-webkit-current-session)))
                    ;;       (format " [%d%%%%]"
                    ;;               (* 100
                    ;;                  (xwidget-webkit-estimated-load-progress
		    ;; 			session))))))
		    )))

(advice-add 'xwidget-webkit-mode :after #'my-xwidget-webkit-fix-mode)

;; add package to the `features' list
(provide 'my-webkit)
;;; my-webkit.el ends here
