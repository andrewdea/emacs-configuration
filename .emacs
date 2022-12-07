;;; .emacs --- init file at HOME  -*- lexical-binding: t; -*-
;; Copyright (C) 2022  Andrew De Angelis

;; Author: Andrew De Angelis <andrewdeangelis@Andrews-MacBook-Air.local>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
;;;; NATIVE COMPILATION
;;;;; don't show compilation warnings
;; keeping warnings on for now to monitor native comp til I'm familiar with it
;; (setq native-comp-async-report-warnings-errors nil)

;;;; PACKAGES setup
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-defer t)
  (require 'use-package-ensure))

;;;; PATH from shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;; PERFORMANCE
;;;;; garbage collection
(defun gc-restore-defaults ()
  (setq gc-cons-threshold 800000
	gc-cons-percentage 1.0)
  (gcmh-mode -1))

;; note that GC was essentially turned off during start up
;; use gcmh to reset garbage collection
(use-package gcmh
  :hook
  (after-init . gcmh-mode)
  :config
  (setq gcmh-idle-delay 'auto  ; default is 15s
	gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
	gcmh-verbose nil
	gcmh-auto-idle-delay-factor 10))

;; defer gc while in the minibuffer
(defun config:defer-gc ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun config:-do-restore-gc ()
  (setq gc-cons-threshold 16777216))
(defun config:restore-gc ()
  (run-at-time 1 nil #'config:-do-restore-gc))

(add-hook 'minibuffer-setup #'config:defer-gc)
(add-hook 'minibuffer-exit #'config:restore-gc)

;;;;; monitoring init
;; ;; check which packages are slow to load/config
;; (setq use-package-verbose t
;;       use-package-minimum-reported-time 0.005)

;; ;; check where init is slow to load
;; (use-package benchmark-init
;;   :demand t
;;   :init
;;   ;; To disable collection of benchmark data after init is done.
;;   ;; third arg is DEPTH: 100 means FUNCTION is added at the end of the hook list
;;   (add-hook 'after-init-hook #'benchmark-init/deactivate 100))

;;;; local PACKAGES and functionalities
(use-package all-the-haikus
  :load-path "custom/packages/all-the-haikus/"
  :defer 1)

;; useful for when I'm working on my own packages and need to update
(defun reload-package-from-file (&optional arg)
  (interactive "spackage name: ")
  (delete-package-quietly (intern arg))
  (call-interactively 'package-install-file))

(defun delete-package-quietly (arg)
  (condition-case nil
      (package-delete (cadr (assq arg package-alist)))
    (error (message "error while deleting, most likely had already deleted"))))

;;;; appearance: SIZING, FRAMES, WINDOWS, THEMES
;;;;; startup
;; choose default theme based on time of day
(defun default-theme ()
  (let ((hour (nth 2 (decode-time (current-time)))))
    (if (or (> hour 21) (< hour 8))
	#'cyberpunk ; at night
      #'my-monokai))) ; during the day

;; my daily default theme is based on standard tango-dark;
;; with some small edits in ~/.emacs.d/tango-dark-theme.el
;; I also really like monokai:
;; made some edits in ~/.emacs.d/my-monokai-theme.el

;; resize current frame (toggle)
(defalias #'big-frame #'toggle-frame-maximized)

(use-package mood-line)

(defun startup-look ()
  "Set (or restore) the initial appearance."
  (interactive)
  (setq column-number-mode t)
  (un-theme)
  (funcall (default-theme))
  ;; if not already maximized, maximize
  (or (eq (frame-parameter (selected-frame) 'fullscreen) 'maximized)
      (toggle-frame-maximized))
  (mood-line-mode t)
  (scroll-bar-mode -1)
  (global-visual-line-mode t)
  (set-fringe-style '(2 . nil))
  (pixel-scroll-precision-mode t)
  (setq-default indent-tabs-mode nil)
  (setq blink-cursor-blinks 5))

(add-hook 'after-init-hook #'startup-look -99)

(setq use-short-answers t)
;;;;; dashboard
(use-package dashboard
  :init
  (defun my-dashboard-init ()
    (setq dashboard-init-info
          (format "Emacs started in %s seconds." (emacs-init-time "%.2f"))))

  (autoload ; avoid loading the whole dashboad package unless needed
    #'dashboard-insert-startupify-lists
    "~/.emacs.d/elpa/dashboard-20221121.1809/dashboard.el")

  (defun dashboard-open ()
    "Open the *dashboard* buffer."
    (interactive)
    (my-dashboard-init)
    (let ((time (current-time))
	  ;; Refresh dashboard buffer or just switch to it?
	  (already-open (and (boundp 'dashboard-buffer-name)
			     (get-buffer dashboard-buffer-name))))
      (if (not already-open)
	  (progn
	    ;; make sure you autoload this function (see above):
	    (make-it-quiet
             (dashboard-insert-startupify-lists))
	    (message
	     "Welcome! Dashboard opened in %.2f seconds"
	     (float-time (time-since time)))))
      (switch-to-buffer dashboard-buffer-name)))

  ;; (add-hook 'after-init-hook (lambda ()
  ;;                              (or initial-buffer-choice (cdr command-line-args)
  ;;                                  (dashboard-open)))
  ;;           99)

  :defer 4

  :config
  (add-hook 'dashboard-mode-hook (lambda () (projectile-mode +1)))

  (add-hook 'dashboard-mode-hook
            (lambda ()
	      (local-set-key (kbd "C-<return>")
			     #'show-haiku-from-line-at-point)))

  (setq dashboard-footer-icon
	(all-the-icons-octicon "book"
			       :height 1.1
			       :v-adjust -0.05
			       :face 'font-lock-keyword-face))
  (defun center-and-propertize-haiku-line (line)
    (dashboard-insert-center (propertize line 'face 'dashboard-footer) "\n"))

  (defun dashboard-insert-footer ()
    "Insert custom haiku-footer for dashboard."
    (let ((footer (car dashboard-footer-messages))
	  (footer-heading "Today's haiku:\n"))
      (insert "\n\n")
      (dashboard-insert-center dashboard-footer-icon " "
                               (propertize footer-heading
                                           'face 'dashboard-heading))
      (mapcar #'center-and-propertize-haiku-line (split-string footer "\n"))
      (insert "\n")))

  (defun find-or-write-haiku ()
    (if (nth (random 2) (list nil t))
	(find-me-a-haiku)
      (write-me-a-haiku)))

  (setq dashboard-items '((recents  . 10)
                          (projects . 5)
			  (bookmarks . 5)
                          (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-week-agenda t)
  (setq dashboard-agenda-release-buffers t)
  (setq dashboard-footer-messages (list (find-or-write-haiku)))
  (setq dashboard-startup-banner 'logo))

;;;;; resizing and movement
;; make current window bigger or smaller
(defun wbig (&optional arg)
  (interactive "P")
  (let ((arg (or arg 25)))
    (enlarge-window-horizontally arg)
    (message "expanded window by %s columns" arg)))

(defun wsmall (&optional arg)
  (interactive "P")
  (let ((arg (or arg 25)))
    (shrink-window-horizontally arg)
    (message "reduced window by %s columns" arg)))

;; frame to have together with max youtube
(defun yt-frame ()
  (interactive)
  (delete-other-windows)
  (toggle-frame-maximized)
  (set-frame-position (selected-frame) 838 24)
  (set-frame-size (selected-frame) 84 54))

(defun right-frame ()
  (interactive)
  (let ((available-width
	 (nth 3 (nth 1 (nth 0 (display-monitor-attributes-list))))))
    (set-frame-position
     (selected-frame) (- available-width (frame-outer-width)) 0)))

(defun left-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0))

(defun delete-window-above ()
  (interactive)
  (windmove-up)
  (delete-window))
(defun delete-window-below ()
  (interactive)
  (windmove-down)
  (delete-window))

(global-set-key (kbd "C-x <up>") #'delete-window-above)
(global-set-key (kbd "C-x <down>") #'delete-window-below)

;;;;; themes and colors
(defun un-theme (&optional arg)
  "Disable all custom themes ad load theme ARG."
  (interactive "snew theme: ")
  (mapc #'disable-theme custom-enabled-themes)
  (if (> (length arg) 0) (load-theme (intern arg))))

(defun my-misterioso ()
  (interactive)
  (un-theme "my-misterioso"))
(defun my-monokai (&optional arg)
  (interactive "P")
  (un-theme)
  (if (not arg)
      (load-theme 'cyberpunk))
  (load-theme 'my-monokai))
(defun tango-dark ()
  (interactive)
  (un-theme "tango-dark"))
(defun cyberpunk  ()
  (interactive)
  (un-theme "cyberpunk"))

;; this highlights characters beyond the 80 char limit
(use-package whitespace
  :init
  (defun wspace ()
    "Shortcut for (whitespace-mode 'toggle)"
    (interactive)
    (let ((action-taken
	   (if (whitespace-mode 'toggle)
	       "enabled"
	     "disabled")))
      (message "Whitespace mode %s in this buffer" action-taken)))
  :config
  (setq whitespace-style '(face lines-tail trailing)))

;; long line to test whitespace-mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; icons
(use-package all-the-icons)

;;;;; appearance for specific modes

(add-hook 'package-menu-mode-hook
	  (lambda ()
	    (hl-line-mode t)
	    (visual-line-mode -1)
	    (setq-local truncate-lines t)))

(add-hook 'csv-mode-hook
	  (lambda () (visual-line-mode -1) (setq-local truncate-lines t)))

(add-hook 'comint-mode-hook
	  (lambda () (visual-line-mode -1) (electric-pair-local-mode t)))

;; (define-key help-mode-map "b" #'help-go-back)
;; (define-key help-mode-map "f" #'help-go-forward)
(use-package help-mode
  :init
  (setq help-window-select t)
  :bind
  (:map help-mode-map
        ("b" . help-go-back)
        ("f" . help-go-forward)))

;;;; ORG mode
(use-package org
  :defer 5
  :config
  ;; this is a nice feature
  ;; but it can slow emacs down with certain optimized JIT-lock settings
  (setq org-hide-emphasis-markers t)

  ;; better bullet-points
  (font-lock-add-keywords #'org-mode
                          '(("^ +\\([-]\\) "
                             (0 (prog1 ()
				  (compose-region (match-beginning 1)
						  (match-end 1) "•"))))))

  (setq org-log-done t)
  (setq org-agenda-files '("~/org/TODO.org" "~/org/ToBuy.org"))

  (defun my-org-tab ()
    "if current line is a heading, call regular org-cycle;
else, first move to previous visible heading, then call it"
    (interactive)
    (move-beginning-of-line 1)
    (if (null (looking-at org-outline-regexp))
	(org-previous-visible-heading 1))
    (org-cycle))

  (defun open-file-same-window () (interactive)
	 (setf (cdr (assoc 'file org-link-frame-setup))
	       #'find-file)
	 (org-open-at-point)
	 (setf (cdr (assoc 'file org-link-frame-setup))
	       #'find-file-other-window))

  ;; tinkering with this to try to make it not clash with delete-selection-mode
  (defun electric-fontify-will-use-region-p ()
    (setq to-fontify
	  (let ((fontify-list '(?* ?/ ?_ ?= ?~ ?+)))
	    (and (derived-mode-p 'org-mode) mark-active
		 (member last-command-event fontify-list)))))

  (add-hook 'self-insert-uses-region-functions #'electric-fontify-will-use-region-p)

  (defun electric-fontify ()
    "If in org-mode (or a derived mode),
when a region is highlighted and we've inserted a character that fontifies text,
the whole region is fontified (by automatically inserting character at mark)"
    (if (and (boundp 'to-fontify) to-fontify)
	(progn (exchange-point-and-mark)
	       (insert last-command-event)
	       (makunbound 'to-fontify))))
  (add-hook 'post-self-insert-hook #'electric-fontify)

  :hook
  (org-mode . org-indent-mode)
  (org-mode . turn-on-flyspell)
  :bind (("C-c s" . org-store-link)
	 ("C-c l" . org-insert-link)
	 ("C-c a" . org-agenda)
	 :map org-mode-map
	 ("M--" . org-timestamp-down-day)
	 ("M-_" . org-timestamp-down-day)
	 ("M-+" . org-timestamp-up-day)
	 ("C-c o" . open-file-same-window)
	 ("C-M-<backspace>" . org-cut-subtree)
	 ("TAB" . my-org-tab)))

(use-package org-roam
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-database-connector 'sqlite-builtin)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)))

;;;; FILE utilities
;;;;; dialog boxes
(setq use-file-dialog nil)
(setq use-dialog-box nil)
;;;;; open file properly at startup
;; this is already handled properly if the file was passed as CLI argument
;; but if Emacs was opened through the GUI, we have to check the ns-input-file
(add-hook 'after-init-hook
          (lambda ()
            (let ((to-emacs "to Emacs"))
              ;; ensure that ns-input-file is available
              ;; not sure why, but somehow
              ;; this is achieved by putting it in a format string
              (message "ns-input-file: %s" ns-input-file)
              (message "Welcome %s" to-emacs))
            (setq initial-buffer-choice (car ns-input-file))
            )
          98)
;;;;; load faster
;; these are useful especially in VERY large files
(defun jit-lock-optimize-settings ()
  (interactive)
  (setq jit-lock-defer-time 0.05))

(defun jit-lock-default-settings ()
  (interactive)
  (setq jit-lock-defer-time nil))

;; don't rm files with just a couple key-presses
(setq delete-by-moving-to-trash t)

;;;;; shortcuts
;; open init file
(defun init ()
  (interactive)
  (find-file user-init-file))

;; open HackerRank
(defun hacker-rank ()
  (interactive)
  (dired "~/desktop/HackerRankProblems/"))

;; open zsh profile
(defun zshenv ()
  (interactive)
  (find-file "/Users/andrewdeangelis/.zshenv"))
;; open bash profile
(defun bash-profile () ; note: I switched shell to zsh, this might no longer be needed
  (interactive)
  (find-file "/Users/andrewdeangelis/.bash_profile"))

;; open org folder
(defun forg ()
  (interactive)
  (dired "~/org"))
;; open mobile org folder
(defun beorg ()
  (interactive)
  (dired
   "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"))
;; open generic todo
(defun todo ()
  (interactive)
  (find-file "~/org/TODO.org"))
;; open generic notes
(defun notes ()
  (interactive)
  (find-file "~/org/Notes.org"))
;; open my custom modes
(defun my-modes ()
  (interactive)
  (dired "~/.emacs.d/custom/modes"))
;; open Crafting Interpreters
(defun crafting-interpreters ()
  (interactive)
  (dired "~/CraftingInterpreters"))


(defun shell-command-open (arg &optional options)
  (interactive "Sopen ")
  (let ((options (or options "")))
    (shell-command (message "open %s %s" options arg))
    (with-current-buffer "*Shell Command Output*"
      (if (eq (buffer-size) 0)
          (kill-buffer)))))

(defun reveal-in-finder (arg)
  "Use the shell command \"open -R ARG\" to select file in Finder."
  (interactive (list (ido-read-file-name "open in Finder: ")))
  (shell-command-open arg "-R"))

(defun open-in-browser (url)
  (interactive
   (let ((uris (eww-suggested-uris)))
     (list (read-string (format-prompt "Enter URL or keywords"
                                       (and uris (car uris)))
                        nil 'eww-prompt-history uris))))
  ;; (message "arg: %s" url))
  (let ((url (eww--dwim-expand-url url)))
    (shell-command-open url)))

;;;;; dired mode
(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  :bind
  (:map dired-mode-map
	("<mouse-2>" . dired-mouse-find-file))
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
	("<tab>" . dired-subtree-toggle)
	("<backtab>" . dired-subtree-cycle)))

;;;;; recent files
(use-package recentf
  :init

  (defun my-recentf-open-files ()
    (interactive)
    (recentf-mode +1)
    (recentf-open-files))

  :defer 3

  :bind* (("C-c C-r" . my-recentf-open-files))
  :config
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (add-to-list 'recentf-exclude "ido.last")
  (make-it-quiet
   (recentf-mode))
  :hook (recentf-dialog-mode . hl-line-mode))

;;;;; projectile mode
(use-package projectile
  :defer 6
  :config
  (setq projectile-ignored-projects '("~/")
        projectile-switch-project-action #'projectile-dired-other-window)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)))

;;;; TEXT EDITING and keyboard commands
;;;;; keys
;; allow more flexibility by binding the right-side cmd-key to C-
(setq ns-right-command-modifier 'control)
;;;;; utilities
(defun dwim-move-beginning-of-line (&optional arg)
  "Move point back to indentation of beginning of line.
`move-beginning-of-line' but smarter.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (let ((arg (or arg 1)))
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg)))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1)))
  (point))

;; remap C-a to 'dwim-move-beginning-of-line'
(global-set-key (kbd "\C-a") #'dwim-move-beginning-of-line)

;; when typing in selected region, delete it
(delete-selection-mode t)

(defun my-kill-whole-line (&optional arg)
  "Delete the line and the preceding 1 char.
This assumes that the preceding char is a newline,
thus bringing the point to the end of the previous line.
With positive ARG, delete whole of current line,
delete following ARG lines and preceding 1 char.
With negative ARG, delete current line *only up to current point*,
delete preceding ARG lines and preceding 1 char."
  (interactive "P")
  (if (or (not arg) (> arg 1))
      (move-beginning-of-line 1))
  (kill-line arg)
  (delete-char -1))

(defun dwim-kill-line (&optional arg)
  (interactive "P")
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (my-kill-whole-line arg)))

(global-set-key (kbd "C-k") #'dwim-kill-line)

(defun dwim-copy ()
  (interactive)
  (if (not mark-active)
      (let ((orig-point (point)))
	(kill-ring-save (dwim-move-beginning-of-line)
			(line-end-position))
	(goto-char orig-point)
        (message "%s %s"
                 (propertize "copied current line:" 'face 'minibuffer-prompt)
                 (current-kill 0)))
    (kill-ring-save (region-beginning) (region-end))))

(global-set-key (kbd "M-w") #'dwim-copy)

(defun dwim-kill ()
  (interactive)
  (if (not mark-active)
      (progn
        (kill-region (dwim-move-beginning-of-line)
		     (line-end-position))
        (message "%s %s"
                 (propertize "copied line:" 'face 'minibuffer-prompt)
                 (current-kill 0)))
    (kill-region (region-beginning) (region-end))))

(global-set-key (kbd "C-w") #'dwim-kill)

(defun total-visible-lines ()
  (interactive)
  (message "Buffer '%s' has %d total lines"
	   (buffer-name (current-buffer))
	   (count-lines (point-min) (point-max))))

(global-set-key (kbd "C-x l") #'goto-line)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; disable mouse-wheel-text-scale, as it can get in the way and is rarely needed
(defun my-scroll (e)
  (interactive "e")
  (if (and (boundp 'pixel-scroll-precision-mode)
	   pixel-scroll-precision-mode)
      (pixel-scroll-precision e)
    (mwheel-scroll e)))

(global-set-key (kbd "C-<wheel-down>") #'my-scroll)
(global-set-key (kbd "C-<wheel-up>") #'my-scroll)

(use-package avy
  :bind (("s-'" . avy-goto-char-2)))

;;;;; visual undo
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (add-hook 'vundo-pre-enter-hook
            (lambda ()
              (garbage-collect-maybe 5)
              (setq gc-cons-threshold most-positive-fixnum)))
  (advice-add 'vundo :after
              (lambda ()
                (setq gc-cons-threshold 16777216)))) ; 16mb

;;;;; flyspell
(use-package flyspell
  :bind (:map flyspell-mode-map
	      ("<mouse-3>" . flyspell-correct-word)
	      ("C-c f" . flyspell-correct-word-before-point)))

;;;;; search
;; from beginning of document
(global-set-key (kbd "M-s")
		(lambda () (interactive)
		  (point-to-register 'r)
		  (goto-char (point-min)) (isearch-forward)))

;; from end of document
(global-set-key (kbd "M-r")
		(lambda () (interactive)
		  (point-to-register 'r)
		  (goto-char (point-max)) (isearch-backward)))

;; get back to where search started
;; not sure why I have to press this twice
(global-set-key (kbd "M-b")
		(lambda () (interactive)
		  (jump-to-register 'r)))

;;;;; comments
;; copy line/region and comment it out
(defun region-copy-comm ()
  (interactive)
  (kill-ring-save nil nil 'region)
  (comment-dwim nil)
  (message "(un)commented region has been copied"))
(defun line-copy-comm ()
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position))
  (comment-line 1)
  (message "(un)commented line has been copied"))
(defun copy-comm ()
  (interactive)
  (if mark-active
      (region-copy-comm)
    (line-copy-comm)))

;; if I call comment with no active region, comment the line
(defun smart-comment ()
  (interactive)
  (if mark-active
      (comment-dwim nil)
    (comment-line 1)))
;; remap M-; to `smart-comment'
(global-set-key (kbd "M-;") #'smart-comment)

(defun query-comment-out (arg)
  (interactive "sstring to comment out: ")
  (query-replace arg (concat comment-start arg)))

(defun query-uncomment (arg)
  (interactive "sstring to uncomment: ")
  (let ((commented-arg (concat comment-start arg)))
    (query-replace commented-arg
		   (string-replace comment-start "" commented-arg))))

(defun comment-out-all (arg)
  (interactive "sstring to comment out: ")
  (while (search-forward arg nil 'noerror)
    (replace-match (concat comment-start arg)))
  (message "commented-out all lines starting with %s" arg))

(defun uncomment-all (arg)
  (interactive "sstring to uncomment: ")
  (let ((commented-arg (concat comment-start arg)))
    (while (search-forward commented-arg nil 'noerror)
      (replace-match (string-replace comment-start "" commented-arg)))
    (message "uncommented all lines starting with %s" commented-arg)))

;;;;; autocomplete
(use-package company
  :init
  (global-company-mode t)
  :config
  (setq company-dabbrev-downcase nil)
  :bind (:map company-mode-map
	      ("s-RET" . company-abort)))

;;;;; macros
(defun save-macro (name)
  "Save the current macro as named function definition
inside your init file so you can reuse it anytime in the
future."
  (interactive "SSave Macro as: ")
  (name-last-kbd-macro name)
  (save-excursion
    (init)
    (goto-char (point-max))
    (insert "\n\n;; Saved macro\n")
    (insert-kbd-macro name)
    (insert "\n")))

;;;;; god mode
(use-package god-mode
  :config
  (advice-add #'mood-line-segment-major-mode :filter-return
	      (lambda (arg)
		(if (bound-and-true-p god-local-mode)
		    (concat "💪⚡" arg)
		  arg)))
  :bind
  ("<escape>" . god-mode-all)
  ("s-<escape>" . god-local-mode)
  ("M-<escape>" . god-execute-with-current-bindings))


;;;; BUFFER AND FRAME movements
(use-package ibuffer
  :config
  (setq ibuffer-directory-abbrev-alist
        (list
         (cons "^/Users/andrewdeangelis" "~")
         (cons "^/Applications/Emacs.app/Contents/Resources/lisp" "Emacs.app/.*/lisp")))
  :bind (("C-x C-b" . ibuffer)
	 :map ibuffer-name-map
	 ("<mouse-1>" . ibuffer-visit-buffer)))

(use-package all-the-icons-ibuffer
  :after (all-the-icons ibuffer)
  :config
  ;; increase size of 'name' column
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (setq all-the-icons-ibuffer-formats
        `((mark modified read-only ,(if (>= emacs-major-version 26) 'locked "")
		;; Here you may adjust by replacing :right with :center or :left
		;; According to taste, if you want the icon further from the name
		" " ,(if all-the-icons-ibuffer-icon
			 '(icon 2 2 :left :elide)
                       "")
		,(if all-the-icons-ibuffer-icon
		     (propertize " " 'display `(space :align-to 8))
		   "")
		(name 27 27 :left :elide)
		" " (size-h 9 -1 :right)
		" " (mode+ 16 16 :left :elide)
		" " filename-and-process+)
	  (mark " " (name 16 -1) " " filename))))
(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)

(global-set-key (kbd "M-m") 'switch-to-minibuffer)

;; use S-right and S-left to navigate buffers
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; turn off M-i = TAB in minibuffer
(define-key minibuffer-local-map (kbd "M-i") "i")

;; find buffer by substring and switch to it
(defun buffer-name-matchp (arg-s arg-b)
  (string-match-p arg-s (buffer-name arg-b)))
(defun get-matching-buffers (arg)
  "Find all buffers whose name contains ARG."
  (seq-filter
   (apply-partially #'buffer-name-matchp arg) (buffer-list)))

;;;; SPEEDBAR & TREEMACS
(use-package sr-speedbar
  :init
  (defun bar-toggle ()
    (interactive)
    (sr-speedbar-toggle))
  (defun bar-open ()
    (interactive)
    (sr-speedbar-open))
  :config
  (defun bar-close ()
    (interactive)
    (sr-speedbar-close))
  (defun bar-refresh ()
    (interactive)
    (sr-speedbar-refresh-turn-on))

  (setq speedbar-show-unknown-files t)

  ;; expand functionalities:
  ;; these 3 functions, plus the built-in
  ;; speedbar-expand-line-descendants & speedbar-contract-line
  ;; allow you to navigate the directory tree in multiple ways
  (defun depth-expand ()
    "Open the directory at line, and keep opening its first subdirectory
until we reach a directory with no subdirectories"
    (interactive)
    (setq line-move-visual nil)
    (point-to-register 'sr)
    (setq expanded-by 'depth)
    (let ((dir-regexp "\\([0-9]:\\)*\\s-*<\\(-\\|\\+\\)>.*"))
      (move-beginning-of-line 1)
      (while (looking-at dir-regexp)
	(speedbar-expand-line)
	(forward-line 1)
	(move-beginning-of-line 1))))

  (defun breadth-expand ()
    "Open the directory at line, and open all its subsequent siblings
(directories that are at its same depth)"
    (interactive)
    (message "expanding by breadth")
    (point-to-register 'sr)
    (setq expanded-by 'breadth)
    (move-beginning-of-line 1)
    (let ((dir-regexp "\\([0-9]:\\)*\\s-*<\\(-\\|\\+\\)>.*"))
      (while (looking-at dir-regexp)
	(speedbar-expand-line)
	(speedbar-restricted-move 1)
	(move-beginning-of-line 1)))
    (message "opened all directories at this level"))

  (defun breadth-contract ()
    "Collapse the directory at line, and close all its subsequent
open siblings (directories at its same depth)"
    (interactive)
    (message "closing all directories at this level")
    (move-beginning-of-line 1)
    (let ((dir-regexp "\\([0-9]:\\)*\\s-*<\\(-\\|\\+\\)>.*"))
      (while (looking-at dir-regexp)
	(speedbar-contract-line)
	(speedbar-restricted-move 1)
	(move-beginning-of-line 1))))

  (defun my-speedbar-expand (&optional arg)
    "call depth-expand. With prefix argument (C-u), call breadth-expand"
    (interactive "P")
    (if arg
	(breadth-expand)
      (depth-expand)))

  (defun my-speedbar-undo ()
    "undo the latest breadth or depth expansion. has no concept of undo tree"
    (interactive)
    (jump-to-register 'sr)
    (cond ((eq expanded-by 'breadth)
	   (breadth-contract)
	   (jump-to-register 'sr)
	   (message "contracted by breadth and jumped to register"))
	  ((eq expanded-by 'depth)
           (speedbar-contract-line)
	   (message "jumped to register and contracted by depth"))))

  :bind (:map speedbar-mode-map
	      ("C-<return>" . my-speedbar-expand)
	      ("M-<down>" . speedbar-restricted-next)
	      ("M-<up>" . speedbar-restricted-prev)
	      ("C-x u" . my-speedbar-undo)))

;;;;; treemacs
(use-package treemacs
  :load-path "/Users/andrewdeangelis/treemacs_paste_feature/treemacs/src/elisp"
  :init
  (defalias #'tm #'treemacs)

  ;; for the following functions to work properly, add this:
  ;; ["Paste here"
  ;;  treemacs--paste-point-to-minibuffer
  ;;  :visible ,(or
  ;;             (equal (minibuffer-prompt) "Move to: ")
  ;;             (equal (minibuffer-prompt) "Copy to: "))]
  ;; to the menu created in `treemacs-rightclick-menu'
  ;; (defun paste-to-minibuffer (&optional arg)
  ;;     "Clear the minibuffer and insert ARG.
  ;; If ARG not provided, get it from the kill ring"
  ;;     (switch-to-minibuffer)
  ;;     (widen)
  ;;     (mark-whole-buffer)
  ;;     (delete-region (region-beginning) (region-end))
  ;;     (insert (or arg (current-kill 0))))

  ;;   (defun treemacs--paste-point-to-minibuffer ()
  ;;     "Paste the ath at point into the minibuffer.
  ;; This assumes that we are running `treemacs--copy-or-move',
  ;; so that pasting this path into the minibuffer allows us to copy/move
  ;; the previously-selected file into this path."
  ;;     (interactive)
  ;;     (let ((path (treemacs--prop-at-point :path)))
  ;;       (message "copied from treemacs")
  ;;       (paste-to-minibuffer (file-name-directory path))))
  )

;;;; PROGRAMMING support and utilities
;;;;; ido completion mode
(use-package ido
  :init
  (ido-mode 1)
  :config
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t))

;;;;; git
(use-package magit
  :config
  (defun vc-refresh-buffer (arg)
    (set-buffer arg)
    (vc-refresh-state))

  (defun magit-add-force (arg)
    "Adds (with force) the file ARG to the git repo.
If ARG not specified, defaults to the current buffer's file"
    (interactive (list (read-file-name "git add -f " (buffer-file-name))))
    (shell-command (concat "git add -f " arg)))

  (defun vc-refresh-all-git-buffers ()
    "get list of git files from magit,
for each open buffer with one of these files, refresh the version-control state"
    (mapcar #'vc-refresh-buffer
	    (seq-intersection
	     (mapcar #'file-name-nondirectory (magit-list-files))
	     (mapcar #'buffer-name (buffer-list))
	     #'string-equal-ignore-case)))

  ;; this might affect performance when there are many files
  ;; but it can always be turned off
  :hook (magit-refresh-buffer . vc-refresh-all-git-buffers)
  :bind ("C-x g" . magit-status))

;;;;; appearance
(defun my-prog-appearance ()
  (interactive)
  (display-line-numbers-mode t)
  (relative-line-numbers-setup)
  (electric-pair-local-mode t))

(defun adj/:around-goto-line-read-args (origfn)
  (let ((display-line-numbers 'absolute))
    (funcall origfn)))

(defun relative-line-numbers-setup ()
  (interactive)
  (display-line-numbers-mode t)
  (setq display-line-numbers 'relative)
  (defun adj/:around-goto-line-read-args (origfn)
    (let ((display-line-numbers 'absolute))
      (funcall origfn)))
  (advice-add 'goto-line-read-args :around #'adj/:around-goto-line-read-args))

(defun absolute-line-numbers-setup ()
  (interactive)
  (setq display-line-numbers 'absolute)
  (display-line-numbers-mode)
  (advice-remove 'goto-line-read-args #'adj/:around-goto-line-read-args))

(add-hook 'prog-mode-hook #'my-prog-appearance)
;;;;; outline
(use-package dash)
(use-package outshine
  :config
  ;; collapse the current level even when I'm not at the heading
  (defun my-outline-tab ()
    "if current line is a heading, call regular outshine-kbd-TAB;
else, first move to previous visible heading, then call it"
    (interactive)
    (move-beginning-of-line 1)
    (if (null (looking-at outline-regexp))
	(outline-previous-visible-heading 1))
    (outshine-kbd-TAB))

  :bind (:map outshine-mode-map
	      ("TAB" . my-outline-tab)
	      ("C-c C-p" . outline-previous-visible-heading)
	      ("C-c C-n" . outline-next-visible-heading)
	      ("M-<down>" . outline-move-subtree-down)
	      ("M-<up>" . outline-move-subtree-up)))

;; outshine
(add-hook 'outline-minor-mode-hook #'outshine-mode)
;; enable outline-minor-mode for elisp programming buffers
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

;;;;; expand region
(use-package expand-region
  :config
  (er/enable-mode-expansions 'inferior-python-mode
                             'er/add-python-mode-expansions)
  :bind
  ("C-=" . er/expand-region)
  ("C-M-/" . er/expand-region))

;;;;; aggressive indent
(use-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode))

;;;;; helper functions
;; implemented my own, extremely dumb version of dumb jump
;; possible enhancements:
;; when not found, try to search outside the current file
(defun find-symbol-first-occurrence ()
  "Search for symbol at point within the file."
  (interactive)
  (let ((my-symbol (thing-at-point 'symbol 'no-properties)))
    (if (null my-symbol)
	(message "No symbol at point")
      (progn (point-to-register 'r)
	     (goto-char (point-min))
	     (goto-char (search-forward-regexp
                         (isearch-symbol-regexp my-symbol)))
	     (isearch-forward-symbol-at-point)))))

;; only use find-symbol-first occurrence as a weak alternative in cases
;; where the backend for xref has not been set
(defun my-find-definition ()
  "If an xref-backend has been set, call `xref-find-definitions'.
Else, call find-symbol-first-occurrence"
  (interactive)
  (if (equal '(etags--xref-backend) xref-backend-functions)
      (find-symbol-first-occurrence)
    (execute-extended-command nil "xref-find-definitions")))

(global-set-key (kbd "M-.") #'my-find-definition)

;; set appropriate default compile-command
(defun set-compile-command (arg use-file &optional options)
  (setq-local compile-command
	      (concat arg " "
		      (if (and buffer-file-name use-file)
			  (shell-quote-argument
			   (file-name-sans-extension
			    (file-name-nondirectory buffer-file-name))))
		      options)))

(add-hook 'java-mode-hook
	  (lambda () (set-compile-command "javac" t)))
(add-hook 'scala-mode-hook
	  (lambda () (set-compile-command "scalac" t)))
(add-hook 'go-mode-hook
	  (lambda () (set-compile-command "go install" nil)))
(add-hook 'monicelli-mode-hook
	  (lambda () (set-compile-command "mcc" t " -o ")))

;;;;; shell
(setq shell-file-name "/bin/zsh")

(use-package sticky-shell
  :load-path "custom/packages/sticky-shell/")

(use-package shell-output-mode
  :load-path "custom/modes/"
  :defer 1)

;;;; PROGRAMMING-LANGUAGES
;;;;; java
(use-package javadoc-lookup
  :bind (("C-c s" . org-store-link)))

(use-package cc-mode
  :bind (:map java-mode-map
	      ("C-i" . javadoc-add-import))
  :hook (java-mode . subword-mode))

;;;;; scala
(use-package scala-mode
  :mode "\\.sc\\'")

;;;;; monicelli
(use-package monicelli-mode
  :load-path "custom/modes/"
  :mode "\\.mc\\'")

;;;;; clojure
(use-package clojure-mode)

(use-package cider)

;;;;; go
(use-package go-mode)

;;;;; python
(add-hook 'python-mode-hook #'subword-mode)
(add-hook 'inferior-python-mode-hook #'subword-mode)
(use-package elpy
  :init
  ;; (advice-add 'python-mode :before 'elpy-enable)
  (setq elpy-modules '(elpy-module-sane-defaults
		       elpy-module-company
		       elpy-module-eldoc
		       elpy-module-flymake
		       elpy-module-pyvenv
		       elpy-module-yasnippet
		       elpy-module-django))
  :bind (:map elpy-mode-map
	      ("M-." . elpy-goto-definition)))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))  ; or lsp-deferred
  )

;; (use-package lsp-ui)

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config
  (defun get-project-pyvenv (directory)
    (interactive)
    (or
     directory
     (list (bound-and-true-p project-pyvenv))))
  (advice-add 'pyvenv-activate :filter-args #'get-project-pyvenv)
  (setq pyvenv-default-virtual-env-name "venv/")
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter
                      (concat pyvenv-virtual-env "bin/python3")))
	      (lambda ()
		(message "%s %s"
                         (propertize "activated venv at:"
                                     'face 'minibuffer-prompt)
                         (getenv "VIRTUAL_ENV")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

;; helper functions to quickly open a shell with venv activated
(defun venv-shell (&optional arg)
  (interactive "P")
  (if (equal current-prefix-arg '(4))
      (setf arg 2))
  (let ((pyvenv-to-use
	 (if (boundp 'project-pyvenv)
	     project-pyvenv
	   (read-directory-name "Activate venv: "))))
    (dotimes (number (or arg 1))
      (let ((buffer-name (format "*shell-%d-venv*" number)))
        (named-venv-shell buffer-name pyvenv-to-use)))))

(defun named-venv-shell (&optional arg pyvenv-to-use)
  (interactive (list (read-string "Name of the shell: " "*shell-")))
  (let ((pyvenv-to-use
	 (or
	  pyvenv-to-use
	  (bound-and-true-p project-pyvenv)
	  (read-directory-name "Activate venv: "))))
    (switch-to-buffer (shell arg))
    (comint-send-string nil (concat "source " pyvenv-to-use "bin/activate"))
    (comint-send-input nil t)))

(defun python-run-this (arg)
  (interactive (list (read-file-name "run this file in a pyvenv shell: ")))
  (named-venv-shell (format "*shell-%s*"(file-name-nondirectory arg))
		    (bound-and-true-p project-pyvenv))
  (let ((desired-dir (file-name-directory arg)))
    (if (not (equal desired-dir default-directory))
	(progn (comint-send-string nil (format "cd %s" desired-dir))
	       (comint-send-input nil t))))
  (comint-send-string nil (format "python %s" arg))
  (comint-send-input nil t))

(use-package blacken
  :commands blacken-mode blacken-buffer)

;; for the inferior python interpreter
(defun inferion-python-current-indentation ()
  (beginning-of-line 1)
  (set-mark-command nil)
  (back-to-indentation)
  (buffer-substring (region-beginning) (region-end)))

(defun input-or-newline (&optional arg)
  (if arg
      (comint-send-input)
    (insert ?\n)))

(defun python-smart-indent (&optional arg)
  (interactive)
  (delete-horizontal-space)
  (if (string-match-p "[^[:space:]]" (or (thing-at-point 'line 'no-prop) ""))
      (let ((indent (inferion-python-current-indentation)))
	(move-end-of-line 1)
	(if (eq (char-before) ?:)
	    ;; I'd rather use tabs, but then this weird string gets inserted:
	    ;; 0__dummy_completion__  1__dummy_completion__
	    (setf indent (concat indent "    ")))
	(input-or-newline arg)
	(insert indent))
    (input-or-newline arg)))

(add-hook 'inferior-python-mode-hook
          (lambda ()
	    (local-set-key (kbd "C-<return>")
			   #'python-smart-indent)))

(add-hook 'inferior-python-mode-hook
          (lambda ()
	    (local-set-key (kbd "<return>")
			   (lambda () (interactive) (python-smart-indent 'send)))))

(add-hook 'inferior-python-mode-hook
          (lambda ()
	    (local-set-key (kbd "C-<backspace>")
			   (lambda () (interactive) (delete-backward-char 4)))))

(add-hook 'inferior-python-mode-hook (lambda () (setq-local use-cleanup-kills t)))

(add-hook 'inferior-python-mode-hook (lambda ()
				       (advice-add #'kill-new :filter-args
						   #'inferior-python-cleanup-kills)))

(defun inferior-python-cleanup-kills (string &optional _replace)
  (if (bound-and-true-p use-cleanup-kills)
      (let ((prompt-regexp (concat "^" "\\(" python-shell-prompt-regexp "\\)+"))
	    (prompt-block-regexp (concat "^" "\\(" python-shell-prompt-block-regexp "\\)+")))
	(thread-last
	  (replace-regexp-in-string prompt-block-regexp "    " (car string))
	  (replace-regexp-in-string prompt-regexp "")
	  (list))) ; string argument is actually passed as a list
    string))
;;;;; c / c++ / objective c
(use-package eglot
  :init
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'objc-mode-hook 'eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode objc-mode) "clangd")))

;;;;; emacs lisp
(defmacro make-it-quiet (&rest body)
  `(let ((inhibit-message t))
     (progn ,@body)))

(add-hook 'kill-emacs-hook (lambda () (setq inhibit-message t)) -99)

;;;;; templates
(auto-insert-mode t)
(setq auto-insert-directory "~/.emacs.d/templates")

(defun template-insert-file-name ()
  (let ((name (thread-last
	        (file-name-nondirectory buffer-file-name)
                (file-name-sans-extension)
                (replace-regexp-in-string "_\\|-" " "))))
    (replace-string "Template"
		    (concat (upcase (string (aref name 0))) (seq-drop name 1)))))

(defun add-auto-insert-template-by-extension (arg)
  `(define-auto-insert
     ,(concat "\\." arg "\\'")
     [,(concat arg ".template") template-insert-file-name]))

(defmacro define-all-templates (args)
  `(progn ,@(mapcar 'add-auto-insert-template-by-extension args)))

(define-all-templates
 ("org" "java" "sc" "c" "go"))

;;;; SPECIAL VIEWS (web and PDF)
;; (use-package my-webkit)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode))

;;;; GAMES
;;;;; tetris

(defun tetris-setup ()
  (yt-frame)
  (pixel-scroll-precision-mode -1)
  (define-key tetris-mode-map "q" #'tetris-quit)
  (gcmh-mode -1)
  (setq gc-cons-threshold 400000
        gc-cons-percentage 0.5))

(add-hook 'tetris-mode-hook #'tetris-setup)

(defun tetris-quit ()
  (interactive)
  (my-gc-setup)
  (gcmh-mode +1)
  (pixel-scroll-precision-mode +1)
  (kill-buffer "*Tetris*")
  (condition-case nil
      (kill-buffer "tetris-scores")
    (error (message "no buffer named tetris-scores")))
  (startup-look))

(defun close-scores-and-play ()
  (interactive)
  (delete-window)
  (switch-to-buffer "*Tetris*")
  (tetris-start-game))

(defvar-keymap tetris-score-mode-map
  "n" #'close-scores-and-play
  "q" (lambda ()
        (interactive)
        (View-quit)
        (tetris-quit)))

(define-minor-mode tetris-score-mode
  "Minor mode for displaying tetris scores
  \\{tetris-score-mode-map}"
  :keymap tetris-score-mode-map
  (add-hook 'view-mode-hook
	    (lambda ()
	      (if tetris-score-mode
		  (progn
                    (define-key view-mode-map "n" nil)
                    (define-key view-mode-map "q" nil))))))

(add-to-list 'auto-mode-alist '("\\tetris-scores\\'" . tetris-score-mode))

;;;; RANDOM STUFF
;; for testing xwidgets
(setq load-prefer-newer t)

;;; CUSTOM-added variables and faces
;; my custom-safe-themes are my-monokai, the-matrix, tango-dark,
;; cyberpunk, and my-misterioso
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("19759a26a033dcb680aa11ee08677e3146ba547f1e8a83514a1671e0d36d626c" "7d52e76f3c9b107e7a57be437862b9d01b91a5ff7fca2524355603e3a2da227f" "a000d0fedd5e1c3b58e3a1c645c316ec2faa66300fc014c9ad0af1a4c1de839b" "ebd933e1d834aa9525c6e64ad8f6021bbbaa25a48deacd0d3f480a7dd6216e3b" "99830ccf652abb947fd63a23210599483a14b1521291cd99aabae9c7ce047428" default))
 '(org-cycle-emulate-tab 'whitestart)
 '(package-selected-packages
   '(sticky-shell use-package ace-window racket-mode emacsql-sqlite-builtin org-roam rainbow-mode esup benchmark-init god-mode blacken lsp-pyright aggressive-indent expand-region cheatsheet exec-path-from-shell dired-subtree pdf-tools tablist vundo treemacs elpy avy csv-mode dashboard gcmh monicelli-mode all-the-icons-ibuffer all-the-icons-dired projectile all-the-icons flycheck cyberpunk-theme the-matrix-theme monokai-theme mood-line org-inlinetask magit outshine javadoc-lookup go-mode sr-speedbar scala-mode cider clojure-mode))
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;; .emacs ends here
