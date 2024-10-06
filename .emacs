;;; .emacs --- init file at HOME  -*- lexical-binding: t; -*-
;; Copyright (C) 2022  Andrew De Angelis

;; Author: Andrew De Angelis <bobodeangelis@gmail.com>
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
(require 'package)
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

(setq use-package-always-ensure t)

;;;; PATH from shell
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

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
  :load-path "custom/packages/all-the-haikus/")

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
  (if (not window-system)
      #'modus-vivendi
    (let ((hour (nth 2 (decode-time (current-time)))))
      (if (or (> hour 23) (< hour 8))
	  #'modus-vivendi ; at night
        #'my-monokai)))) ; during the day

;; my daily default theme is based on standard tango-dark;
;; with some small edits in ~/.emacs.d/tango-dark-theme.el
;; I also really like monokai:
;; made some edits in ~/.emacs.d/my-monokai-theme.el

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

;;;;; close
(add-hook 'kill-emacs-hook (lambda () (setq inhibit-message t)) -99)

;;;;; windows
(defun window-vertically-split-p (&optional window)
  (let ((height (window-height window)))
    (< height (- (frame-height) 1))))

;;;;; dashboard
(use-package dashboard
  :if window-system
  :autoload dashboard-insert-startupify-lists
  :init
  (require 'all-the-haikus)
  (require 'all-the-icons)
  (defun my-dashboard-init ()
    (setq dashboard-init-info
          (format "Emacs started in %s seconds." (emacs-init-time "%.2f"))))

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
;; resize current frame
(defalias #'fullscreen-frame #'toggle-frame-fullscreen)

(defun big-frame (&optional fullscreen)
  (interactive "P")
  (if fullscreen (toggle-frame-fullscreen)
    (toggle-frame-maximized)))

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
(defun yt-frame (&optional arg)
  (interactive "P")
  (when arg (make-frame))
  (treemacs-close-and-other-windows t)
  (let ((frame (selected-frame)))
    (when (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth))
      (toggle-frame-fullscreen))
    (when (eq (frame-parameter frame 'fullscreen) 'maximized)
      (toggle-frame-maximized)))
  (set-frame-size (selected-frame) 84 60)
  (right-frame))

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

(defun other-frame-or-make (&optional arg)
  "Switch to `other-frame'. With prefix ARG, `make-frame'"
  (interactive "P")
  (if arg
      (make-frame)
    (other-frame 1)))

(global-set-key (kbd "C-x <up>") #'delete-window-above)
(global-set-key (kbd "C-x <down>") #'delete-window-below)
(global-set-key (kbd "M-o") #'other-frame-or-make)

;;;;; themes and colors
(defun un-theme (&optional arg)
  "Disable all custom themes ad load theme ARG."
  (interactive "snew theme: ")
  (mapc #'disable-theme custom-enabled-themes)
  (if (> (length arg) 0) (load-theme (intern arg))))

(use-package cyberpunk-theme)

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
(defun modus-vivendi ()
  (interactive)
  (un-theme "modus-vivendi"))
(defun cloud-theme ()
  (interactive)
  (un-theme "cloud"))

;; this highlights characters beyond the 80 char limit
(use-package whitespace
  :init
  ;; shortcut
  (defalias #'wspace #'whitespace-mode)
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
  :load-path
  "/opt/homebrew/Cellar/emacs-plus@29/29.0.60/share/emacs/29.0.60/lisp/"
  :init
  (setq help-window-select t)
  :bind
  (:map help-mode-map
        ("b" . help-go-back)
        ("f" . help-go-forward)))

;; json files
(defun show-paren-full-matching-sexp ()
  "Toggle `show-paren-style' between 'parenthesis and 'expression"
  (interactive)
  (if (eq show-paren-style 'expression)
      (setq-local show-paren-style 'parenthesis)
    (setq-local show-paren-style 'expression)))

(add-to-list 'auto-mode-alist '("\\.jsonl\\'" . json-mode))

(defun json-pretty-print-if-scratch ()
  (when (and (equal "*scratch*" (buffer-name (current-buffer)))
	     (y-or-n-p "format this json doc?"))
    (json-pretty-print-buffer)))

(use-package json-mode
  :config
  :hook
  (json-mode . hs-minor-mode)
  (json-mode . json-pretty-print-if-scratch)
  :bind (:map json-mode-map
	      ("C-c C-s" . show-paren-full-matching-sexp)
	      ("C-c C-h" . hs-toggle-hiding)
	      ("C-c C-l" . hs-hide-level)
	      ("C-c C-b" . json-pretty-print-buffer)))

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
  (setq org-agenda-files '("~/org/TODO.org" "~/org/ToBuy.org" "~/org/chtu_todo.org"))

  (defun tick (&optional untick)
    (interactive "P")
    (if untick
        (insert "❌")
      (insert "✅")))

  (defun org-get-link-at-point ()
    "Copy the link at point, and message it in the minibuffer"
    (interactive)
    (let* ((props (text-properties-at (point)))
           (link (plist-get (plist-get props 'htmlize-link) :uri)))
      (kill-new link)
      (message "%s %s"
               (propertize "copied:" 'face 'minibuffer-prompt)
               link)))

  (defun my-org-tab ()
    "If current line is a heading, call regular org-cycle;
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
            ;; (let ((to-emacs "to Emacs"))
            ;;   ;; ensure that ns-input-file is available
            ;;   ;; not sure why, but somehow
            ;;   ;; this is achieved by putting it in a format string
            ;;   (message "ns-input-file: %s" ns-input-file)
            ;;   (message "Welcome %s %s" to-emacs (car ns-input-file)))
            ;; (message "ns-input-file: %s" ns-input-file)
            (message "Welcome to Emacs %s" (or (car ns-input-file) ""))
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

;;;;; file shortcuts
(defmacro define-file-shortcut (name file)
  `(defun ,name ()
     ,(concat "Interactive shortcut for " (symbol-name name))
     (interactive)
     (find-file ,file)))

(defun define-all-shortcuts (args)
  (eval `(progn ,@(mapcar (lambda (el)
                            `(define-file-shortcut ,(car el) ,(cadr el)))
                          args))))

(define-all-shortcuts
 (list
  '(init user-init-file)
  '(zshrc "~/.zshrc")
  '(forg "~/org")
  '(beorg "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
  '(todo "~/org/TODO.org")
  '(tobuy "~/org/ToBuy.org")
  '(my-modes "~/.emacs.d/custom/modes")
  '(crafting-interpreters "~/CraftingInterpreters")
  '(practice-notebook "~/org/practice_notebook.org")
  '(chtu-todo "~/org/chtu_todo.org")
  '(gym-notes "~/org/gym_exercise_notes.org")))

;;;;; open files in specialized ways
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

(setq eww-suggest-uris (append eww-suggest-uris '(region-at-point word-at-point)))

(setq eww-search-prefix "https://search.brave.com/search?q=")

(autoload ; so `open-in-browser' works, even if we haven't loaded eww yet
  #'eww-suggested-uris
  "/opt/homebrew/Cellar/emacs-plus@29/29.0.60/share/emacs/29.0.60/lisp/net/eww.el")

(defun open-in-browser (url)
  (interactive
   (let ((uris (eww-suggested-uris)))
     (list (read-string (format-prompt "Enter URL or keywords"
                                       (and uris (car uris)))
                        nil 'eww-prompt-history uris))))
  (let ((url (concat "\"" (eww--dwim-expand-url url) "\"")))
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
  (setq recentf-exclude
	(append recentf-exclude
		'("~/.emacs.d/ido.last" ".*treemacs-persist" "~/.emacs.d/bookmarks")))
  (make-it-quiet
   (recentf-mode))

  (defun my-search-for-read-only-buffers ()
    "Start an isearch with the latest inputted key"
    (interactive)
    (isearch-with-start (char-to-string last-input-event)))

  (defun bind-alpha-keys (keymap function)
    "Bind all alphabetical keys to FUNCTION in KEYMAP."
    (let ((alpha-chars (append
			(string-to-list "abcdefghijklmnopqrstuvwxyz")
			(string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
			(string-to-list ",./;:"))))
      (dolist (char alpha-chars)
	(define-key keymap (char-to-string char) function))))

  ;; all alphabetical characters are bound to the search function
  ;; this way, when in a recentf-dialog buffer:
  ;; all digits will select a specific file
  ;; all letters will open a search for the specific file
  (bind-alpha-keys recentf-dialog-mode-map 'my-search-for-read-only-buffers)

  :hook (recentf-dialog-mode . hl-line-mode))

;;;;; projectile mode
(use-package projectile
  :defer 6
  :config
  (setq projectile-ignored-projects '("~/") ;; TODO: fix this
        projectile-switch-project-action #'projectile-dired-other-window)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)))

;;;;; iCloud functionalities
(use-package icloud
  :load-path "custom/packages/icloud-utilities/"
  :custom
  (icloud-default-open-function (if ido-mode #'ido-find-file #'find-file))
  :defer 1)

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
  (setq arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1)))
  (point))

;; remap C-a to 'dwim-move-beginning-of-line'
(global-set-key (kbd "\C-a") #'dwim-move-beginning-of-line)

;; when typing in selected region, delete it
(delete-selection-mode t)

;; kill and copy
(defun my-kill-whole-line (&optional arg)
  "Delete the line and, when appropriate, preceding newline.
If ARG not provided or ARG > 1, first move to the beginning of the line,
so we will delete the whole line. Then kill ARG lines.
Then, delete all preceding whitespace."
  (interactive "P")
  (if (or (not arg) (> arg 1))
      (move-beginning-of-line 1))
  (kill-line arg)
  (when (eq (char-before) ?\C-j) ; newline
    (delete-char -1))
  (delete-horizontal-space 'backwards))

(defun dwim-kill-line (&optional arg)
  "If highlighted, `kill-region', else `my-kill-whole-line'"
  (interactive "P")
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (my-kill-whole-line arg)))

(global-set-key (kbd "C-k") #'dwim-kill-line)

(defun copy-current-line ()
  (let ((orig-point (point)))
    (kill-ring-save (dwim-move-beginning-of-line)
		    (line-end-position))
    (goto-char orig-point))
  (message "%s %s"
           (propertize "copied current line:" 'face 'minibuffer-prompt)
           (current-kill 0))
  (current-kill 0))

(defun dwim-copy ()
  (interactive)
  (if (not mark-active)
      (copy-current-line)
    (kill-ring-save (region-beginning) (region-end))))

(global-set-key (kbd "M-w") #'dwim-copy)

(defun dwim-kill ()
  (interactive)
  (if (not mark-active)
      (progn
        (kill-region (dwim-move-beginning-of-line)
		     (line-end-position))
        (message "%s %s"
                 (propertize "cut line:" 'face 'minibuffer-prompt)
                 (current-kill 0)))
    (kill-region (region-beginning) (region-end))))

(global-set-key (kbd "C-w") #'dwim-kill)

(defun query-delete-line (arg)
  (interactive "sbeginning of line to delete: ")
  (query-replace-regexp
   (concat "\n"
	   "\s*"
	   arg
	   ".*$")
   ""))

;;
(defun count-total-lines ()
  (interactive)
  (message "Buffer '%s' has %d total lines"
	   (buffer-name (current-buffer))
	   (count-lines (point-min) (point-max))))

(global-set-key (kbd "C-x l") #'goto-line)

;; upcase and downcase
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun on-region-or-char (command)
  (unless mark-active
    (progn
      (set-mark-command nil)
      (forward-char 1)))
  (funcall command (region-beginning) (region-end))
  (buffer-substring (region-beginning) (region-end)))

(defun my-smart-upcase ()
  "If region is selected, `upcase-region', else `upcase-char'.
Print a message to alert of the capitalization"
  (interactive)
  (if mark-active
      (upcase-region (region-beginning) (region-end))
    (upcase-char nil))
  (message "CAPITALIZED %s, %s" (thing-at-point 'word) (what-line)))

(defun my-smart-downcase ()
  "If region is selected, `downcase-region', else `downcase-char'.
Print a message to alert of the capitalization"
  (interactive)
  (if mark-active
      (downcase-region (region-beginning) (region-end))
    (downcase-region (point) (progn (forward-char) (point))))
  (message "downCASED %s, %s" (thing-at-point 'word) (what-line)))

(global-set-key (kbd "C-x C-u") #'my-smart-upcase)
(global-set-key (kbd "C-x C-l") #'my-smart-downcase)

;; move lines
(defalias 'move-line-up
  (kmacro "C-e C-SPC C-a C-w C-SPC <up> C-e <backspace> C-a C-y <return> <up>"))
(global-set-key (kbd "M-<up>") #'move-line-up)

(defalias 'move-line-down
  (kmacro "C-e C-SPC C-a C-w <down> C-a C-SPC <up> C-e <backspace> C-e <return> C-y"))

(global-set-key (kbd "M-<down>") #'move-line-down)

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
  :bind (("s-'" . avy-goto-char-2)
	 ("C-'" . avy-goto-char-2)
	 ("s-0" . avy-goto-word-0)))

;; better text-navigation, especially with god-mode
(global-set-key (kbd "C-b") #'left-word)
(global-set-key (kbd "C-f") #'right-word)


;; save clipboard contents to kill-ring
(setq save-interprogram-paste-before-kill 1000)

;;;;; undo
;; make sure you use the better keyboard command
(global-set-key (kbd "C-x u")
                (lambda ()
                  (interactive)
                  (warn "to UNDO, use C-/ or other, not C-x u")))

;;;;;; visual undo
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

(defun isearch-with-start (str)
  "Start an isearch with STR"
  (isearch-resume str nil nil t str nil))

(setq isearch-lazy-count t) ;; shows the number of total matches in the minibuffer

;; from beginning of document
(defun isearch-from-top (&optional regexp-p)
  (interactive "P")
  (push-mark) ; save the place where search started
  (goto-char (point-min))
  (isearch-forward regexp-p)
  ;; isearch sets the mark to the beginning of the search
  ;; in our case that's useless (just the beginning of the buffer)
  ;; so we pop that
  (pop-mark))

(global-set-key (kbd "M-s") #'isearch-from-top)

;; from end of document
(defun isearch-from-bottom (&optional regexp-p)
  (interactive "P")
  (push-mark) ; save the place where search started
  (goto-char (point-max))
  (isearch-backward regexp-p)
  ;; isearch sets the mark to the beginning of the search
  ;; in our case that's useless (just the end of the buffer)
  ;; so we pop that
  (pop-mark))

(global-set-key (kbd "M-r") #'isearch-from-bottom)

;; use C-u C-SPC to get back to where search started

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
  (while (search-forward-regexp (concat "^[[:space:]]*" arg) nil 'noerror)
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
  ;; :bind
  ;; I'm out of practice with god-mode, better to avoid triggering it for now
  ;; ("<escape>" . god-mode-all)
  ;; ("s-<escape>" . god-local-mode)
  ;; ("M-<escape>" . god-execute-with-current-bindings)
  ;; ("C-z" . god-mode-all)
  ;; ("C-s-z" . god-local-mode)
  ;; ("C-M-z" . god-execute-with-current-bindings)
  )


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
  ;; :load-path "/Users/andyjda/treemacs-paste-feature/treemacs/src/elisp"
  :if window-system
  :demand t
  :init
  (defalias #'tm #'treemacs)

  (defun treemacs-close-window ()
    (if (and
         (fboundp #'treemacs-current-visibility)
         (equal 'visible (treemacs-current-visibility)))
        (delete-window (treemacs-get-local-window))))

  (defun treemacs-close-and-other-windows (&optional arg)
    (interactive "P")
    (if arg (treemacs-close-window))
    (delete-other-windows))


  ;; for the paste to work, make sure you have this function in
  ;; treemacs-mouse-interface
  ;; (defun treemacs--paste-point-to-minibuffer ()
  ;;     "Paste the path at point into the minibuffer.
  ;; This is used by the \"Paste here\" button,
  ;; which assumes that we are running `treemacs--copy-or-move',
  ;; so that pasting this path into the minibuffer allows us to copy/move
  ;; the previously-selected file into the path at point."
  ;;     (interactive)
  ;;     (let* ((path-at-point (treemacs--prop-at-point :path))
  ;;            (path
  ;;             (if (file-directory-p path-at-point)
  ;;                 path-at-point
  ;;               (file-name-directory path-at-point))))
  ;;       (switch-to-minibuffer)
  ;;       (mark-whole-buffer)
  ;;       (delete-region (region-beginning) (region-end))
  ;;       (insert path))
  ;;     (message "copied from treemacs"))

  ;; and this in `treemacs-rightclick-menu'

  ;; `(["Paste here"
  ;;                treemacs--paste-point-to-minibuffer
  ;;                :visible ,(string-match-p "\\(\\(Move\\)\\|\\(Copy\\)\\) to: " (or (minibuffer-prompt) ""))]

  :config
  ;; this is necessary for treemacs-paste to work properly
  ;; (add-to-list 'ido-read-file-name-non-ido #'treemacs-rightclick-menu)

  :bind
  (("C-x 1" . treemacs-close-and-other-windows)))

;;;; PROGRAMMING support and utilities
;;;;; ido completion mode
(use-package ido
  :init
  (ido-mode 1)
  :config
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t))

;;;;; prog-mode
(use-package prog-setup
  :hook
  (prog-mode . prog-setup-appearance)
  :load-path "custom/packages/prog-setup/")


;;;;; git
(use-package magit
  :config

  ;; instead of `magit-insert-unpushed-to-upstream-or-recent',
  ;; show both `magit-insert-unpushed-to-upstream' and
  ;; `magit-insert-recent-commits' in `magit-status-sections-hook'
  (setq magit-status-sections-hook
        (replace-in-list #'magit-insert-unpushed-to-upstream-or-recent
                         '(magit-insert-unpushed-to-upstream
                           magit-insert-recent-commits)
                         magit-status-sections-hook))

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
  :hook (magit-refresh-buffer . vc-refresh-all-git-buffers))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

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
  (emacs-lisp-mode . aggressive-indent-mode)
  (inferior-emacs-lisp-mode . aggressive-indent-mode))

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
      (progn (beginning-of-buffer)
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
;; TODO: there are already some built-in compile utils, you should
;; leverage those
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

;; TODO: expand the compile and run command into a whole package onto itself
;; that can take up any language
;; (defun run-this ()
;;   (interactive)
;;   (let ((func (pcase major-mode
;;                 ('python-mode #'python-run-this)
;;                 ('web-mode #'js-run-this)
;;                 (_ (error "run-this does not currently support this mode: %s" major-mode)))))
;;     (call-interactively func)))

;;;;; shell
(setq shell-file-name "/bin/zsh")

(use-package sticky-shell
  :hook (sticky-shell-mode . sticky-shell-shorten-header-set-mode)
  :load-path "custom/packages/sticky-shell/"
  ;; put the load path here whenever you need to test things
  :config
  (defalias #'sticky-mode #'sticky-shell-mode))

(use-package shell-output-mode
  :load-path "custom/modes/"
  :config
  (setq find-default-options " ")
  :defer 1)


(defun recenter-middle (string)
  (when (eq (window-buffer (selected-window)) (current-buffer))
    (recenter nil t))
  string)

(define-minor-mode center-shell-mode
  "Minor mode to show the shell output at the center of the buffer."
  :group 'comint
  :global t
  :lighter nil
  (if center-shell-mode
      (add-hook 'comint-output-filter-functions #'recenter-middle 99)
    (remove-hook 'comint-output-filter-functions #'recenter-middle)))

(use-package coterm
  :init
  (coterm-mode)
  :defer 1)
;; comint (shell) mode
(add-hook 'comint-mode-hook
	  (lambda ()
	    (visual-line-mode -1)
	    (electric-pair-local-mode t)
            ;; TODO center-shell doesn't appear to work, unclear why
	    (center-shell-mode t)))

;;;;; highlight TODO words
(use-package hl-todo
  :hook
  (prog-mode . global-hl-todo-mode))

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

(use-package python
  :config

  (defun py-generic-format (arg action &optional line-number block-name)
    (let ((arg (if arg
                   (string-replace "\"" "'" arg))))
      (format (concat
               action
               "(f\""
               (when line-number (format "From: %s; " block-name))
               (when line-number (format "line number: %s; " line-number))
               (when arg "%s : {%s}")
               "\")")
              arg arg)))

  (defun py-format (arg &optional line-number block-name)
    (py-generic-format arg "print" line-number block-name))

  (defun py-log-format (arg &optional line-number block-name)
    (py-generic-format arg "logger.info" line-number block-name))

  (defun py-debug-log (&optional arg)
    (interactive "P")
    (prog-debug-print arg #'py-log-format))

  (defun py-debug-print (&optional arg)
    (interactive "P")
    (prog-debug-print arg #'py-format))

  (defun py-run-this (file)
    (interactive (list (read-file-name "run this file in a shell: ")))
    (prog-run-this file nil
                   (concat "python "
                           (file-name-nondirectory file) " ")
                   #'python-activate-venv))

  (defun py-query-delete-print ()
    (interactive)
    (query-replace-regexp "\n\s*#?\s*print(.*)" ""))

  ;; NOTE the below is not really necessary
  ;; I'm using it because when I call py-query-delete-print (defined above)
  ;; I always get this other function as another possible option:
  ;; gnus-summary-simplify-subject-query
  ;; I never need these libraries, so I'm ok with unbinding all of their symbols.
  ;; (wrapping it within a condition-case so it doesn't throw an error when it
  ;; tries to unload something that's already been unloaded)
  (condition-case
      nil
      (progn (unload-feature 'gnus-group 'force)
             (unload-feature 'gnus-sum 'force))
    (error nil))

  (setq-local delete-print #'py-query-delete-print)

  (defun query-comment-out-print ()
    (interactive)
    (query-comment-out "print"))

  :hook
  (python-mode . subword-mode)
  :bind (:map python-mode-map
              ("M-<right>" . python-indent-shift-right)
              ("M-<left>" . python-indent-shift-left)
              ("C-c r" . py-run-this)
              ("C-M-l" . py-debug-log)
              ("C-M-p" . py-debug-print)))


(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))  ; or lsp-deferred
  :config
  (setq lsp-enable-file-watchers nil))

(defun get-project-venv (&optional directory)
  ;; possible improvement: when venv not found,
  ;; add option to query user for venv directory
  (let* ((root (or
                (when (boundp 'projectile-project-root)
                  (projectile-project-root directory))
                directory
                default-directory))
	 (venv-dir (concat root ".venv")))
    (if (file-directory-p venv-dir)
	(file-name-as-directory venv-dir)
      (progn (message "no '.venv' or 'venv' directory found in this project: %s" root) nil))))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config

  (setq pyvenv-default-virtual-env-name ".venv/")
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))
	      (lambda ()
		(message "activated venv at: \t%s" (getenv "VIRTUAL_ENV")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

;; when starting interpreter, automatically start the project's virtual environment
(advice-add 'run-python :before
	    (lambda (&optional _cmd _dedicated _show)
	      (message "_cmd from the advice: %s" _cmd)
	      (let ((venv (get-project-venv)))
		;; (read-directory-name "Activate venv: ")
		(when venv (pyvenv-activate (get-project-venv))))))

;; when starting interpreter, make sure to include the default environment variables
;; TODO: this needs some work
;; (advice-remove 'python-shell-calculate-command :filter-return
;; 	       (lambda (cmd)
;; 		 (message "cmd from the advice: %s" cmd)
;; 		 (message "concated: %s" (concat "source /Users/andrewjda/.zshrc && " cmd))
;; 		 (concat "source /Users/andrewjda/.zshrc && " cmd)))

(defun python-activate-venv ()
  (interactive)
  (let ((venv (get-project-venv)))
    (when venv
      (comint-send-string nil
			  (message "source %sbin/activate" venv))
      (comint-send-input nil t))))

(defun named-shell (name &optional setup-funcs)
  "Create (or switch to) a shell with *shell-NAME*  and
  `pop-to-buffer'.
SETUP-FUNCS is a list of functions to run when setting up the shell."
  ;; move or create the buffer: if the buffer is new, it'll be in
  ;; fundamental mode so we also have to start the shell
  (pop-to-buffer name)
  (when (eq major-mode 'fundamental-mode)
    (shell name)
    (dolist (func setup-funcs) (funcall func)))
  (comint-send-input nil t))

(defun named-shell-file (file &optional setup-funcs)
  "Start (or switch to) a shell for FILE.
Start the shell with `named-shell' and cd into FILE's directory.
SETUP-FUNCS is a list of functions to run when setting up the shell."
  (let ((name (format "*shell-%s*" (file-name-nondirectory file)))
        (desired-dir (file-name-directory file)))
    (named-shell name setup-funcs)
    ;; move to the desired dir if needed
    (if (not (equal desired-dir default-directory))
	(progn (comint-send-string nil (message "cd %s" desired-dir))
	       (comint-send-input nil t)))))

(defun python-run-app ()
  (interactive)
  (message "project app: %s" (bound-and-true-p project-app))
  (py-run-this (bound-and-true-p project-app)))

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

;;;;; javascript
(use-package tide)

;;;
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; if you use typescript-mode
(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
;;;

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'web-mode-hook #'subword-mode)

(defun js-format (arg &optional line-number _)
  (format (concat
           "console.log(`"
           (when line-number (format "At line number: %s; " line-number))
           (when arg "%s : ${%s}`")
           ");")
           arg arg))

(defun js-format-stringify (arg &optional line-number _)
  (format (concat
           "console.log(`"
           (when line-number (format "At line number: %s; " line-number))
           (when arg "%s STRINGIFIED : ${JSON.stringify(%s)}`")
           ");")
          arg arg))

(defun js-debug-log (&optional verbose)
  (interactive "P")
  (prog-debug-print verbose #'js-format))

(defun js-debug-log-stringify (&optional verbose)
  (interactive "P")
  (prog-debug-print verbose #'js-format-stringify))

(defun js-query-delete-console ()
  (interactive)
  (query-replace-regexp "\n\s*\s*console.log(.*);?" ""))

(use-package web-mode
  :config
  (setq web-mode-enable-current-element-highlight t)
  (set (make-local-variable 'delete-print) #'js-query-delete-console)
  :bind (:map web-mode-map
              ;; TODO verify that these work
	      ("M-p" . js-debug-log-stringify)
	      ("C-M-p" . js-debug-log)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(defun js-run-this ()
  (interactive)
  (python-run-app)
  (comint-send-input)
  (split-window-below)
  (named-shell "*shell-npm-dev*" nil)
  (let ((desired-dir (projectile-project-root)))
    (if (not (equal desired-dir default-directory))
	(progn (comint-send-string nil (message "cd %s" desired-dir))
	       (comint-send-input nil t))))
  (insert "npm run dev"))

(use-package prettier
  :init
  ;; (add-hook 'typescript-mode-hook 'prettier-mode)
  (add-hook 'web-mode-hook 'prettier-mode))

(use-package lsp-mode
  :ensure t
  :config
  ;; (require 'lsp-clients)
  (add-hook 'web-mode-hook 'lsp))

;;;;; rust
(use-package rustic
  :config
  ;; TODO: use the dbg! macro here instead
  (defun rs-format (arg &rest line-number-etc)
    (format "println!(\"%s : {:?}\", %s);"arg arg))

  (defun rs-debug-print (&optional arg)
    (interactive "P")
    (prog-debug-print arg #'rs-format))

  ;; standardize this so `prog-run-this' can use it
  (setq rustic-compilation-buffer-name "*compilation*")

  (defun rs-run-this (file)
    (interactive (list (read-file-name "run this file in a shell: ")))
    (prog-run-this file #'rustic-compile "cargo run"))

  :bind (:map rustic-mode-map
              ("C-M-p" . rs-debug-print)
              ("C-c r" . rs-run-this)
              ("C-c c" . rustic-compile)))

;;;;; c / c++ / objective c lang
(use-package eglot
  :init
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'objc-mode-hook 'eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode objc-mode) "clangd")))

(defun use-c-style-comments ()
  (setq comment-start "/*")
  (setq comment-end "*/"))

(mapc
(lambda (mode-hook)
  (add-hook mode-hook #'use-c-style-comments))
(list 'c-mode-hook 'c++-mode-hook 'objc-mode-hook))

;;;;; emacs lisp

(defun el-format (arg &optional line-number block-name)
  (format (concat
           "(message \""
           (when line-number (format "At line number: %s; " line-number))
           "%s: %%s\" %s)")
          arg arg))

(defun el-run-this ()
  (interactive)
  (when (called-interactively-p)
    (call-interactively #'elisp-eval-region-or-buffer)
    (funcall #'elisp-eval-region-or-buffer)))

(defun el-debug-print (&optional arg)
  (interactive "P")
  (prog-debug-print arg #'el-format))

(define-key emacs-lisp-mode-map (kbd "C-M-p") 'el-debug-print)
(define-key emacs-lisp-mode-map (kbd "C-c r") 'el-run-this)

;; TODO: due to some changes in how `inhibit-message' works,
;; this macro no longer works
(defmacro make-it-quiet (&rest body)
  `(let ((inhibit-message t))
     (progn ,@body)
     ;; if message aren't actually inhibit, use this to clear the echo area:
     (message nil)))

(defun region-at-point ()
  (thing-at-point 'region 'no-properties))

(defun replace-in-list (to-replace replacement list)
  "Replace TO-REPLACE with REPLACEMENT in LIST.
If REPLACEMENT is a list, each element is inserted.
If TO-REPLACE is not found in LIST, return LIST unaltered"
  (let ((pos (seq-position list to-replace)))
    (if pos
        (append (take pos list)
                (if (sequencep replacement) replacement (list replacement))
                (cdr (nthcdr pos list)))
      list)))

(add-hook 'kill-emacs-hook (lambda () (setq inhibit-message t)) -99)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
	    (local-set-key (kbd "M-<up>")
			   #'move-line-up)
	    (local-set-key (kbd "M-<down>")
			   #'move-line-down))
	  99)

(defun current-line-empty-p ()
  "Check if the current line contains nothing but whitespace"
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))


(setq source-directory "~/Emacs-Build/emacs")

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

;;;; SPECIAL VIEWS (web, PDF, ebooks)
(use-package webkit-mac-enhance
  :defer 1
  :load-path "custom/packages/webkit-mac-enhance/")

;;;;; pdf tools & epub
(use-package pdf-tools
  :init
  ;; install pdf-tools at the first call to doc-view-mode
  (advice-add 'doc-view-mode :after #'pdf-tools-install)
  :bind
  (:map pdf-view-mode-map
	("M-w" . pdf-view-kill-ring-save)
	("C-w" . pdf-view-kill-ring-save)
	("s-c" . pdf-view-kill-ring-save)))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;;; GAMES
;;;;; tetris

(defun tetris-setup ()
  (yt-frame)
  (pixel-scroll-precision-mode -1)
  (define-key tetris-mode-map "q" #'tetris-quit)
  ;; TODO: any time you do fancy stuff with memory, maybe take a look
  ;; at memory-report and see if you can check from there whether or
  ;; not it is safe to do it
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
  (hl-line-mode t)
  (visual-line-mode -1)
  (setq-local truncate-lines t)
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
;; my custom-safe-themes are my-monokai, tango-dark,
;; cyberpunk, and my-misterioso
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes t)
 '(org-cycle-emulate-tab 'whitestart)
 '(package-selected-packages
   '(prettier web-mode tide ll-debug json-mode magit-todos timu-caribbean-theme vterm eat sticky-shell symbol-overlay hacker-typer flycheck-package package-lint cloud-theme rustic rust-mode nov tree-sitter-langs tree-sitter god-mode toc-org use-package ace-window racket-mode emacsql-sqlite-builtin org-roam rainbow-mode benchmark-init blacken lsp-pyright aggressive-indent expand-region cheatsheet exec-path-from-shell dired-subtree pdf-tools tablist vundo elpy avy csv-mode dashboard gcmh monicelli-mode all-the-icons-ibuffer all-the-icons-dired projectile all-the-icons flycheck cyberpunk-theme monokai-theme mood-line org-inlinetask magit outshine javadoc-lookup go-mode sr-speedbar scala-mode cider clojure-mode))
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;; .emacs ends here
