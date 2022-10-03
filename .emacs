;;; .emacs --- init file at HOME  -*- lexical-binding: t; -*-
;; Author: Andrew De Angelis

;;; Code:

;;;; NATIVE COMPILATION
;;;;; don't show compilation warnings
;; keeping warnings on for now to monitor native comp til I'm familiar with it
;; (setq native-comp-async-report-warnings-errors nil)

;;;; PACKAGES
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; useful for when I'm working on my own packages and need to update
(defun reload-package-from-file (&optional arg)
  (interactive "spackage name: ")
  (delete-package-quietly (intern arg))
  (call-interactively 'package-install-file))

(defun delete-package-quietly (arg)
  (condition-case nil
      (package-delete (cadr (assq arg package-alist)))
    (error (message "error while deleting, most likely had already deleted"))))

;;;; PERFORMANCE
;;;;; garbage collection
;; avoid garbage collection at startup
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(defun gc-restore-defaults ()
  (setq gc-cons-threshold 800000
	gc-cons-percentage 1.0)
  (gcmh-mode -1))

;; use gcmh to reset garbage collection
(use-package gcmh
  :hook
  (after-init . gcmh-mode)
  (tetris-mode . gc-restore-defaults)
  :defer t
  :config
  (setq gcmh-idle-delay 'auto  ; default is 15s
	gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
	gcmh-verbose nil
	gcmh-auto-idle-delay-factor 10))


;;;;; monitoring init
;; check which packages are slow to load/config
;; (setq use-package-verbose t
;;       use-package-minimum-reported-time 0.005)

;; check where init is slow to load
;; (use-package benchmark-init
;;   :init
;;   ;; To disable collection of benchmark data after init is done.
;;   ;; third arg is DEPTH: 100 means FUNCTION is added at the end of the hook list
;;   (add-hook 'after-init-hook #'benchmark-init/deactivate 100))

;;;; CHEATSHEET
(use-package cheatsheet
  :hook (kill-emacs . cheatsheet-save-list-to-file))

;; quick ways to reload the updates:
(defun delete-cheatsheet ()
  (interactive)
  (delete-package-quietly 'cheatsheet))

(defun reload-cheatsheet ()
  (interactive)
  (delete-cheatsheet)
  (package-install-file "~/.emacs.d/custom/packages/cheatsheet/cheatsheet.el"))

;;;; appearance: SIZING, FRAMES, WINDOWS, THEMES
;;;;; startup
;; don't show startup screen
(setq inhibit-startup-screen t)

;; don't show tool bar
(tool-bar-mode -1)

;; scratch buffer in fundamental mode
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "**Welcome to Emacs!**\n\n\n")

;; choose default theme based on time of day
(defun default-theme ()
  (let ((hour (nth 2 (decode-time (current-time)))))
    (if (or (> hour 21) (< hour 8))
	'cyberpunk ; at night
      'my-monokai))) ; during the day

;; my daily default theme is based on standard tango-dark;
;; with some small edits in ~/.emacs.d/tango-dark-theme.el
;; I also really like monokai:
;; made some edits in ~/.emacs.d/my-monokai-theme.el

;; resize current frame (toggle)
(defun big-frame ()
  (interactive)
  (if (< (frame-parameter (selected-frame) 'width) 200)
      (set-frame-size (selected-frame) 204 55) ; used to be 203 55
    (set-frame-size (selected-frame) 100 45))
  (set-frame-position (selected-frame) 0 0))

(use-package mood-line
  :defer t)

(defun startup-look ()
  (interactive)
  (setq column-number-mode t)
  (load-theme (default-theme))
  (big-frame)
  (mood-line-mode t)
  (scroll-bar-mode -1)
  (global-visual-line-mode t)
  (set-fringe-style '(2 . nil))
  (pixel-scroll-precision-mode t))

(add-hook 'after-init-hook #'startup-look)

;;;;; dashboard
(use-package all-the-haikus
  :defer t)

(use-package bookmark
  :defer t)

(use-package dashboard
  :init
  (defun my-dashboard-init ()
    (setq dashboard-init-info
          (format "Emacs started in %s seconds." (emacs-init-time "%.2f"))))

  (autoload ; avoid loading the whole dashboad package unless needed
    #'dashboard-insert-startupify-lists
    "~/.emacs.d/elpa/dashboard-20220922.509/dashboard.el")

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
	    (dashboard-insert-startupify-lists)
	    (message
	     "Welcome! Dashboard opened in %.2f seconds"
	     (float-time (time-since time)))))
      (switch-to-buffer dashboard-buffer-name)))

  ;; (add-hook 'after-init-hook #'dashboard-open)

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

  (defun dashboard-insert-footer ()
    "Insert custom haiku-footer for dashboard."
    (let ((footer (and dashboard-set-footer (car dashboard-footer-messages)))
	  (footer-heading "Today's haiku:\n\n"))
      (when footer
	(insert "\n")
	;; (dashboard-center-line footer)
	(insert dashboard-footer-icon)
	(insert " ")
	(insert (propertize footer-heading 'face 'dashboard-heading))
	(insert (propertize footer 'face 'dashboard-footer))
	(insert "\n"))))

  (defun find-or-write-haiku ()
    (if (nth (random 2) (list nil t))
	(find-me-a-haiku 'random)
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
       (if arg () (setq arg 25))
       (enlarge-window-horizontally arg)
       (message (concat "expanded window by " (number-to-string arg))))

(defun wsmall (&optional arg)
  (interactive "P")
       (if arg () (setq arg 25))
       (shrink-window-horizontally arg)
       (message (concat "reduced window by " (number-to-string arg))))

;; frame to have together with max youtube
(defun yt-frame ()
  (interactive)
  (delete-other-windows)
  (set-frame-size (selected-frame) 84 54)
  (set-frame-position (selected-frame) 838 24))
(add-hook 'tetris-mode-hook #'yt-frame)

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
  (if arg (load-theme (intern arg))))

(defun my-misterioso ()
  (interactive)
  (un-theme "my-misterioso"))
(defun my-monokai ()
  (interactive)
  (un-theme "my-monokai"))
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
  :defer t
  :config
  (setq whitespace-style '(face lines-tail trailing)))

;; long line to test whitespace-mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; tabs
(use-package centaur-tabs
  :config
  (centaur-tabs-headline-match)

  (defun centaur-tabs-disable-locally ()
    (if centaur-tabs-mode (centaur-tabs-local-mode)))

  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'left)
  :hook
  (speedbar-mode . centaur-tabs-disable-locally)
  (shell-mode . centaur-tabs-disable-locally))

;;;;; icons
(use-package all-the-icons
  :defer t)

;;;;; appearance for specific modes

(add-hook 'package-menu-mode-hook
	  (lambda () (hl-line-mode t) (visual-line-mode -1)))

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

  (add-hook 'org-mode-hook #'turn-on-flyspell)
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

;;;; FILE utilities
;;;;; load faster
;; these are useful especially in VERY large files
(defun jit-lock-optimize-settings ()
  (interactive)
  (setq jit-lock-defer-time 0.05))

(defun jit-lock-default-settings ()
  (interactive)
  (setq jit-lock-defer-time nil))

;;;;; shortcuts
;; open init file
(defun init ()
  (interactive)
  (find-file "~/.emacs"))

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

;;;;; dired mode
(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  :bind
  (:map dired-mode-map
	("<mouse-2>" . dired-mouse-find-file))
  :config
  (setq all-the-icons-dired-monochrome nil))

;;;;; find and grep
(use-package shell-output-mode
  :defer t)

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
  (recentf-mode)
  :hook (recentf-dialog-mode . hl-line-mode)
  ;; :hook (after-init . recentf-mode)
  )

;;;;; projectile mode
(use-package projectile
  :defer 6
  :config
  ;; (setq projectile-ignored-projects '("~/"))
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)))

;;;; TEXT EDITING
;;;;; utilities
(defun smarter-move-beginning-of-line (arg)
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
      (move-beginning-of-line 1))))

;; remap C-a to 'smarter-move-beginning-of-line'
(global-set-key (kbd "\C-a") #'smarter-move-beginning-of-line)

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

(global-set-key (kbd "C-k") #'my-kill-whole-line)

(defun count-total-visible-lines ()
  (interactive)
  (message "Buffer '%s' has %d total lines"
	   (buffer-name (current-buffer))
	   (count-lines (point-min) (point-max))))

(global-set-key (kbd "C-x l") #'count-total-visible-lines)

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
  :defer t
  :bind (("C-'" . avy-goto-char-2)))
;;;;; flyspell
(use-package flyspell
  :bind (:map flyspell-mode-map
	      ("<mouse-3>" . flyspell-correct-word-before-point)
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

;;;; BUFFER AND FRAME movements
(use-package ibuffer  :bind (("C-x C-b" . ibuffer)
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

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "M-m") 'switch-to-minibuffer-window)

;; use S-right and S-left to navigate buffers
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; turn off M-i = TAB in minibuffer
(define-key minibuffer-local-map (kbd "M-i") "i")

;; switch frames
(global-set-key (kbd "M-o") 'other-frame)

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
  :ensure t
  :defer t)

;;;; PROGRAMMING support and utilities
;;;;; ido completion mode
(use-package ido
  :init
  (ido-mode 1)
  :config
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t))

;;;;; git
(defun vc-refresh-buffer (arg)
  (set-buffer arg)
  (vc-refresh-state))

(use-package magit
  :config
  (defun vc-refresh-all-git-buffers ()
    "get list of git files from magit,
for each open buffer with one of these files, refresh the version-control state"
    (mapcar #'vc-refresh-buffer
	    (seq-intersection
	     (mapcar #'file-name-nondirectory (magit-list-files))
	     (mapcar #'buffer-name (buffer-list))
	     #'string-match)))
  ;; this might affect performance when there are many files
  ;; but it can always be turned off
  :hook (magit-refresh-buffer . vc-refresh-all-git-buffers))

;;;;; appearance
(defun my-prog-appearance ()
  (interactive)
  (display-line-numbers-mode t)
  ;; (color-identifiers-mode t)
  (electric-pair-local-mode t)
  ;; (set-window-fringes (selected-window) 0)
  )

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
;; enable outline-minor-mode for *ALL* programming buffers
(add-hook 'prog-mode-hook #'outline-minor-mode)

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
	     (goto-char (search-forward-regexp (isearch-symbol-regexp my-symbol)))
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
			   (template-trim-name buffer-file-name)))
		      options)))
;; todo: move these to the appropriate section below
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
;;;;; programming-language specifics
;; java
(use-package javadoc-lookup
  :bind (("C-c s" . org-store-link)))

(use-package cc-mode
  :bind (:map java-mode-map
	      ("C-i" . javadoc-add-import))
  :hook (java-mode . subword-mode))

;; scala
(use-package scala-mode
  :mode "\\.sc\\'")

;; monicelli
(use-package monicelli-mode
  :mode "\\.mc\\'")

;; clojure
(use-package clojure-mode
  :defer t)

(use-package cider
  :defer t)

;; go
(use-package go-mode
  :defer t)

;; python
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setq elpy-modules '(elpy-module-sane-defaults
                          elpy-module-company
                          elpy-module-eldoc
                          elpy-module-flymake
                          elpy-module-pyvenv
                          elpy-module-yasnippet
                          elpy-module-django)))
(add-hook 'python-mode-hook #'color-identifiers-mode)

;;;;; templates
(defun template-trim-name (file-name &optional file-ext)
  "Find and replace file-path from FILE-NAME.
If provided find and replace FILE-EXT also"
  (replace-regexp-in-string ".*\/" ""
			    (if file-ext
				(string-replace file-ext "" file-name)
			      file-name)))

(defun template-write-to-buffer (contents-as-string)
  "Write CONTENTS-AS-STRING to the current buffer.
\(if the buffer is not empty, CONTENTS-AS-STRING will be appended to the end)"
  (with-current-buffer (buffer-name)
    (goto-char (point-max))
    (insert contents-as-string)))

(defun template-file-to-string (file-name)
  "Return contents of FILE-NAME as string."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))

(defun template-set-contents (file-name file-ext)
  "Read from the template file and write to buffer the contents.
replacing 'Template' with FILE-NAME"
  (let ((file-contents
	 (template-file-to-string
	  (concat "~/.emacs.d/custom/programming-language-templates/Template"
		  file-ext))))
    (template-write-to-buffer
     (string-replace "Template"
		     (template-trim-name file-name file-ext)
		     file-contents))))

(defun get-file-ext (&optional file-name)
  (if (equal nil file-name) (setq file-name buffer-name))
  ;; (substring file-name (string-match "\.[^.]*$" file-name))
  (concat "." (file-name-extension file-name)))

(defun template-open ()
  "Prompt for file name, find the file, ask for confirmation,
and set its contents as the appropriate programming-language-template"
  (interactive)
  (setq file-name (read-file-name "Enter the name of your file:"))
  (setq file-ext  (get-file-ext file-name))
  (find-file file-name)
  (message "Opened %s" file-name)
  (if (equal "y" (read-string "Write your template? y/n: "))
      (template-set-contents file-name file-ext)))

;;;; RANDOM STUFF
;; implementing a simple web search for quick questions
(setq my-search-engine "searx.bar")
(defun websearch (&optional arg)
  (interactive "P")
  (let ((url
	 (thread-last
	   (read-from-minibuffer
	    (format "use %s to search: " my-search-engine))
	   (string-replace " " "+")
	   (concat "https://" my-search-engine "/search?q=")))
	(new-session (if arg t nil)))
    (message "xwidgeting this: %s" url)
    (xwidget-webkit-browse-url url new-session)))

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
   '("024e125a165ef1f13cf858942b9e8f812f93f6078d8d84694a5c6f9675e94462" "e5dc4ab5d76a4a1571a1c3b6246c55b8625b0af74a1b9035ab997f7353aeffb2" "ebd933e1d834aa9525c6e64ad8f6021bbbaa25a48deacd0d3f480a7dd6216e3b" "7d52e76f3c9b107e7a57be437862b9d01b91a5ff7fca2524355603e3a2da227f" "19759a26a033dcb680aa11ee08677e3146ba547f1e8a83514a1671e0d36d626c" "99830ccf652abb947fd63a23210599483a14b1521291cd99aabae9c7ce047428" default))
 '(org-cycle-emulate-tab 'whitestart)
 '(package-selected-packages
   '(treemacs elpy all-the-haikus cheatsheet avy csv-mode dashboard shell-output-mode gcmh monicelli-mode all-the-icons-ibuffer color-identifiers-mode centaur-tabs all-the-icons-dired projectile all-the-icons flycheck cyberpunk-theme use-package the-matrix-theme monokai-theme mood-line org-inlinetask magit outshine javadoc-lookup benchmark-init go-mode sr-speedbar scala-mode cider clojure-mode))
 '(projectile-ignored-projects '("~/")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;; .emacs ends here
