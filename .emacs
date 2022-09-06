;;; .emacs --- init file at HOME  -*- lexical-binding: t; -*-
;; Author: Andrew De Angelis

;;; Code:

;;;; NATIVE COMPILATION
;;;;; use same environment as terminal
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;;; don't show compilation warnings
;; keeping warnings on for now to monitor native comp til I'm familiar with it
;; (setq native-comp-async-report-warnings-errors nil)

;;;; BENCHMARK
;; benchmark-init to check where init is slow to load
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  ;; third arg is DEPTH: 100 means FUNCTION is added at the end of the hook list
  (add-hook 'after-init-hook #'benchmark-init/deactivate 100))

;;;; LOAD FASTER
;; not sure if these work, keeping an eye on them for now
(setq jit-lock-stealth-time nil)
(setq jit-lock-defer-time nil)
(setq jit-lock-defer-time 0.05)
(setq jit-lock-stealth-load 200)

;;;; PACKAGES
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package package)
(package-initialize)

(add-hook 'package-menu-mode-hook
	  (lambda () (hl-line-mode t) (visual-line-mode -1)))

;;;; CHEATSHEET

;; use updated source files since I'm working on this
(setq load-prefer-newer t)
(use-package cheatsheet
  :ensure t
  :config
  (add-hook 'kill-emacs-hook #'cheatsheet--save-list-to-elisp-file))

;;;; appearance: SIZING, FRAMES, WINDOWS, THEMES
;;;;; startup
;; don't show startup screen
(setq inhibit-startup-screen t)

;; scratch buffer in fundamental mode
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "**Welcome to Emacs!**\n\n\n")

;; choose default theme based on time of day
(defun default-theme ()
  (let ((hour (nth 2 (decode-time (current-time)))))
    (if (or (> hour 21) (< hour 8))
	'my-misterioso ; at night
      'tango-dark))) ; during the day

;; my daily default theme is based on standard tango-dark;
;; with some small edits in ~/.emacs.d/tango-dark-theme.el
;; I also really like monokai:
;; made some edits in ~/.emacs.d/my-monokai-theme.el

;; resize current frame (toggle)
(defun big-frame ()
  (interactive)
  (if (< (frame-parameter (selected-frame) 'width) 200)
      (set-frame-size (selected-frame) 203 55)
    (set-frame-size (selected-frame) 100 45))
  (set-frame-position (selected-frame) 0 0))

(defun startup-look (&optional arg)
  (interactive)
  (tool-bar-mode -1) ; I've never needed the toolbar
  (setq column-number-mode t)
  (load-theme (default-theme))
  (big-frame)
  (mood-line-mode t)
  (scroll-bar-mode -1)
  (global-visual-line-mode t)
  (if arg (find-file arg)))

(add-hook 'after-init-hook #'startup-look)

;;;;; dashboard
(defun dashboard-open ()
  "Open the *dashboard* buffer."
  (interactive)
  (delete-other-windows)
  ;; Refresh dashboard buffer
  (if (get-buffer dashboard-buffer-name)
      (kill-buffer dashboard-buffer-name))
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name))

(use-package dashboard
  :ensure t
  ;; :init
  ;; (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  ;; (add-hook 'dashboard-mode-hook 'my-dashboard-init)
  :config
  (setq dashboard-init-info
        (format "Emacs started in %s." (emacs-init-time)))
  (add-hook 'dashboard-mode-hook (lambda () (projectile-mode +1)))
  (
   setq haiku-dataset-file "~/.emacs.d/custom/datasets/haiku-dataset.csv")

  (defun get-random-haiku ()
    (with-temp-buffer
      (insert-file-contents haiku-dataset-file)
      (forward-line (random (count-lines (point-min) (point-max))))
      (let ((line (thing-at-point 'line 'no-properties)))
	(string-join (butlast (split-string line ",") 4) "\n"))))

  (defun find-haiku-in-file ()
    (interactive)
    (let ((line (string-trim (thing-at-point 'line 'no-properties))))
      (switch-to-buffer (find-file-other-window haiku-dataset-file))
      (search-forward line)
      (set-mark-command nil)
      (move-end-of-line nil)))

  (setq dashboard-footer-icon (all-the-icons-octicon "book"
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

  (add-hook 'dashboard-mode-hook
            (lambda () (local-set-key (kbd "C-<return>") #'find-haiku-in-file)))

  ;; (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 10)
                          (projects . 5)
			  ;; bookmarks
                          (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-week-agenda t)
  (setq dashboard-agenda-release-buffers t)
  (setq dashboard-footer-messages
	(list (get-random-haiku)))
  (setq dashboard-startup-banner 'logo))

;;;;; resizing and movement
;; make current window bigger or smaller
(defun wbig (&optional arg) (interactive "P")
       (if arg () (setq arg 25))
       (enlarge-window-horizontally arg)
       (message (concat "expanded window by " (number-to-string arg))))

(defun wsmall (&optional arg) (interactive "P")
       (if arg () (setq arg 25))
       (shrink-window-horizontally arg)
       (message (concat "reduced window by " (number-to-string arg))))

;; frame to have together with max youtube
(defun yt-frame ()
  (interactive)
  (delete-other-windows)
  (set-frame-size (selected-frame) 83 52)
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
(defun inkpot  ()
  (interactive)
  (un-theme "inkpot"))
(defun cyberpunk  ()
  (interactive)
  (un-theme "cyberpunk"))

;; this highlights characters beyond the 80 char limit
(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(face lines-tail trailing)))
(defun wspace () (interactive) (whitespace-mode 'toggle))

;; long line to test whitespace-mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; tabs
(use-package centaur-tabs
  :ensure t
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
;;;;; appearance for specific modes
;;;; ORG mode
(use-package org
  :ensure t
  :defer 3
  :config
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
  (defun electric-fontify-will-use-region ()
    (message "in el-fontify will use region")
    (message (format "returning: %s" (let ((fontify-list '(?* ?/ ?_ ?= ?~ ?+)))
      (and transient-mark-mode mark-active
	   (member last-command-event fontify-list)))))
    (let ((fontify-list '(?* ?/ ?_ ?= ?~ ?+)))
      (and transient-mark-mode mark-active
	   (member last-command-event fontify-list))))

  (put 'insert-char 'delete-selection 'delete-selection-uses-region-p)

  (defun electric-fontify ()
    "If in org-mode (or a derived mode),
when a region is highlighted and we've inserted a character that fontifies text,
the whole region is fontified (by automatically inserting character at mark)"
    (message "in electric fontify!")
    (if (derived-mode-p 'org-mode)
	(if (electric-fontify-will-use-region)
	    (progn (exchange-point-and-mark)
		   (insert last-command-event)))))

  (add-hook 'post-self-insert-hook #'electric-fontify-will-use-region)
  (add-hook 'self-insert-uses-region-functions
	    #'electric-fontify)

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
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)))
(setq all-the-icons-dired-monochrome nil)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;;;; find and grep
(require 'shell-output-mode "~/.emacs.d/custom/modes/shell-output-mode.el")

;;;;; recent files
(use-package recentf
  :ensure t
  :bind (:map recentf-mode-map
	      ("C-c C-r" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (add-to-list 'recentf-exclude "ido.last")
  :hook (after-init . recentf-mode)
  )

;;;;; projectile mode
(use-package projectile
  :ensure t
  :config
  ;; (setq projectile-ignored-projects '("~/"))
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)))

;;;; TEXT EDITING
;;;;; utilities
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

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

;; don't think this is needed any more because of the above
;; (defun smart-yank ()
;;   "When region is highlighted, kill current region and call yank."
;;   (interactive)
;;   (if mark-active
;;       (progn (delete-region (region-beginning) (region-end)) (yank))
;;     (yank)))
;; remap C-y to `smart-yank'
;; (global-set-key (kbd "C-y") #'smart-yank)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;; flyspell
(use-package flyspell
  :ensure t
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
(defun region-copy-comm (&optional arg) (interactive "p")
       (kmacro-exec-ring-item (quote ("\M-w\C-x\C-x\M-;\n" 0 "%d")) arg)
       (message "commented region has been copied"))
(defun line-copy-comm (&optional arg)
  (interactive "p")
  (kmacro-exec-ring-item (quote ([?\C-a ?\C-  ?\C-e ?\M-w ?\C-x ?\C-x
					?\M-\; ?\C-e return]
				 0 "%d"))
			 arg)
  (message "commented line has been copied"))
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
  (replace-string arg (concat comment-start arg)))

(defun uncomment-all (arg)
  (interactive "sstring to uncomment: ")
  (let ((commented-arg (concat comment-start arg)))
    (replace-string commented-arg
		    (string-replace comment-start "" commented-arg))))

;;;;; autocomplete
(use-package company
  :ensure t
  :config
  (setq company-dabbrev-downcase nil)
  :bind (:map company-mode-map
	      ("s-RET" . company-abort)))
(global-company-mode t)

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

;; might not be needed if I like the ido functionalities
;; (defun my-switch-to-buffer (arg)
;;   "find all buffers whose name contains the argument,
;; if there is one match, open that buffer;
;; if there are multiple matches, display them in a *Buffer List*;
;; if there are no matches, open a new buffer and display *Buffer List*"
;;   ;; TODO: figure out why I'm getting the format error here
;;   ;; (interactive "sSwitch to buffer: (default %s)"
;;   ;; 		       (buffer-name (other-buffer (current-buffer))))
;;   (interactive "sSwitch to buffer: ")
;;   (let ((matching (get-matching-buffers arg)))
;;     (if (car matching)
;; 	(if (cdr matching)
;; 	    (display-buffer (list-buffers-noselect nil matching))
;; 	  (switch-to-buffer (car matching)))
;;       (progn (message "No buffer with name %S found: created a new one" arg)
;; 	     (switch-to-buffer arg)
;; 	     (display-buffer (list-buffers-noselect))))))
;; ;; remap \C-xb to `my-switch-to-buffer'
;; (global-set-key (kbd "\C-xb") 'my-switch-to-buffer)

;;;; SPEEDBAR
(defun bar-toggle ()
  (interactive)
  (sr-speedbar-toggle))
(defun bar-open ()
  (interactive)
  (sr-speedbar-open))

(use-package sr-speedbar
  :ensure t
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
	(next-line)
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

;;;; PROGRAMMING support and utilities
;;;;; ido completion mode
(setq ido-everywhere t)
(ido-mode 1)
;;;;; git
(defun vc-refresh-buffer (arg)
  (set-buffer arg)
  (vc-refresh-state))

(use-package magit
  :ensure t
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
  (linum-mode t)
  ;; (color-identifiers-mode t)
  (electric-pair-local-mode t))
(add-hook 'prog-mode-hook #'my-prog-appearance)
;;;;; outline
(use-package dash :ensure t)
(use-package outshine
  :ensure t
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
  "Gets the symbol at the cursor's current location,
moves to the beginning of the file and searches for that symbol"
  (interactive)
  (let ((my-symbol (thing-at-point 'symbol 'no-properties)))
    (if (null my-symbol)
	(message "No symbol at point")
      (progn (point-to-register 'r)
	     (beginning-of-buffer)
	     (goto-char (search-forward-regexp (isearch-symbol-regexp my-symbol)))
	     (isearch-forward-symbol-at-point)))))

;; only use find-symbol-first occurrence as a weak alternative in cases
;; where the backend for xref has not been set
(defun my-find-definition ()
  (interactive)
  "If an xref-backend has been set, call xref-find-definitions,
else, call find-symbol-first-occurrence"
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
(global-set-key (kbd "C-h j") #'javadoc-lookup)

(defun on-java-loaded ()
  (define-key java-mode-map (kbd "C-i") #'javadoc-add-import))

(add-hook 'java-mode-hook #'on-java-loaded)

(add-hook 'java-mode-hook #'subword-mode)

;; scala
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))

;; lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl") ; slime

;; monicelli
;; adding customization path
(add-to-list 'load-path "~/.emacs.d/custom/modes")

;; loading monicelli mode
(autoload 'monicelli-mode "~/.emacs.d/custom/modes/monicelli-mode.el")
(add-to-list 'auto-mode-alist '("\\.mc\\'" . monicelli-mode))

;;;;; templates
(defun template-trim-name (file-name &optional file-ext)
  "Find and replace file-path from FILE-NAME,
if provided find and replace FILE-EXT also"
  (replace-regexp-in-string ".*\/" ""
			    (if file-ext
				(string-replace file-ext "" file-name)
			      file-name)))
(defun template-write-to-buffer (contents-as-string)
  "Write CONTENTS-AS-STRING to the current buffer
(if the buffer is not empty, CONTENTS-AS-STRING will be appended to the end)"
  (with-current-buffer (buffer-name)
    (goto-char (point-max))
    (insert contents-as-string)))
(defun template-file-to-string (file-name)
  "Return contents of FILE-NAME as string."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))
(defun template-set-contents (file-name file-ext)
  "Read from the template file and writes to buffer the contents,
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
  (substring file-name (string-match "\.[^.]*$" file-name)))
(defun template-open ()
  "Prompt for file name, find the file, ask for confirmation,
and set its contents as the appropriate programming-language-template"
  (interactive)
  (setq file-name (read-file-name "Enter the name of your file:"))
  (setq file-ext  (get-file-ext file-name))
  (find-file file-name)
  (message "Opened %s" file-name)
  (if (equal "y" (read-string "Write your template? y/n: "))
      (template-set-contents file-name file-ext)
    )
  )

;;;; RANDOM STUFF
;;; CUSTOM-added variables and faces
;; my custom-safe-themes are inkpot (really good for org-files),
;; cyberpunk, the-matrix, my-misterioso, my-monokai, and tango-dark.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ebd933e1d834aa9525c6e64ad8f6021bbbaa25a48deacd0d3f480a7dd6216e3b" "2f7247b7aa8aeccbc385ed2dd6c3576ac82c00ef0d530422969727110426044c" "f9bd650eff0cf6c64eb4cf7b2f5d00819ff687198d90ab37aca02f2234524ac7" "e5dc4ab5d76a4a1571a1c3b6246c55b8625b0af74a1b9035ab997f7353aeffb2" "19759a26a033dcb680aa11ee08677e3146ba547f1e8a83514a1671e0d36d626c" "c2f4b626fdab4b17dc0e5fb488f4f831382f78c526744839113efc8d5e9a75cb" "86c6fccf6f3f969a0cce5e08748830f7bfdcfc14cea2e4b70f7cb03d4ea12843" default))
 '(org-cycle-emulate-tab 'whitestart)
 '(package-selected-packages
   '(cheatsheet color-identifiers-mode centaur-tabs all-the-icons-dired projectile all-the-icons dashboard flycheck cyberpunk-theme exec-path-from-shell use-package alda-mode the-matrix-theme monokai-theme mood-line org-inlinetask magit outshine javadoc-lookup benchmark-init inkpot-theme go-mode sr-speedbar scala-mode cider clojure-mode slime))
 '(projectile-ignored-projects '("~/")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
