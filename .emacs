;;; .emacs --- init file at HOME  -*- lexical-binding: t; -*-
;; Author: Andrew De Angelis

;;; Code:

;;;; BENCHMARK
;; benchmark-init to check where init is spending time

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;;;; MELPA and package stuff
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

(defun my-hl-line-hook ()
  (hl-line-mode t))
(add-hook 'package-menu-mode-hook #'my-hl-line-hook)

;;;; appearance: SIZING, FRAMES, WINDOWS, THEMES
;;;;; startup
;; use updated source files
(setq load-prefer-newer t)
;; show column numbers
(setq column-number-mode t)
;; default theme: tango-dark; load a different theme at night
(setq hour (nth 2 (decode-time (current-time))))
(if (or (> hour 21) (< hour 8))
    (load-theme 'inkpot t) ; I really like the way org files look in this
  (load-theme 'tango-dark)) ; default

;; TODO thinking of adding this to tango-dark:
;; (set-face-attribute 'region nil :background "#483d8b")
;; need to figure out how to do it elegantly
;; I might also like the purple that's used by inkpot when highlighting
;; not sure what it is though

;; resize current frame
(defun big-frame ()
  (interactive)
  (if (< (frame-parameter (selected-frame) 'width) 200)
      (set-frame-size (selected-frame) 202 55)
    (set-frame-size (selected-frame) 100 45))
  (set-frame-position (selected-frame) 0 0))

;; default to big-frame
(big-frame)

;; inhibit startup screen when opening a file
(defun my-inhibit-startup-screen ()
  (ignore (setq inhibit-startup-screen t)))
(add-hook 'command-line-functions #'my-inhibit-startup-screen)

;;;;; resizing
;; make current window bigger or smaller
(defun wbig (&optional arg) (interactive "P")
       (if arg () (setq arg 25))
       (kmacro-exec-ring-item (quote ("\C-x}" 0 "%d")) arg)
       (message (concat "expanded window by " (number-to-string arg))))

(defun wsmall (&optional arg) (interactive "P")
       (if arg () (setq arg 25))
       (kmacro-exec-ring-item (quote ("\C-x{" 0 "%d")) arg)
       (message (concat "reduced window by " (number-to-string arg))))

;; frame to have together with max youtube
(defun yt-frame ()
  (interactive)
  (set-frame-size (selected-frame) 83 52)
  (set-frame-position (selected-frame) 838 24))
(add-hook 'tetris-mode-hook #'yt-frame)

;;;;; themes and colors
;; this highlights characters beyond the 80 char limit
(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(face lines-tail trailing)))
(defun wspace () (interactive) (whitespace-mode))

;; long line to test whitespace-mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; define a misterioso theme with 'charcoal' background and 'tea rose' cursor
(defun my-misterioso ()
  (interactive)
  (load-theme 'misterioso)
  (set-background-color "#232B2B"); "#36454F")
  (set-cursor-color "#F88379") ; thinking of dark orange as well: #FF8C00

  ;; using a dark cyan as color for highlighted region
  ;; not set on it, but the default is hard to notice on dark background
  ;; have also used #232B2B, which is darker and easier on the eyes
  (set-face-attribute 'region nil :background "#008B8B")
  )

(defun un-theme (&optional arg)
  "disables all custom themes
and loads the optional argument.
the my-misterioso theme is loaded
when the argument consists of a
substring of 'my-misterioso'"
  (interactive "snew theme: ")
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (if (not (string= "" arg))
      (if (string-match-p arg "my-misterioso")
	  (my-misterioso)
	(load-theme (intern arg)))))

;;;;; appearance for specific modes
(add-hook 'dired-mode-hook
	  (lambda () (visual-line-mode t)))

;;;; ORG mode
(use-package org
  :ensure t
  :config
  (define-key global-map "\C-cs" #'org-store-link)
  (define-key global-map "\C-cl" #'org-insert-link)
  (define-key global-map "\C-c \C-o" #'org-open-at-point)
  (define-key global-map "\C-ca" #'org-agenda)
  (define-key global-map (kbd "C-M-<backspace>") #'org-cut-subtree) ;; todo: move this to the :bind section!!

  (setq org-hide-emphasis-markers t)

  ;; better bullet-points
  (font-lock-add-keywords #'org-mode
                          '(("^ +\\([-]\\) "
                             (0 (prog1 ()
				  (compose-region (match-beginning 1)
						  (match-end 1) "•"))))))

  (setq org-log-done t)

  (defun org-refresh-agenda ()
    (setq org-agenda-files (directory-files "~/org" nil "org$")))
  (org-refresh-agenda)
  (defun my-org-tab ()
    "if current line is a heading, call regular org-cycle;
else, first move to previous visible heading, then call it"
    (interactive)
    (move-beginning-of-line 1)
    (if (null (looking-at org-outline-regexp))
	(org-previous-visible-heading 1))
    (org-cycle))

  (defun open-file-same-window () (interactive)
	 (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file)
	 (org-open-at-point)
	 (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file-other-window))
  
  :bind (("C-c o" . open-file-same-window)
	 :map org-mode-map
	 ("TAB" . my-org-tab)))

(add-hook 'org-mode-hook
	  (lambda () (visual-line-mode t)))

;;;; FILE SHORTCUTS
;; open init file
(fset 'init
      (lambda (&optional arg) (interactive)
	(find-file "~/.emacs")))

;; open HackerRank
(fset 'hacker-rank
      (lambda (&optional arg) (interactive)
	(find-file "~/desktop/HackerRankProblems/")))

;; open zsh profile
(fset 'zsh-profile
      (lambda (&optional arg) (interactive)
	(find-file "/Users/andrewdeangelis/.zshenv")))
;; open bash profile
(fset 'bash-profile ; note: I switched shell to zsh, this might no longer be needed
      (lambda (&optional arg) (interactive)
	(find-file "/Users/andrewdeangelis/.bash_profile")))

;; open org folder
(fset 'forg
      (lambda (&optional arg) (interactive)
	(find-file "~/org")))
;; open mobile org folder
(fset 'beorg
      (lambda (&optional arg) (interactive)
	(find-file "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")))
;; open generic todo
(fset 'todo
      (lambda (&optional arg) (interactive)
	(find-file "~/org/TODO.org")))
;; open generic notes
(fset 'notes
      (lambda (&optional arg) (interactive)
	(find-file "~/org/Notes.org")))

;;;; TEXT EDITING
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

;; SEARCH from beginning of document
(global-set-key (kbd "M-s")
		(lambda () (interactive)
		  (point-to-register 'r)
		  (beginning-of-buffer) (isearch-forward)))

;; search from end of document
(global-set-key (kbd "M-r")
		(lambda () (interactive)
		  (point-to-register 'r)
		  (end-of-buffer) (isearch-backward)))

;; get back to where search started
;; not sure why I have to press this twice
(global-set-key (kbd "M-b")
		(lambda () (interactive)
		  (jump-to-register 'r)))

;; when region is highlighted,
;; a call to yank is also call to kill current region
(defun smart-yank ()
  (interactive)
  (if mark-active
      (progn (delete-region (region-beginning) (region-end)) (yank))
    (yank)))
;; remap M-; to `smart-yank'
(global-set-key (kbd "C-y") #'smart-yank)


;; Editing macro:
;; copies line/region and comments it out
(fset 'region-copy-comm
      (lambda (&optional arg) (interactive "p")
	(kmacro-exec-ring-item (quote ("\M-w\C-x\C-x\M-;\n" 0 "%d")) arg)
	(message "commented region has been copied")))

(fset 'line-copy-comm
      (lambda (&optional arg) (interactive "p")
	(kmacro-exec-ring-item (quote ([?\C-a ?\C-  ?\C-e ?\M-w ?\C-x ?\C-x
					      ?\M-\; ?\C-e return] 0 "%d"))
			       arg)
	(message "commented line has been copied")))

(defun copy-com ()
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

;; autocomplete
(use-package company
  :ensure t)
(global-company-mode t)

;;;; BUFFER AND FRAME movements
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
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
  "find all buffers whose name contains the argument"
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
(use-package sr-speedbar
  :ensure t
  :config
  (defun bar-toggle ()
    (interactive)
    (sr-speedbar-toggle))
  (defun bar-open ()
    (interactive)
    (sr-speedbar-open))
  (defun bar-close ()
    (interactive)
    (sr-speedbar-close))
  (defun bar-refresh ()
    (interactive)
    (sr-speedbar-refresh-turn-on))
  (add-hook 'speedbar-mode-hook
	    (lambda () (visual-line-mode t))))

;;;; PROGRAMMING support and utilities
;;;;; ido completion framework
(setq ido-everywhere t)
(ido-mode 1)
;;;;; appearance
(defun my-prog-appearance ()
  (linum-mode t)
  (electric-pair-local-mode t)
  (visual-line-mode t))
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

  (defun my-next-visible-heading ()
    (interactive)
    (if (null (looking-at outline-regexp))
	(outline-next-visible-heading 1))
    (forward-paragraph))
  (defun my-previous-visible-heading ()
    (interactive)
    (if (null (looking-at outline-regexp))
	(outline-previous-visible-heading 1))
    (backward-paragraph))
  
  :bind (:map outshine-mode-map
	      ("TAB" . my-outline-tab)
	      ("M-<down>" . outline-move-subtree-down)
	      ("M-<up>" . outline-move-subtree-up)))

;; outshine
(add-hook 'outline-minor-mode-hook #'outshine-mode)
;; enable outline-minor-mode for *ALL* programming buffers
(add-hook 'prog-mode-hook #'outline-minor-mode)

;;;;; helper functions
;; implemented my own, extremely dumb version of dumb jump
;; possible enhancement:
;; when not found, try to search outside the current file
(defun find-symbol-first-occurrence ()
  "gets the symbol at the cursor's current location,
moves to the beginning of the file and searches for that symbol"
  (interactive)
  (setq my-word (thing-at-point 'symbol 'no-properties))
  (message "looking for symbol: %s" my-word)
  (if (null my-word)
      (message "No symbol at point")
    (progn (point-to-register 'r)
	   (beginning-of-buffer)
	   (goto-char (search-forward-regexp (isearch-symbol-regexp my-word)))
	   (isearch-forward-symbol-at-point)
	   ;; TODO: [check if final point is same as starting point*, then
	   ;; add a function to ask whether we should use grep,
	   ;; provide a default folder,
	   ;; open a new shell and call grep on that folder]
	   ;; * +/- length of symbol
	   )))

(global-set-key (kbd "M-.") #'find-symbol-first-occurrence)

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

;; scala
(add-to-list 'auto-mode-alist '("\.sc" . scala-mode))

;; monicelli
;; adding customization path
(add-to-list 'load-path "~/.emacs.d/custom/modes")

;; loading monicelli mode
(autoload 'monicelli-mode "~/.emacs.d/custom/modes/monicelli-mode.el")
(add-to-list 'auto-mode-alist '("\\.mc\\'" . monicelli-mode))

;;;;; templates
(defun template-trim-name (file-name &optional file-ext)
  "find and replace file-path from FILE-NAME,
if provided find and replace FILE-EXT also"
  (replace-regexp-in-string ".*\/" ""
			    (if file-ext
				(string-replace file-ext "" file-name)
			      file-name)))
(defun template-write-to-buffer (contents-as-string)
  "write CONTENTS-AS-STRING to the current buffer
(if the buffer is not empty, CONTENTS-AS-STRING will be appended to the end)"
  (with-current-buffer (buffer-name)
    (goto-char (point-max))
    (insert contents-as-string)))
(defun template-file-to-string (file-name)
  "return contents of FILE-NAME as string"
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))
(defun template-set-contents (file-name file-ext)
  "read from the template file and writes to buffer the contents,
replacing 'Template' with FILE-NAME"
  (setq file-contents
	(template-file-to-string
	 (concat "~/.emacs.d/custom/programming-language-templates/Template"
		 file-ext)))
  (template-write-to-buffer
   (string-replace "Template"
		   (template-trim-name file-name file-ext)
		   file-contents)))
(defun get-file-ext (&optional file-name)
  (if (equal nil file-name) (setq file-name buffer-name))
  (substring file-name (string-match "\.[^.]*$" file-name)))
(defun template-open ()
  "prompt for file name, find the file, ask for confirmation,
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

(defun java-print ()
  (interactive)
  (setq contents-as-string
	(template-file-to-string
	 "~/.emacs.d/custom/programming-language-templates/PrintLn.java"))
  (with-current-buffer (buffer-name)
    (insert contents-as-string)))


;;;; RANDOM STUFF
;; this should help with slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;;;;; scratch place to try out stuff

(require 'ytdl);; TODO try this out

;; some scratch stuff that might help with compiling
;; compile short cut
;; (defun cmp ()
;;   (interactive)
;;   (setq file-ext (get-file-ext))
;;   (setq prog-language


;; 	(setq programming-language (cond ((equal ".java" file-ext) "c")
;; 					  ((equal ".c" file-ext) "c")
;; 					  (equal ".sc" file-ext) "scala"))

;; 	auto-mode-alist

;; 	(symbol-name (cdr (quote ("\\.tzst\\'" . tar-mode))))

;;
;;

;;; CUSTOM-added variables and faces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e1f160fa86a1b1caf048291afc747dee2bd71a90004618eb43b3011439c23651" "bbec550ad7022c5bde15ca81819ed94401144fe4c9242cad10a0c7bfea2c1982" default))
 '(org-cycle-emulate-tab 'whitestart)
 '(package-selected-packages
   '(magit outshine ytdl javadoc-lookup benchmark-init inkpot-theme go-mode sr-speedbar scala-mode cider clojure-mode slime))
 '(speedbar-show-unknown-files t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
