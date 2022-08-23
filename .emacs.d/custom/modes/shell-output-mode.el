;;; find-output-mode.el --- minor mode for displaying the output of shell commands, optimized for 'find' and 'grep'. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2017, by you

;; Author: Andrew De Angelis ( bobodeangelis@gmail.com )
;; Version: 0.0.1
;; Created: 18 Aug 2022

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; wrapping shell-command with a handy interface providing a couple utilities

;; the shell-command output is displayed in a new buffer,
;; you can open the files listed in the new buffer with C-RET;
;; if you want to open a directory leading to a file, just highlight the directory
;; (or part of its name) and press C-RET. If the file includes a line-number
;; (eg after using grep command), C-RET also brings you to that line
;; and highlights it.
;; Additional utilities:
;; shell-redo: re-run the latest command
;; shell-flush: delete all of current buffer's contents

;;; Code:

(setq shell-command-dont-erase-buffer t)
(setq shell-async "&")
(setq shell-latest-command nil)
(setq find-default-options " -maxdepth 3 ")
(setq shell-default-options nil)
(setq grep-default-options "-nr")

;; look for file in current directory
(defun execute-command (command)
  (let*
      ((output-buffer-name "*shell-command output*")
       (output-buffer (my-switch-to-buffer output-buffer-name)))
    (message (concat "\nexecuting command: " command " from execute-command,"
		     " output-buffer: " (buffer-name output-buffer)))
    (end-of-buffer)
    (shell-command command (buffer-name output-buffer))
    (insert (concat "\n➡️➡️ result of " command
		    "\n____________________________________________\n\n")))
  (shell-output-mode)
  (setq-local shell-latest-command (substring command)) ; copy by value
  (setq-local default-directory
	      (or (parse-folder-from-command command) default-directory))
  (message
   (format "set local default-directory: %S from latest command: %S"
	   default-directory shell-latest-command)))

(defun parse-directory-from-command (command)
  ;; remove the potential '&' character at the end of the string
  (let ((adj-size (- (length command) 1)))
    (if (equal (aref command adj-size) ?&)
	(aset command adj-size ?\s)))
  ;; parse according to command name
  (let* ((split (split-string command))
	 (name (pop split))
	 (directory
	  (cond ((string-equal name "find") ; the first non-option is the dir
		 (seq-find (lambda (arg)
			     (not (string-match "\\-" arg)))
			   split))
		((string-equal name "grep") ; last arg is dir
	         (car (last split))))))
    ;; could update this to file-exists-p
    ;; if I want grep to keep track of which file it searched
    (if (file-directory-p directory) ; only return if it actually exists
	directory)))


(defun shell-redo ()
  (interactive)
  (let ((default-command shell-latest-command))
    (execute-command (read-from-minibuffer "shell command: " default-command))))

(defun my-switch-to-buffer (arg)
  (if (string-equal (buffer-name) arg)
      (switch-to-buffer arg)
    (switch-to-buffer-other-window arg)))

(defun find-here ()
  (interactive)
  (let ((default-command
	  (concat "find "
		  default-directory
		  find-default-options
		  shell-default-options
		  "-iname "
		  shell-async)))
    (execute-command (read-from-minibuffer "shell command: " default-command))))

(defun grep-here ()
  (interactive)
  (let ((default-command
	  (concat "grep "
		  grep-default-options " "
		  shell-default-options " "
		  "'' "
		  default-directory " "
		  shell-async)))
    (execute-command (read-from-minibuffer "shell command: " default-command))))

(defun my-shell-command (arg)
  (interactive "sshell command: ")
  (execute-command arg))

(defun parse-file-at-line ()
  (interactive)
  (if mark-active
      (progn (goto-char (max (region-beginning) (region-end)))
	     (deactivate-mark)
	     (while (and (not (eolp)) (not (char-equal ?/ (char-after))))
	       (right-char))
	     (let ((right-margin (point)))
	       (list
		(buffer-substring (move-beginning-of-line 1) right-margin))))
    (split-string (thing-at-point 'line 'no-properties) "\n\\|:")))

(defun so-open-file-at-point ()
  (interactive)
  (let* ((parsed-file (parse-file-at-line))
	 (line (cdr parsed-file)))
    (find-file (car parsed-file))
    (if line ; select the line
	(progn
	  (goto-line (string-to-number (car line)))
	  (set-mark-command nil) (move-end-of-line nil)))))

(defun so-flush ()
  (interactive)
  (erase-buffer))

(defvar shell-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'so-open-file-at-point)
    (define-key map (kbd "C-M-<backspace>") #'so-flush)
    (define-key map (kbd "C-<up>") #'backward-paragraph) ; define paragraphs by file path mb?
    (define-key map (kbd "C-<down>") #'forward-paragraph)
    (define-key map (kbd "C-r") #'shell-redo) ; will have to think what key is best for this
    map))

(defcustom shell-output-mode-hook '()
  "Hook for customizing find-output mode."
  :type 'hook
  :group 'shell-output)

;;;###autoload
(define-minor-mode shell-output-mode
  "minor mode for shell output"
  :lighter "shell-output"
  :init-value nil
  :keymap shell-output-mode-map
  (message "set the shell-output-mode"))

;; add the mode to the `features' list
(provide 'shell-output-mode)
;;;
