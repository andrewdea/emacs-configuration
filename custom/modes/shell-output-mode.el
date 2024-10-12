;;; shell-output-mode.el --- Minor mode for displaying the output of shell commands -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022  Andrew De Angelis

;; Author: Andrew De Angelis <bobodeangelis@gmail.com>
;; Maintainer: Andrew De Angelis <bobodeangelis@gmail.com>
;; URL: https://github.com/andyjda/shell-output-mode
;; Version: 0.01.0
;; Package-Requires: ((cl-lib "1.0"))
;; Keywords: terminals, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TBD

;;; Code:

(setq shell-command-dont-erase-buffer t)
(defvar shell-async "&")
(defvar shell-latest-command nil)
(defvar find-default-options " -maxdepth 3 ")
(defvar shell-default-options nil)
(defvar grep-default-options "-nr -I")

(defun shell-output-execute-command (command)
  "Execute COMMAND and display output in new buffer.
The new buffer is called *shell-output-command*,
if the buffer already exists, switch to it (while leaving current buffer open),
 else, create it.  Then, update the `default-directory' to be the directory
in which the command was executed, and update latest-command"
  (let*
      ((output-buffer-name "*shell-command output*")
       (output-buffer (pop-to-buffer output-buffer-name)))
    (message (concat "Shell-output: executing command: " command
		     "\noutput-buffer: " (buffer-name output-buffer)))
    (goto-char (point-max))
    (shell-command command (buffer-name output-buffer))
    (insert (concat "\n➡️➡️ result of '" command
		    "'\n____________________________________________\n\n")))
  (shell-output-mode)
  (setq-local shell-latest-command (substring command)) ; copy by value
  (setq-local default-directory
	      (or (parse-directory-from-command command) default-directory))
  (message
   (format "set this shell's default-directory: %S from latest command: %S"
	   default-directory shell-latest-command)))

(defun parse-directory-from-command (command)
  "Given a shell COMMAND, find the directory-path.
This assumes the path was provided as an argument"
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
  "Put the latest command in the prompt to re-execute it."
  (interactive)
  (let ((default-command shell-latest-command))
    (shell-output-execute-command (my-prompt default-command))))

;;;###autoload
(defun find-here ()
  "Run the POSIX command `find' in the current folder."
  (interactive)
  (let ((default-command
	  (concat "find "
		  default-directory
		  find-default-options
		  shell-default-options
		  "-iname "
		  "'' "
		  shell-async)))
    (shell-output-execute-command (my-prompt default-command))))

;;;###autoload
(defun grep-here ()
  "Run the POSIX command `grep' in the current folder."
  (interactive)
  (let ((default-command
	  (concat "grep "
		  grep-default-options " "
		  shell-default-options " "
		  "'' "
		  default-directory " "
		  shell-async)))
    (shell-output-execute-command (my-prompt default-command))))

(defun my-prompt (default-command)
  "Put DEFAULT-COMMAND in prompt and place cursor in expected position."
  (read-from-minibuffer "shell command: "
			`(,default-command . ; initial contents
			  ,(+ 2 (string-match "'" default-command))))); cursor pos

;; get a file name from a line in the shell-output buffer
(defun parse-file-at-line ()
  "Parse the file name and line-number from text in shell-output buffer."
  (interactive)
  (if mark-active
      (progn (goto-char (max (region-beginning) (region-end)))
	     (deactivate-mark)
	     (while (and (not (eolp)) (not (char-equal ?/ (char-after))))
	       (right-char))
	     (let ((right-margin (point)))
	       (list
		(buffer-substring (move-beginning-of-line 1) right-margin))))
    ;; return list:
    ;; first element is the parsed file,
    ;; second element is the line-number (if present)
    (split-string (thing-at-point 'line 'no-properties) "\n\\|:")))

(defun shell-open-file-at-point ()
  "Open the file at point in shell-output buffer.
If line-number is present, switch to line and highlight it"
  (interactive)
  (let* ((parsed-file (parse-file-at-line))
	 (line (cadr parsed-file)))
    (find-file (car parsed-file))
    (if (> (length line) 0) ; if line -number present, select the line
	(progn
	  (goto-char (point-min))
	  (forward-line (string-to-number line))
	  (move-beginning-of-line 0)
	  (set-mark-command nil)
	  (move-end-of-line nil)))))

(defun shell-flush ()
  "Erase all the contents of the shell-output buffer.
This can be undone with normal `undo'"
  (interactive)
  (erase-buffer))

(defvar shell-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'shell-open-file-at-point)
    (define-key map (kbd "C-M-<backspace>") #'shell-flush)
    (define-key map (kbd "C-<up>") #'backward-paragraph) ; define paragraphs by file path mb?
    (define-key map (kbd "C-<down>") #'forward-paragraph)
    (define-key map (kbd "C-r") #'shell-redo) ; will have to think what key is best for this
    map))

(defcustom shell-output-mode-hook '()
  "Hook for customizing shell-output mode."
  :type 'hook
  :group 'shell-output)

;;;###autoload
(define-minor-mode shell-output-mode
  "Minor mode for shell output.

\\{shell-output-mode-map}"
  :lighter " Shell Output"
  :init-value nil
  :keymap shell-output-mode-map
  (message "set the shell-output-mode"))

(provide 'shell-output-mode)
;;; shell-output-mode.el ends here
