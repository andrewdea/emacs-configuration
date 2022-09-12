;;; all-the-haikus.el --- functionalities to read from csv file of haikus -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Andrew De Angelis ( bobodeangelis@gmail.com )
;; Version: 0.0.1
;; Created: 8 Sep 2022

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; TBD
;; important to add description of dataset and format

;;; Code:
(eval-when-compile (require 'cl-lib))

(defgroup all-the-haikus nil
  "Read haikus from csv file."
  :group 'convenience) ; leisure/art/something like that?

(defcustom haiku-dataset-file
  "~/.emacs.d/custom/datasets/haiku-dataset.csv"
  "Comma-separated file containing our list of haikus."
  :group 'all-the-haikus
  :type 'file)

(defvar within-quotes nil
  "Boolean to keep track of where we are while iterating a string.")

(defvar generated-poem-marker "*generated*"
  "String that will be placed at the beginning of a generated poem.")

(defun comma-to-newline-except-within-quotes (c)
  "Return `char-to-string' of C or, if C is a comma, of newline.
Use `within-quotes' to keep track of whether
the string contains a quote before the current char:
if a quote was read and no quote has closed it yet, don't convert to newline"
  (if (eq c ?\")
      (setq-local within-quotes ; toggle it
		  (not within-quotes)))
  (if (and (eq c ?,) ; char is comma
	   (not within-quotes)) ; we are currently outside of quotes
      ?\n
    c))

(defmacro with-haiku-temp-buffer (&rest body)
  "Execute BODY inside of a temporary buffer with the haiku file.
If we're already in the haiku temp buffer, just execute body
else open a new one and insert the file contents."
  (if (boundp 'temp-haiku-buffer)
      `(progn ,@body)
    (progn
      `(with-temp-buffer
	 (insert-file-contents-literally haiku-dataset-file)
	 (setq-local temp-haiku-buffer (current-buffer))
	 ,@body))))

(defun get-haiku-line (line-number)
  "From the haiku-dataset-file, read line at LINE-NUMBER.
Return it as a list of strings"
  (with-haiku-temp-buffer
   (goto-char (point-min))
   (forward-line (- line-number 1))
   (let* ((line (thing-at-point 'line 'no-properties))
	  (parsed
	   (concat (mapcar 'comma-to-newline-except-within-quotes line))))
     (split-string parsed "\n"))))

(defun get-random-haiku-line ()
  "Read a random line from the haiku-dataset-file."
  (with-haiku-temp-buffer
   (get-haiku-line
    (+ 1 ; lowerbound of random: excludes the header
       (random
	(count-lines (point-min) (point-max)))))))

(defun format-haiku-just-text (list-of-strings)
  "Given a LIST-OF-STRINGS, return a newline-separated single string.
Remove any \" characters."
  (string-replace "\"" ""
		  (string-join list-of-strings "\n")))

(defun find-haiku-from-line-at-point ()
  "Read the line at point, open the haiku file and search for line."
  (interactive)
  (let ((line (string-trim (thing-at-point 'line 'no-properties))))
    (switch-to-buffer (find-file-other-window haiku-dataset-file))
    (goto-char (point-min))
    (search-forward line)
    (move-beginning-of-line 1)
    (set-mark-command nil)
    (move-end-of-line nil)))

(defun get-random-matching-line (to-find)
  "Go to random point in file, search forward for regexp TO-FIND.
If no results are found from start-point to end,
search from beginning to start-point."
  (let ((start-point ;random start
	 (random (point-max))))
    (goto-char start-point)
    (if (or
	 (search-forward-regexp to-find nil t) ; try from here to the end
	 (progn (goto-char (point-min)) ; else from beginning to here
		(search-forward-regexp to-find start-point t)))
	(get-haiku-line (line-number-at-pos))
      (progn (message "%s not found." to-find) nil))))

(defun get-haiku-with-format (syllable-count)
  "Given a SYLLABLE-COUNT, find a random haiku with this format.
TODO: Describe what syllable-count looks like."
  (let ((to-find
	 (mapconcat (lambda (arg)
		      (if (stringp arg)
			  (concat "\"" arg "\"")
			(number-to-string arg)))
		    syllable-count ",")))
    (with-haiku-temp-buffer
     (let ((found
	    (get-random-matching-line (regexp-quote to-find))))
       (or found ; if found, return it
	   (progn
	     (message "no poem with format (%s) found :(" to-find)
	     nil)))))) ; else return nothing

(defun generate-poem-with-format (syllable-count)
  "Given a SYLLABLE-COUNT, create a new poem with matching lines."
  (message "searching for this syllable count: %s"
	   syllable-count)
  (with-haiku-temp-buffer
   (let* ((first-regexp (format "[[:alpha:]],%S," (car syllable-count)))
	  (second-regexp (format "[[:digit:]]\"?,%S,"(nth 1 syllable-count)))
	  (third-regexp (format "[[:digit:]]\"?,%S$"(nth 2 syllable-count)))
	  (to-find-list (list first-regexp second-regexp third-regexp))
	  (found-list ; the three matching lines from the file
	   (mapcar #'get-random-matching-line to-find-list))
	  (just-the-right-lines ; three strings that will make our poem
	   ;; preserve the order of the lines:
	   ;; for the first line in our new poem,
	   ;; take a line that was originally a first line in its poem
	   ;; apply the same logic for second line, etc
	   ;; this makes the sentences more coherent/meaningful
	   (cl-mapcar #'nth (number-sequence 0 2) found-list)))
     (format-haiku-just-text just-the-right-lines))))

;;;###autoload
(defun write-me-a-haiku (&optional syllable-count)
  "Writes a new poem.
If SYLLABLE-COUNT provided, generate a poem with that format.
Else, generate a poem by getting three random lines.
The returned poem has an additional first line with the `generated-poem-marker'.
When called interactively, the poem is displayed in the minibuffer."
  (interactive)
  (concat generated-poem-marker "\n"
	  (if (and syllable-count (listp syllable-count))
	      (generate-poem-with-format syllable-count)
	    (let* ((found-list ; three random lines from file
		    (mapcar 'funcall (make-list 3 'get-random-haiku-line)))
		   (just-the-right-lines ; three strings that will make our poem
		    ;; preserve the order of the lines:
		    ;; for the first line in our new poem,
		    ;; take a line that was originally a first line
		    ;; apply the same logic for second line, etc
		    ;; this makes the sentences more coherent/meaningful
		    (cl-mapcar #'nth (number-sequence 0 2) found-list))
		   (haiku (format-haiku-just-text just-the-right-lines)))
	      (if (called-interactively-p 'any)
		  (message haiku) ; display in minibuffer
		haiku)))))

;;;###autoload
(defun find-me-a-haiku (&optional arg)
  "Read a haiku from haiku-dataset-file.  Return as string of poetry.
If ARG is a number or a marker, read the haiku at that line-number.
If arg is a non-nil list, look for a haiku with this syllable-count
\(see get-haiku-with-format).
else (including if ARG is nil) read haiku from a random line-number.
When called interactively, the poem is displayed in the minibuffer."
  (interactive "P")
  (let* ((haiku-line
	  (cond ((and arg (listp arg))
		 (get-haiku-with-format arg))
		((integer-or-marker-p arg)
		 (get-haiku-line arg))
		(t
		 (get-random-haiku-line))))
	 (haiku
	  (format-haiku-just-text (butlast haiku-line 5))))
    (if (called-interactively-p 'any)
	(message haiku) ; display in minibuffer
      haiku)))

;; add package to the `features' list
(provide 'all-the-haikus)
;;; all-the-haikus.el ends here
