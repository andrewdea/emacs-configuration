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
(require 'cl-lib)

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

(defun comma-to-newline-except-within-quotes (c)
  "Return `char-to-string' of C or, if C is a comma, of newline.
Keeps track of whether it has seen a quote before the current char:
if we've seen a quote and no quote has closed it yet, don't convert to newline"
  (if (eq c ?\")
      (setq-local within-quotes ; toggle it
		  (not within-quotes)))
  (if (and (eq c ?,) ; char is comma
	   (not within-quotes))
      ?\n
    c))

(defmacro with-temp-haiku-buffer (&rest body)
  "Execute BODY inside of a temporary buffer with the haiku file.
If we're already in a temp buffer, assume it is the haiku file,
else open a new one and insert the file contents."
  (if (string-match-p (regexp-quote "*temp*") (buffer-name (current-buffer)))
      `(progn ,@body)
    (progn
      (push `(insert-file-contents-literally haiku-dataset-file) body)
      `(with-temp-buffer ,@body))))

(defun get-haiku-at-line (&optional arg)
  "Read a haiku from haiku-dataset-file.  Return as list of haiku data.
If ARG is a number or a marker, it reads the haiku at that line-number,
else (including if ARG is nil/not provided) reads from a random line-number"
  (with-temp-haiku-buffer
    (let ((line-number
	   (if (integer-or-marker-p arg)
	       arg
	     (+ 1 ; lowerbound of random: excludes the header
		(random
		 (count-lines (point-min) (point-max)))))))
      (forward-line (- line-number 1)))

    (let* ((line (thing-at-point 'line 'no-properties))
	   (parsed
	    (concat (mapcar 'comma-to-newline-except-within-quotes line))))
      (split-string parsed "\n"))))

(defun format-haiku-just-text (list-of-lines)
  "Given a LIST-OF-LINES, return newline-separated single string.
Remove any \" characters."
  (string-replace "\"" ""
		  (string-join list-of-lines "\n")))

(defun find-haiku-from-line-at-point ()
  "Read the line at point, open the haiku file and search for line."
  (interactive)
  (let ((line (string-trim (thing-at-point 'line 'no-properties))))
    (switch-to-buffer (find-file-other-window haiku-dataset-file))
    (search-forward line)
    (move-beginning-of-line 1)
    (set-mark-command nil)
    (move-end-of-line nil)))

(defun get-haiku-with-format (syllable-count)
  "Given a SYLLABLE-COUNT, find a random haiku with this format.
TODO: Describe what syllable-count looks like."
  (let ((to-find
	 (mapconcat (lambda (arg)
		      (if (stringp arg)
			  (concat "\"" arg "\"")
			(number-to-string arg)))
		    syllable-count ",")))
    (with-temp-haiku-buffer
      (let ((start-point ;random start so results are not always the same
	     (random (point-max))))
	(goto-char start-point)
        (if (or
	     (search-forward to-find nil t) ; try from here to the end
	     (progn (goto-char (point-min)) ; else from beginning to here
		    (search-forward to-find start-point t)))
	    (get-haiku-at-line (line-number-at-pos)) ; format found
	  (message "no poem with format (%s) found :(" to-find))))))

(defun search-regexp-in-haiku-file (rgx)
  "From random point in haiku file, search RGX, return the first matching line."
  (with-temp-haiku-buffer
    (let ((start-point ;random start so results are not always the same
	   (random (point-max))))
      (goto-char start-point)
      (if (or
	   (search-forward-regexp rgx nil t) ; try from here to the end
	   (progn (goto-char (point-min)) ; else from beginning to here
		  (search-forward-regexp rgx start-point t)))
	  (get-haiku-at-line (line-number-at-pos))
	nil))))

(defun generate-poem-with-format (syllable-count)
  "Given a SYLLABLE-COUNT, create a new poem with matching lines."
  (message "searching for this syllable count: %s"
	   syllable-count)
  (with-temp-haiku-buffer
    (let* ((first-regexp (format "[[:alpha:]],%S," (car syllable-count)))
	   (second-regexp (format "[[:digit:]]\"?,%S,"(nth 1 syllable-count)))
	   (third-regexp (format "[[:digit:]]\"?,%S$"(nth 2 syllable-count)))
	   (to-find-list (list first-regexp second-regexp third-regexp))
	   (found-list
	    (mapcar #'search-regexp-in-haiku-file to-find-list))
	   (just-the-right-lines
	    (cl-mapcar #'nth (number-sequence 0 2) found-list)))
      (format-haiku-just-text just-the-right-lines))))

;;;###autoload
(defun write-me-a-haiku (&optional syllable-count)
  "Writes a new poem.
If SYLLABLE-COUNT provided, generate a poem with that format.
Else, generate a poem by getting three random lines.
The returned poem starts with the string *self-generated*\n.
When called interactively, the poem is displayed in the minibuffer."
  (interactive)
  (if (and syllable-count (listp syllable-count))
      (generate-poem-with-format syllable-count)
    (let* ((found-list
	    (mapcar 'funcall (make-list 3 'get-haiku-at-line)))
	   (just-the-right-lines
	    (cl-mapcar #'nth (number-sequence 0 2) found-list))
	   (haiku (format-haiku-just-text just-the-right-lines)))
      (concat "*self-generated*\n"
	      (if (called-interactively-p 'any)
		  (message haiku) ; display in minibuffer
		haiku)))))

;;;###autoload
(defun find-me-a-haiku (&optional arg)
  "Read a haiku from haiku-dataset-file.  Return as string of poetry.
If ARG is a number or a marker, read the haiku at that line-number.
If arg is a non-nil list, look for a haiku with this syllable-count
(see get-haiku-with-format).
else (including if ARG is nil) read haiku from a random line-number.
When called interactively, the poem is displayed in the minibuffer."
  (interactive "P")
  (let* ((haiku-line
	 (if (and arg (listp arg))
	     (get-haiku-with-format arg)
	   (get-haiku-at-line arg)))
	 (haiku
	  (format-haiku-just-text (butlast haiku-line 5))))
    (if (called-interactively-p 'any)
	(message haiku)
      haiku)))

;; add the mode to the `features' list
(provide 'all-the-haikus)
;;; all-the-haikus.el ends here
