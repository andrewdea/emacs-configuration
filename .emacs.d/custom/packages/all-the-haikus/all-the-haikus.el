;;; all-the-haikus.el --- functionalities to read from csv file of haikus -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Andrew De Angelis ( bobodeangelis@gmail.com )
;; Version: 0.0.1
;; Created: 8 Sep 2022

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; TBD


;;; Code:

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

;;;###autoload
(defun get-haiku-at-line (&optional arg)
  "Read a haiku from haiku-dataset-file.  Return as list of haiku data.
If ARG is a number or a marker, it reads the haiku at that line-number,
else (including if ARG is nil/not provided) reads from a random line-number"
  (with-temp-buffer
    (insert-file-contents haiku-dataset-file)

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

(defun format-haiku-just-text (haiku-data)
  "Given list of HAIKU-DATA, return newline-separated string of poetry."
  (string-join (butlast haiku-data 5) "\n"))

(defun read-poetry-at-line (&optional arg)
    "Read a haiku from haiku-dataset-file.  Return as string of poetry.
If ARG is a number or a marker, it reads the haiku at that line-number,
else (including if ARG is nil/not provided) reads from a random line-number"
  (format-haiku-just-text (get-haiku-at-line arg)))

(defun send-me-a-haiku (&optional arg)
  "Send a haiku to the message buffer.
If ARG provided, get haiku at that line, else pick a random one."
  (interactive "P")
  (message (read-poetry-at-line arg)))

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
  "Given a SYLLABLE-COUNT, find a random haiku with this format."
  (let ((to-find
	 (mapconcat (lambda (arg)
		      (if (stringp arg)
			  (concat "\"" arg "\"")
			(number-to-string arg)))
		    syllable-count ",")))
    (with-temp-buffer
      (insert-file-contents haiku-dataset-file)
      (let ((start-point
	     (random (point-max)))) ;random start so results are not always same
	(goto-char start-point)
        (if (search-forward to-find nil t) ; try from here to the end
	    (read-poetry-at-line (line-number-at-pos))
	  (progn (goto-char (point-min)) ; else from beginning to here
		 (if (search-forward to-find start-point t)
		     (read-poetry-at-line (line-number-at-pos))
		   (format "no poem with format %s found :'(" to-find))))))))

;; add the mode to the `features' list
(provide 'all-the-haikus)
;;; all-the-haikus.el ends here
