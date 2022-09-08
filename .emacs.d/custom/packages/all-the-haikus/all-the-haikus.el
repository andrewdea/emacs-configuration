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
  "Comma-separated file containing our list of haikus"
  :group 'all-the-haikus
  :type 'file)

;;;###autoload
(defun read-haiku-at-line (&optional arg)
  "Read a haiku from haiku-dataset-file.
If ARG is a number or a marker, it reads the haiku at that line-number,
else (including if ARG is nil/not provided) reads from a random line-number"
  (with-temp-buffer
    (insert-file-contents haiku-dataset-file)

    (let ((line-number
	   (if (integer-or-marker-p arg)
	       arg
	     (random (count-lines (point-min) (point-max))))))
      (forward-line line-number))

    (let ((line (thing-at-point 'line 'no-properties)))
      (string-join (butlast (split-string line ",") 4) "\n"))))

(defun find-haiku-from-line-at-point ()
  "Read the line at point, open the haiku file and search for line."
  (interactive)
  (let ((line (string-trim (thing-at-point 'line 'no-properties))))
    (switch-to-buffer (find-file-other-window haiku-dataset-file))
    (search-forward line)
    (move-beginning-of-line 1)
    (set-mark-command nil)
    (move-end-of-line nil)))

;; add the mode to the `features' list
(provide 'all-the-haikus)
;;; all-the-haikus.el ends here
