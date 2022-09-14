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
  "Read and write haikus from csv file."
  :group 'convenience) ; leisure/art/something like that?

(defcustom haiku-dataset-file
  "~/.emacs.d/custom/datasets/haiku-dataset.csv"
  "Comma-separated file containing our list of haikus."
  :group 'all-the-haikus
  :type 'file)

(defcustom haiku-favorites-file
  "~/.emacs.d/custom/datasets/haiku-favorites.csv"
  "Comma-separated file containing our favorite haikus."
  :group 'all-the-haikus
  :type 'file)

(defvar haiku-file-header
  "0,1,2,source,0_syllables,1_syllables,2_syllables\n"
  "String used as header for our csv haiku files.")

(defvar within-quotes nil
  "Boolean to keep track of where we are while iterating a string.")

(defvar generated-poem-marker "*generated*"
  "String that will be placed at the beginning of a generated poem.")

(defmacro with-haiku-temp-buffer (&rest body)
  "Execute BODY inside of a temporary buffer with the haiku file.
If we're already in the haiku temp buffer, just execute body;
else, open a new buffer and insert the file contents."
  (if (boundp 'temp-haiku-buffer)
      `(progn ,@body)
    (progn
      `(with-temp-buffer
	 (insert-file-contents-literally haiku-dataset-file)
	 (setq-local temp-haiku-buffer (current-buffer))
	 ,@body))))

(defun comma-to-newline-except-within-quotes (char)
  "Convert a CHAR to newline if it doesn't appear within quotes in a string.
Use `within-quotes' to keep track of whether
the string contains a quote before the current char:
if a quote was read and no quote has closed it yet, don't convert to newline"
  (if (eq char ?\")
      (setq-local within-quotes ; toggle it
		  (not within-quotes)))
  (if (and (eq char ?,) ; char is comma
	   (not within-quotes))
      ?\n
    char))

(defun get-haiku-line (line-number)
  "From the `haiku-dataset-file', read line at LINE-NUMBER.
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
Add an empty line to end the poem.
Maybe: Remove any \" characters."
  ;; (string-replace "\"" ""
  (concat
   (string-join list-of-strings "\n")
   "\n"))

(defun show-haiku-from-line-at-point ()
  "Read the line at point, open `haiku-dataset-file', highlight matching line."
  (interactive)
  (let ((line (string-trim (thing-at-point 'line 'no-properties))))
    (switch-to-buffer (find-file-other-window haiku-dataset-file))
    (goto-char (point-min))
    (search-forward line)
    (move-beginning-of-line 1)
    (set-mark-command nil)
    (move-end-of-line nil)))

(defun return-haiku-from-line (&optional arg)
  "Return the line from `haiku-dataset-file' matching ARG.
If ARG is not provided, use the line at point."
  (interactive)
  (let ((line
	 (string-trim (or
		       arg
		       (thing-at-point 'line 'no-properties)))))
    (with-haiku-temp-buffer
     (goto-char (point-min))
     (search-forward line)
     (thing-at-point 'line 'no-properties))))

(defun haiku-get-syllable-count (string line-number)
  "Given a STRING and a LINE-NUMBER, determine STRINGS's syllable count.
This is done by looking for a poem in `haiku-dataset-file'
that contains STRING at the position given by LINE-NUMBER,
and then returning the syllable-count of that same position."
  (let* ((to-find
	  ;; use a regexp depending on what position to find the line
	  (cond ((eq line-number 0) (format "^%s,.*,.*,.*,.*,.*,.*$" string))
		((eq line-number 1) (format "^.*,%s,.*,.*,.*,.*,.*$" string))
		((eq line-number 2) (format "^.*,.*,%s,.*,.*,.*,.*$" string))))
	 (found
	  (with-haiku-temp-buffer
	   (get-random-matching-line to-find)))) ; TODO: there's no need for this to be random
    ;; the last 4 elements are the syllable counts (and nil termination)
    (nth line-number (last found 4))))

(defun haiku-generate-data (poem)
  "Add syllable-count and source to the generated POEM.
To determine their syllable count,
search for each of POEM's lines in `haiku-dataset-file'.
Add the `generated-poem-marker' as the source."
  (let* ((parsed (split-string poem "\n"))
	 (syllable-count
	  (cl-mapcar #'haiku-get-syllable-count parsed (number-sequence 0 2))))
    (string-join
     (append parsed (list generated-poem-marker) syllable-count) ",")))

(defun generated-poem-p (&optional arg)
  "Given a string ARG, determine if it's a generated poem.
If it is, return the three lines of the poem and its data.
This is done by checking if the string contains `generated-poem-marker'
If called interactively or string is not provided,
check the text around the point."
  (interactive)
  (let ((to-find (concat "^" (regexp-quote generated-poem-marker) "\n")))
    (if arg
	;; if ARG is provided & matches, replace the marker and return the poem
	(if (string-match-p to-find arg)
	    (replace-regexp-in-string to-find "" arg))
      ;; if ARG is not provided, check the text around point
      (progn
	(move-beginning-of-line 1)
	(if (search-forward-regexp "^$" nil t) ; go to the end of the poem
	    (let* ((pos (point))
		   (poetry-lines
		    (forward-line -4) ; go back 4 lines to find the marker
		    (if (string-match-p to-find
					(thing-at-point 'line 'no-properties))
			;; if marker found, ignore marker line
			(progn (forward-line 1)
			       ;; return last 3 lines
			       (buffer-substring-no-properties (point) pos)))))
	      ;; return the poem along with its data
	      (haiku-generate-data poetry-lines)))))))

(defun haiku-save-to-favorites (&optional arg)
  "Find ARG's data and save it to `haiku-favorites-file'.
When called interactively or ARG not provided, check the text at point.
Parse `haiku-dataset-file' to find ARG's data (source and syllable-count)."
  (interactive)
  (let ((haiku-with-data
	 (or
	  ;; if the poem is generated, we'll have to search for each line
	  (generated-poem-p arg)
	  ;; else we can search a single line
	  (return-haiku-from-line
	   ;; the line to search: if provided, arg, else the line at point
	   (or
	    arg
	    (string-trim (thing-at-point 'line 'no-properties)))))))
    (with-temp-buffer
      (if (file-exists-p haiku-favorites-file)
	  (insert-file-contents-literally haiku-favorites-file)
	(insert haiku-file-header))
      (set-visited-file-name haiku-favorites-file)
      (goto-char (point-max))
      (if (not (eq ?\n (char-before)))
	  (insert ?\n))
      (insert haiku-with-data)
      (insert ?\n)
      (save-buffer))))

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
	   (progn ; else send message and return nothing
	     (message "no poem with format (%s) found :(" to-find)
	     nil))))))

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
The file used to copy lines from is `haiku-dataset-file'.
The returned poem has an additional first line with the `generated-poem-marker'.
When called interactively, the poem is displayed in the minibuffer."
  (interactive)
  (concat generated-poem-marker "\n"
	  (if (and syllable-count (listp syllable-count))
	      (generate-poem-with-format syllable-count)
	    (let* ((found-list ; three random lines from file
		    (mapcar #'funcall (make-list 3 #'get-random-haiku-line)))
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
  "Read a haiku from `haiku-dataset-file'.  Return as string of poetry.
If ARG is a number or a marker, read the haiku at that line-number.
If ARG is a non-nil list, look for a haiku with this syllable-count
\(see `get-haiku-with-format').
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
	  (format-haiku-just-text (take 3 haiku-line))))
    (if (called-interactively-p 'any)
	(message haiku) ; display in minibuffer
      haiku)))

;; add package to the `features' list
(provide 'all-the-haikus)
;;; all-the-haikus.el ends here
