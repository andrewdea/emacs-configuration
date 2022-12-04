;;; all-the-haikus.el --- write and find poems with emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Andrew De Angelis

;; Author: Andrew De Angelis <bobodeangelis@gmail.com>
;; Maintainer: Andrew De Angelis <bobodeangelis@gmail.com>
;; URL: https://github.com/andyjda/all-the-haikus
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "1.0"))
;; Keywords: games, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3 as published
;; by the Free Software Foundation, version 3 of the License

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides two main functionalities, `write-me-a-haiku' and
;; `find-me-a-haiku'.
;; `find-me-a-haiku' returns a poem from a file.  It can be a random poem, or
;; - a poem from a specific line in the file
;; - a poem with specific words
;; - a poem whose lines have a specific syllable count
;; `write-me-a-haiku' finds random lines from the file and combines them to
;; generate a new poem.  Currently, the only constraint it can accept is a
;; specified syllable count.

;; These can be used to (for example) configure your Emacs to show you a poem
;; on startup.

;; Some additional functionalities are `haiku-save-to-favorites', which
;; can save a specified poem to the `haiku-favorites-file',
;; and `show-haiku-from-line-at-point', which opens the `haiku-dataset-file'
;; and shows the specified poem.

;; The package depends on the file `haiku-dataset-file'.
;; The expected format of the file is:
;; - first file-line is a header, not a poem, and will be ignored by our program
;; - each subsequent file-line is a three-line poem
;; - the lines in the poem are separated by commas
;; - in addition to those three lines, the file-line contains 4 columns:
;;      - the source of the poem
;;      - the syllable-count of each line in the poem
;; - if a column contains a comma, the column is encased in quotes \"
;; - syllable counts can have multiple numbers assigned for the same line:
;;      this means that the line has differing number of syllables
;;      depending on pronounciation
;; A dataset like this one should be fine:
;; https://github.com/docmarionum1/haikurnn/blob/master/input/poems/haikus.csv

;; Note about variable names in the code:
;; to avoid confusion, 'file-line' refers to a line in the dataset file,
;; while 'line' refers to a line of a poem

;;; Code:
(eval-when-compile (require 'cl-lib))

(defgroup all-the-haikus nil
  "Read and write haikus from csv file."
  :group 'games)

(defcustom haiku-dataset-file
  "~/.emacs.d/custom/datasets/haiku-dataset.csv"
  "Comma-separated file containing our list of haikus.
See commentary in `all-the-haikus'.el for a detailed explanation
of its expected format."
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

;;;; Formatting haikus
(defun haiku-break-into-lines (string within-quotes)
  "Replace every comma in STRING with a newline, unless it's within quotes.
Use WITHIN-QUOTES to keep track
of whether the string contains a quote before the current character;
apply recursively to each charater in the string"
  (if (not (string-empty-p string))
      (progn
	(let* ((char (aref string 0))
	       (within-quotes
		(if (eq char ?\")
		    (not within-quotes) ; toggle it
		  within-quotes)) ; keep as is
	       (res
		(if (and (eq char ?,) (not within-quotes))
		    ?\n ; 'convert' char to newline
		  char)))
	  (concat (string res)
		  (haiku-break-into-lines
		   (substring string 1) within-quotes))))))

(defun format-haiku-just-text (list-of-strings)
  "Given a LIST-OF-STRINGS, return a newline-separated single string.
Remove all \" characters, and add an empty line to end the poem."
  (string-replace "\"" ""
		  (concat
		   (string-join list-of-strings "\n")
		   "\n")))

(defun format-haiku-line (arg)
  "Trim the string ARG.
If ARG contains a comma, surround ARG with quotes."
  (let ((trimmed (string-trim arg)))
    (if (string-match-p (regexp-quote ",") trimmed)
	(concat "\"" trimmed "\"")
      trimmed)))

;;;; Fetching haikus
;;;;; get/create haikus from query
(defun get-haiku-file-line (line-number)
  "From the `haiku-dataset-file', read line at LINE-NUMBER.
Return it as a list of strings"
  (with-haiku-temp-buffer
   (goto-char (point-min))
   (forward-line (- line-number 1))
   (let* ((line (thing-at-point 'line 'no-properties))
	  (parsed
	   (haiku-break-into-lines line nil)))
     (split-string parsed "\n"))))

(defun get-random-haiku-file-line ()
  "Read a random line from the `haiku-dataset-file'.
Uses `get-haiku-file-line'"
  (with-haiku-temp-buffer
   (get-haiku-file-line
    (+ 1 ; lowerbound of random: excludes the header
       (random
	(count-lines (point-min) (point-max)))))))

(defun get-random-matching-file-line (to-find)
  "Go to random point in file, search forward for regexp TO-FIND.
If no results are found from start-point to end,
search from beginning to start-point."
  (message "looking for: %s" to-find)
  (let ((start-point ;random start
	 (+ (length haiku-file-header) ; lowerbound: excludes the header
	    (random
	     (point-max)))))
    (goto-char start-point)
    (if (or
	 (search-forward-regexp to-find nil t) ; try from here to the end
	 (progn (goto-char (+ (length haiku-file-header)
			      (point-min))) ; else from beginning to here
		(search-forward-regexp to-find start-point t)))
	(get-haiku-file-line (line-number-at-pos))
      (progn (message "%s not found." to-find) nil))))

(defun haiku-word-list-to-regexp (arg)
  "Convert a list of strings ARG into a single regexp string.
The resulting regexp is equivalent to a haiku-line that contains
the first word in the first column, the second word in the second column, etc."
  (if arg
      (concat ".*" (car arg) ".*,"
	      (haiku-word-list-to-regexp (cdr arg)))))

(defun get-haiku-with-format (&optional arg syllable-count)
  "Find a random haiku with the specified SYLLABLE-COUNT and containing ARG.
ARG can be a string or a regexp to look for inside the poem, or a list of words,
which must appear in the specified order in the poem.
TODO: refactor this to be more elegant and cleaner."
  (let* ((words-to-find
	  (cond
	   ((null arg) ".*,")
	   ((stringp arg)
	    (concat ".*" arg ".*"))
	   ((listp arg)
	    (concat "^" ; beginning of line
		    (haiku-word-list-to-regexp arg)
		    ".*,")) ; the 'source' column
	   (t
	    (message "arg is the wrong type"))))
	 (format-to-find
	  (cond
	   ((null syllable-count) ".*,.*,.*$") ; any format
	   ((listp syllable-count)
	    (mapconcat (lambda (arg)
			 (if (stringp arg)
			     (concat "\"" arg "\"")
			   (number-to-string arg)))
		       syllable-count ",")) ; construct the format from list
	   ((stringp syllable-count) syllable-count) ; regex provided by user
	   (t
	    (message "syllable-count is the wrong type"))))
	 (to-find (concat words-to-find format-to-find)))
    (with-haiku-temp-buffer
     (let ((found
	    (get-random-matching-file-line to-find)))
       (or found ; if found, return it
	   (progn ; else send message and return nothing
	     (message "no poem with format %s found :(" to-find)
	     nil))))))

;;;;; show/return haikus from data
(defun show-haiku-from-line-at-point ()
  "Read the line at point, open `haiku-dataset-file', highlight matching line."
  (interactive)
  (let ((line (format-haiku-line (thing-at-point 'line 'no-properties))))
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
	 (format-haiku-line (or
			     arg
			     (thing-at-point 'line 'no-properties)))))
    (message "line in return haiku from line: %s" line)
    (with-haiku-temp-buffer
     (goto-char (point-min))
     (if (search-forward line nil t)
	 (thing-at-point 'line 'no-properties)
       (progn (message "%s not found :(" line) nil)))))

;;;; Saving haikus
(defun haiku-save-to-favorites (&optional arg)
  "Find ARG's data and save it to `haiku-favorites-file'.
When called interactively or ARG not provided, check the text at point.
Parse `haiku-dataset-file' to find ARG's data (source and syllable-count)."
  (interactive)
  (let* ((line
	  (format-haiku-line (or
			      arg
			      (thing-at-point 'line 'no-properties))))
	 (haiku-with-data
	  (or (generated-poem-p arg)
	      (return-haiku-from-line line))))
    (message "haiku-with-data: %s" haiku-with-data)
    (with-temp-file haiku-favorites-file
      (if (file-exists-p haiku-favorites-file)
	  (insert-file-contents-literally haiku-favorites-file)
	(insert haiku-file-header))
      (goto-char (point-max))
      ;; unless the file already ends in newline, insert one
      (if (not (eq ?\n (char-before)))
	  (insert ?\n))
      (insert haiku-with-data)
      ;; unless the last character in the inserted line is a newline, insert one
      (if (not (eq ?\n (char-before)))
	  (insert ?\n)))
    (message "saved \n\"%s\"\n to `haiku-favorites-file'" haiku-with-data)))

;;;;; Finding haikus' information
(defun haiku-get-syllable-count (string line-number)
  "Given a STRING and a LINE-NUMBER, determine STRINGS's syllable count.
This is done by looking for a poem in `haiku-dataset-file'
that contains STRING at the position given by LINE-NUMBER,
and then returning the syllable-count of that same position."
  (let* ((to-find
	  ;; use a regexp depending on what position to find the line
	  (pcase line-number
	    (`0 (format "^%s,.*,.*,.*,.*,.*,.*$" string))
	    (`1 (format "^.*,%s,.*,.*,.*,.*,.*$" string))
	    (`2 (format "^.*,.*,%s,.*,.*,.*,.*$" string))))
	 (found
	  (with-haiku-temp-buffer
	   (if (search-forward-regexp to-find nil t)
	       (get-haiku-file-line (line-number-at-pos))
	     (progn (message "%s not found." to-find) nil)))))
    ;; the last 4 elements are the syllable counts (and nil termination)
    (nth line-number (last found 4))))

(defun haiku-generate-data (poem)
  "Add syllable-count and source to the generated POEM.
To determine their syllable count,
search for each of POEM's lines in `haiku-dataset-file'.
Add the `generated-poem-marker' as the source."
  (message "in haiku-generate-data,  poem: %s" poem)
  (let* ((lines (split-string poem "\n"))
	 (non-empty-lines
	  (seq-filter
	   (lambda (arg)
	     (not (string-empty-p arg)))
	   lines))
	 (formatted (mapcar #'format-haiku-line non-empty-lines))
	 (syllable-count
	  (cl-mapcar #'haiku-get-syllable-count formatted '(0 1 2))))
    (string-join
     (append formatted (list generated-poem-marker) syllable-count) ",")))

(defun generated-poem-p (&optional arg)
  "Given a string ARG, determine if it's a generated poem.
If it is, return the three lines of the poem and its data.
This is done by checking if the string contains `generated-poem-marker'
If called interactively or string is not provided,
check the text around the point."
  (interactive)
  (let ((to-find (concat "^\"?" (regexp-quote generated-poem-marker) "\n")))
    (if arg
	;; if ARG is provided & matches, replace the marker and return the poem
	(if (string-match-p to-find arg)
	    (replace-regexp-in-string to-find "" arg))
      ;; if ARG is not provided, check the text around point
      (progn
	(move-beginning-of-line 1)
	(if (search-forward-regexp "^\"?$" nil t) ; go to the end of the poem
	    (let* ((pos (- (point) 1)) ; subtract 1 to remove the final \" char
		   (poetry-lines
		    (progn
		      (forward-line -4) ; go back 4 lines to find the marker
		      (if (string-match-p to-find
					  (thing-at-point 'line 'no-properties))
			  ;; if marker found, ignore marker line
			  (progn (forward-line 1)
                                        ;(message "returning the last 3 lines")
				 ;; return last 3 lines
				 (buffer-substring-no-properties
				  (point) pos))))))
	      ;; return the poem along with its data
	      (message "poetry lines: %s" poetry-lines)
	      (if poetry-lines
		  (haiku-generate-data poetry-lines))))))))


;;;; Creating haikus
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
	   (mapcar #'get-random-matching-file-line to-find-list))
	  (just-the-right-lines ; three strings that will make our poem
	   ;; preserve the order of the lines:
	   ;; for the first line in our new poem,
	   ;; take a line that was originally a first line in its poem
	   ;; apply the same logic for second line, etc
	   ;; this makes the sentences more coherent/meaningful
	   (cl-mapcar #'nth '(0 1 2) found-list)))
     (format-haiku-just-text just-the-right-lines))))

;;;; Interface
;;;###autoload
(defun write-me-a-haiku (&optional syllable-count)
  "Writes a new poem.
SYLLABLE-COUNT is a list of three integers specifying the desired
syllable-count of each line in the poem.
If SYLLABLE-COUNT provided, generate a poem with that format.
Else, generate a poem by getting three random lines.
The returned poem has an additional first line with the `generated-poem-marker'.
When called interactively, the poem is displayed in the minibuffer."
  (interactive)
  (concat generated-poem-marker "\n"
	  (if (and syllable-count (listp syllable-count))
	      (generate-poem-with-format syllable-count)
	    (let* ((found-list ; three random lines from file
		    (mapcar #'funcall (make-list 3 #'get-random-haiku-file-line)))
		   (just-the-right-lines ; three strings that will make our poem
		    ;; preserve the order of the lines:
		    ;; for the first line in our new poem,
		    ;; take a line that was originally a first line
		    ;; apply the same logic for second line, etc
		    ;; this makes the sentences more coherent/meaningful
		    (cl-mapcar #'nth '(0 1 2) found-list))
		   (haiku (format-haiku-just-text just-the-right-lines)))
	      (if (called-interactively-p 'any)
		  (message haiku) ; display in minibuffer
		haiku)))))

;;;###autoload
(defun find-me-a-haiku (&optional arg syllable-count)
  "Read a haiku from `haiku-dataset-file'.  Return as string of poetry.
SYLLABLE-COUNT is a list of three integers specifying the desired
syllable-count of each line in the poem.
If ARG is a number or a marker, read the haiku at that line-number.
If ARG is a non-nil list, look for a haiku with this syllable-count
\(see `get-haiku-with-format').
else (including if ARG is nil) read haiku from a random line-number.
When called interactively, the poem is displayed in the minibuffer."
  (interactive "P")
  (let* ((haiku-line
	  (cond ((integer-or-marker-p arg)
		 (get-haiku-file-line arg))
		((or arg syllable-count)
		 (get-haiku-with-format arg syllable-count))
		(t
		 (get-random-haiku-file-line))))
	 (haiku
	  (format-haiku-just-text (take 3 haiku-line))))
    (if (called-interactively-p 'any)
	(message haiku) ; display in minibuffer
      haiku)))

;; add package to the `features' list
(provide 'all-the-haikus)
;;; all-the-haikus.el ends here
