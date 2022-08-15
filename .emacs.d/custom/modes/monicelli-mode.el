;;; monicelli-mode.el --- sample major mode for editing Monicelli code. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2017, by you

;; Author: Andrew De Angelis ( bobodeangelis@gmail.com )
;; Version: 0.0.1
;; Created: 10 Jul 2022

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; Simple mode providing syntax highlight for the Monicelli programming language
;; see https://github.com/esseks/monicelli for more info on this great language

;; Add this to your init file to have monicelli-mode available:
;; (add-to-list 'load-path "<path to the folder where you have this file>")
;; 			   ;eg ~/.emacs.d/custom/modes
;; ;; loading monicelli mode
;; (autoload 'monicelli-mode "<path to this file>")
;; 			     ; eg ~/.emacs.d/custom/modes/monicelli-mode.el
;;
;; ;; opening monicelli files with monicelli-mode
;; (add-to-list 'auto-mode-alist '("\\.mc\\'" . monicelli-mode))


;;; Code:
(setq comment-start "#")
;; earlier version of syntax highlights
(setq monicelli-highlights
	     '(
	       ("\\(\\(bituma\\)\\|\\(#\\)\\).*$" . 'font-lock-comment-face)
	       ("\\( e velocità di esecuzione\\)\\|\\(o tarapia tapioco\\)\\|\\(o magari\\)\\|\\(che cos'è\\)\\|\\(Lei ha clacsonato\\)\\|\\(voglio\\)\\|\\(come\\( se\\)? fosse\\)" . 'font-lock-keyword-face)
	       ("\\(Necchi\\)\\|\\(Mascetti\\)\\|\\(Perozzi\\)\\|\\(Melandri\\)\\|\\(Sassaroli\\)" . 'font-lock-type-face)
	       ("\\(a posterdati\\)\\|\\(mi porga\\)" . 'font-lock-builtin-face)
	       ("\\(\\(\\(blinda\\)\\|\\(\\(b\\|p\\)rematurata\\)\\) la supercazzo\\(l\\|r\\)a\\)\\|\\(o scherziamo?\\)\\|\\(vaffanzum\\)" . 'font-lock-function-name-face)
	       ("voglio \\([^,]+\\), " . (1 'font-lock-constant-face))
	       ))

;; trying out better approach?
;; this would have been an easier way to do it
;; create the list for font-lock.
;; each category of keyword is given a particular face

;; define some helper functions for regex construction
;; (defun rgx-group (arg)
;;   (concat "\\(" arg "\\)"))

;; (defun rgx-or (args)
;; 	 (let ((first (car args))
;; 	       (rest (cdr args)))
;; 	   (concat (if (> (length first) 1)
;; 		     (rgx-group first) 
;; 		       first)
;; 		   (when rest
;; 		     (concat "\\|" (rgx-or rest))))))

;; (defun rgx-create (args &optional prefix postfix)
;;   (concat prefix (rgx-or args) postfix))

;; (setq rgx-til-newline ".*$")

;; (setq rgx-? "?")

;; (defun rgx-opt (args)
;;   (concat (rgx-or args) "?"))


;; (setq monicelli-font-lock-keywords
;;       (let* (
;;              ;; define several category of keywords
;; 	     ;; (x-comments '("bituma" "#"))
;;              (x-keywords '("e velocità di esecuzione" "o tarapia tapioco"
;; 			   "o magari" "che cos'è" "Lei ha clacsonato" "voglio"
;; 			   "come se fosse" "come fosse"))
;;              (x-types '("Necchi" "Mascetti" "Perozzi" "Melandri" "Sassaroli"))
;; 	     (x-built-ins '("a posterdati" "mi porga"))
;;              (x-functions '("blinda" "prematurata la supercazzola" "vaffanzum"))

;;              ;; generate regex string for each category of keywords
;; 	     (until-newline-regexp ".*$")
;; 	     (or-regexp "\\|")
;; 	     ;; (x-comments-regexp (concat (regexp-opt x-comments 'words)
;; 	     ;; 				until-newline-regexp))
;; 	     ;; (x-comments-regexp "\\(#\\|bituma\\).*$")
;; 	     (x-comments-regexp (concat (rgx-group (rgx-or '("bituma" "#"))) rgx-til-newline))
;;              (x-keywords-regexp (regexp-opt x-keywords 'words))
;;              (x-types-regexp (regexp-opt x-types 'words))
;;              (x-built-ins-regexp (regexp-opt x-built-ins 'words))
;;              ;; (x-functions-regexp (regexp-opt x-functions 'words))
;; 	     (x-functions-regexp (rgx-group (rgx-or `(,(concat (rgx-create `("blinda" ,(concat (rgx-group (rgx-or '("b" "p"))) "rematurata")))
;; 	       (concat " la supercazzo" (rgx-group (rgx-or '("l" "r"))) "a o scherziamo"))
;; 		"vaffanzum"))))
;; 	     )

;;         `(
;;           (,x-types-regexp . 'font-lock-type-face)
;;           (,x-built-ins-regexp . 'font-lock-builtin-face)
;;           (,x-functions-regexp . 'font-lock-function-name-face)
;; 	  (,x-comments-regexp . 'font-lock-comment-face)
;;           (,x-keywords-regexp . 'font-lock-keyword-face)
;;           ;; note: order above matters, because once colored, that part won't change.
;;           ;; in general, put longer words first
;;           )))

;;;###autoload
 (define-derived-mode monicelli-mode prog-mode "monicelli"
	 "majore mode for editing code in Monicelli language"
	 (setq font-lock-defaults '(monicelli-highlights))
	 ;; (setq font-lock-defaults '(monicelli-font-lock-keywords))
	 )

;; add the mode to the `features' list
(provide 'monicelli-mode)

;;; 
