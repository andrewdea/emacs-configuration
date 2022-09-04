;;; monicelli-mode.el --- sample major mode for editing Monicelli code. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2017, by you

;; Author: Andrew De Angelis ( bobodeangelis@gmail.com )
;; Version: 0.0.1
;; Created: 10 Jul 2022

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here

;;; Code:

(defvar monicelli-highlights
	     '(
	       ("\\(\\(bituma\\)\\|\\(#\\)\\).*$" . 'font-lock-comment-face)
	       ("\\( e velocità di esecuzione\\)\\|\\(o tarapia tapioco\\)\\|\\(o magari\\)\\|\\(che cos'è\\)\\|\\(Lei ha clacsonato\\)\\|\\(voglio\\)\\|\\(come\\( se\\)? fosse\\)" . 'font-lock-keyword-face)
	       ("\\(Necchi\\)\\|\\(Mascetti\\)\\|\\(Perozzi\\)\\|\\(Melandri\\)\\|\\(Sassaroli\\)" . 'font-lock-type-face)
	       ("\\(a posterdati\\)\\|\\(mi porga\\)" . 'font-lock-builtin-face)
	       ("\\(\\(\\(blinda\\)\\|\\(\\(b\\|p\\)rematurata\\)\\) la supercazzo\\(l\\|r\\)a\\)\\|\\(o scherziamo?\\)\\|\\(vaffanzum\\)" . 'font-lock-function-name-face)
	       ("voglio \\([^,]+\\), " . (1 'font-lock-constant-face))
	       ))

;; this would have been an easier way to do it
;; create the list for font-lock.
;; each category of keyword is given a particular face
;; (setq mylsl-font-lock-keywords
;;       (let* (
;;             ;; define several category of keywords
;;             (x-keywords '("break" "default" "do" "else" "for" "if" "return" "state" "while"))
;;             (x-types '("float" "integer" "key" "list" "rotation" "string" "vector"))
;;             (x-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
;;             (x-events '("at_rot_target" "at_target" "attach"))
;;             (x-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))

;;             ;; generate regex string for each category of keywords
;;             (x-keywords-regexp (regexp-opt x-keywords 'words))
;;             (x-types-regexp (regexp-opt x-types 'words))
;;             (x-constants-regexp (regexp-opt x-constants 'words))
;;             (x-events-regexp (regexp-opt x-events 'words))
;;             (x-functions-regexp (regexp-opt x-functions 'words)))

;;         `(
;;           (,x-types-regexp . 'font-lock-type-face)
;;           (,x-constants-regexp . 'font-lock-constant-face)
;;           (,x-events-regexp . 'font-lock-builtin-face)
;;           (,x-functions-regexp . 'font-lock-function-name-face)
;;           (,x-keywords-regexp . 'font-lock-keyword-face)
;;           ;; note: order above matters, because once colored, that part won't change.
;;           ;; in general, put longer words first
;;           )))

;;;###autoload
 (define-derived-mode monicelli-mode prog-mode "monicelli"
	 "majore mode for editing code in Monicelli language"
	 (setq font-lock-defaults '(monicelli-highlights)))

;; add the mode to the `features' list
(provide 'monicelli-mode)
;;; monicelli-mode.el ends here
