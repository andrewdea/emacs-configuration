;;; monicelli-mode.el --- sample major mode for editing Monicelli code. -*- coding: utf-8; lexical-binding: t; -*-

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
;; syntax highlighting 
(setq monicelli-highlights
      '(
	("\\(\\(bituma\\)\\|\\(#\\)\\).*$" . 'font-lock-comment-face)
	("\\( e velocità di esecuzione\\)\\|\\(o tarapia tapioco\\)\\|\\(o magari\\)\\|\\(che cos'è\\)\\|\\(Lei ha clacsonato\\)\\|\\(voglio\\)\\|\\(come\\( se\\)? fosse\\)\\|\\(con\\)" . 'font-lock-keyword-face)
	("\\(Necchi\\)\\|\\(Mascetti\\)\\|\\(Perozzi\\)\\|\\(Melandri\\)\\|\\(Sassaroli\\)" . 'font-lock-type-face)
	("\\(a posterdati\\)\\|\\(mi porga\\)" . 'font-lock-builtin-face)
	("\\(\\(\\(blinda\\)\\|\\(\\(b\\|p\\)rematurata\\)\\) la supercazzo\\(l\\|r\\)a\\)\\|\\(o scherziamo?\\)\\|\\(vaffanzum\\)" . 'font-lock-function-name-face)
	("voglio \\([^,]+\\), " . (1 'font-lock-constant-face))
	))

;;;###autoload
(define-derived-mode monicelli-mode prog-mode "monicelli"
  "majore mode for editing code in Monicelli language"
  (setq font-lock-defaults '(monicelli-highlights)))

;; add the mode to the `features' list
(provide 'monicelli-mode)

;;; 
