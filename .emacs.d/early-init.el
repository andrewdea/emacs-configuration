;;; early-init.el --- pre-GUI initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Andrew De Angelis

;; Author: Andrew De Angelis <andrewdeangelis@Andrews-MacBook-Air.local>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; avoid garbage collection at startup
(setq gc-cons-threshold most-positive-fixnum) ; 2^61 bytes
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))
(setq gc-cons-percentage 0.6)
;; reset these after init

;; don't show startup screen
(setq inhibit-startup-screen t)

;; don't show tool bar
(tool-bar-mode -1)

;; scratch buffer in fundamental mode
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "**Welcome to Emacs!**\n\n\n")

(provide 'early-init)
;;; early-init.el ends here
