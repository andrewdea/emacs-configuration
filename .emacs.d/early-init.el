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
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))
;; reset these after init


(progn ;; early customizations for nice appearance
  (push '(tool-bar-lines . 0)   default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (setq tool-bar-mode   nil
	scroll-bar-mode nil)

  ;; scratch buffer in fundamental mode
  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message "**Welcome to Emacs!**\n\n\n")

  (push '(left-fringe . 2)  default-frame-alist)
  (push '(right-fringe . 0) default-frame-alist)

  (setq inhibit-startup-screen t))

(provide 'early-init)
;;; early-init.el ends here
