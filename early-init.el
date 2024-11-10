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
;;;; setup variables
(if (native-comp-available-p)
    (startup-redirect-eln-cache
     "eln-cache"))

;;;; emacs-lsp-booster
(setenv "LSP_USE_PLISTS" "true")

;;;; early GUI customizations for nice appearance
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq tool-bar-mode   nil
      scroll-bar-mode nil)

;; scratch buffer in fundamental mode
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message
      "*** Welcome to Emacs! ***\n-------------------------\n\n\n")

(push '(left-fringe . 2)  default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)

(push '(fullscreen . maximized) default-frame-alist)

(setq custom-theme-directory
      (expand-file-name "custom/themes" user-emacs-directory))

(if (or (member "-nw" command-line-args)
        (member "--no-window-system" command-line-args))
    (load-theme 'modus-vivendi)
  ;; monokai is a custom theme so we need to specify no-confirm
  ;; TODO we could also simply set `custom-safe-themes' to t here?
  (load-theme 'my-monokai 'no-confirm))

;; no round corners & no title-bar
(push '(undecorated . t) default-frame-alist)

;;;; optimization
;;;;; avoid garbage collection at startup:
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(defun my-gc-setup ()
  (setq gc-cons-threshold 16777216 ; 16mb
        gc-cons-percentage 0.1))

;; avoid checking this list for any file that's opened
(defvar my-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; reset these after init:
(add-hook 'after-init-hook
          (lambda ()
            (my-gc-setup)
            (setq file-name-handler-alist my-file-name-handler-alist)
            (makunbound 'my-file-name-handler-alist))
          100)

;;;;; don't load unneeded GUI components
(unless noninteractive

  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (advice-add #'display-startup-screen :override #'ignore)

  (advice-add #'scroll-bar-mode :override #'ignore)
  (advice-add #'tool-bar-mode :override #'ignore)
  (advice-add #'tool-bar-setup :override #'ignore)

  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

(provide 'early-init)
;;; early-init.el ends here
