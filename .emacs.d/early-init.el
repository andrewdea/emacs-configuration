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
(setq user-emacs-directory
      (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path
             (expand-file-name "my-monokai-theme.el" user-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "cyberpunk-theme-20200601.1632"
                               user-emacs-directory))

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

;; (push '(fullscreen . maximized) default-frame-alist)

(setq custom-safe-themes
      '("a000d0fedd5e1c3b58e3a1c645c316ec2faa66300fc014c9ad0af1a4c1de839b" "024e125a165ef1f13cf858942b9e8f812f93f6078d8d84694a5c6f9675e94462" "e5dc4ab5d76a4a1571a1c3b6246c55b8625b0af74a1b9035ab997f7353aeffb2" "ebd933e1d834aa9525c6e64ad8f6021bbbaa25a48deacd0d3f480a7dd6216e3b" "7d52e76f3c9b107e7a57be437862b9d01b91a5ff7fca2524355603e3a2da227f" "19759a26a033dcb680aa11ee08677e3146ba547f1e8a83514a1671e0d36d626c" "99830ccf652abb947fd63a23210599483a14b1521291cd99aabae9c7ce047428" default))
(load-theme 'my-monokai)

;;;; optimization
;;;;; avoid garbage collection at startup:
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; avoid checking this list for any file that's opened
(defvar my-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; reset these after init:
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1
                  file-name-handler-alist my-file-name-handler-alist)
            (makunbound 'my-file-name-handler-alist)))

;;;;; don't load unneeded GUI components
(unless noninteractive

  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (advice-add #'display-startup-screen :override #'ignore)

  (advice-add #'scroll-bar-mode :override #'ignore)
  (advice-add #'tool-bar-mode :override #'ignore)
  (advice-add #'tool-bar-setup :override #'ignore)

  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

(setq load-prefer-newer noninteractive)

(provide 'early-init)
;;; early-init.el ends here
