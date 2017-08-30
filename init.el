;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2017 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)


(menu-bar-mode -1)
(tool-bar-mode -1)

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Prelude is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.4")
  (error "Prelude requires at least GNU Emacs 24.4, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-dir)
  "This files contains a list of modules that will be loaded by Prelude.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#\.].*el$")))

(message "Loading Prelude's core...")

;; the core stuff
(require 'prelude-packages)
(require 'prelude-global-keybindings)


(load-theme 'zonokai-blue)
;; Do not load anything other than the prelude packages and global keybindings

; (require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
; (require 'prelude-ui)
; (require 'prelude-core)
; (require 'prelude-mode)
; (require 'prelude-editor)
; 

; ;; OSX specific settings
; (when (eq system-type 'darwin)
;   (require 'prelude-osx))

; (message "Loading Prelude's modules...")

; ;; the modules
; (if (file-exists-p prelude-modules-file)
;     (load prelude-modules-file)
;   (message "Missing modules file %s" prelude-modules-file)
;   (message "You can get started by copying the bundled example file from sample/prelude-modules.el"))

; ;; config changes made through the customize UI will be store here
; (setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

; ;; load the personal settings (this includes `custom-file')
; (when (file-exists-p prelude-personal-dir)
;   (message "Loading personal configuration files in %s..." prelude-personal-dir)
;   (mapc 'load (directory-files prelude-personal-dir 't "^[^#\.].*el$")))

; (message "Prelude is ready to do thy bidding, Master %s!" current-user)

; (prelude-eval-after-init
;  ;; greet the use with some useful tip
;  (run-at-time 5 nil 'prelude-tip-of-the-day))

;; Map keys to do helm
(global-set-key (kbd "M-x") 'helm-M-x)

;; Utility function to open default org tasks file
(defun open-org-tasks () (find-file "~/org/Tasks.org"))
(global-set-key (kbd "M-q") (open-org-tasks))

;; Sublime like keybinding for opening list of file
(global-set-key (kbd "M-p") 'projectile-find-file) 

;; Kebindings for projectile and neotree
(global-unset-key "\M-m")
(global-set-key (kbd "M-m p p") 'projectile-switch-project)
(global-set-key (kbd "M-m p t") 'neotree-projectile-action)

;; Keybinding for ibuffer
(global-set-key (kbd "M-m b b") 'ibuffer-list-buffers)
(global-set-key (kbd "M-m b s") (lambda () (switch-to-buffer "*scratch*")))

;; Keybindings for swiper
(global-set-key (kbd "C-s") 'swiper)

;; setup js-mode
(setq js2-basic-offset 2)

;; Setup web mode
(add-hook 'web-mode-hook
          (lambda ()
            (if (equal web-mode-content-type "javascript")
                (web-mode-set-content-type "jsx"))))

(load-library "org-protocol")
(setq org-default-notes-file "~/org/Tasks.org")

(setq org-ellipsis "⤵")
;;(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/"))
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
  (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
  (quote (("TODO" :foreground "red" :weight bold)
    ("NEXT" :foreground "blue" :weight bold)
    ("DONE" :foreground "forest green" :weight bold)
    ("WAITING" :foreground "orange" :weight bold)
    ("HOLD" :foreground "magenta" :weight bold)
    ("CANCELLED" :foreground "forest green" :weight bold)
    ("MEETING" :foreground "forest green" :weight bold)
    ("PHONE" :foreground "forest green" :weight bold))))

(setq org-todo-state-tags-triggers
  (quote (("CANCELLED" ("CANCELLED" . t))
    ("WAITING" ("WAITING" . t))
    ("HOLD" ("WAITING") ("HOLD" . t))
    (done ("WAITING") ("HOLD"))
      ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
      ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
      ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq powerline-default-separator 'arrow)
(setq neo-theme 'icons)
(windmove-default-keybindings)
(scroll-bar-mode -1)

(defun set-font-size (font-height)
  (custom-set-faces `(default ((t (:height ,font-height :family "SF Mono"))))))

(set-font-size 130)

;; Enable rainbow-delimiters for all programming modes
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

;; Prevent the system from asking to load the theme everytime.
(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'powerline)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "SF Mono")))))
