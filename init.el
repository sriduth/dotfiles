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


;;(load-theme 'zonokai-blue)
;; Do not load anything other than the prelude packages and global keybindings

; (require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
; (require 'prelude-ui)
(require 'prelude-core)
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
(global-set-key (kbd "M-q") (lambda () (interactive) (open-org-tasks)))

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
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'prog-mode-hook 'linum-mode)

;; Enable beacon-mode globally
(beacon-mode)

(setq beacon-blink-delay 0.5)
(setq beacon-blink-when-window-changes t)

;; Enable EditorConfig mode globally
(editorconfig-mode)

(add-hook 'web-mode-hook
	  (lambda ()
	    (if (equal web-mode-content-type "javascript")
		(web-mode-set-content-type "jsx"))))
(setq js2-basic-offset 2)

;; Setup company-mode for autocomplete
(global-company-mode)


;; Setup diff-hl mode
(diff-hl-mode)

(global-flycheck-mode)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "21443b9ce8ea8c14f84a625fe70a5be257849ca60348b40da8a2cc408162c7aa" "4575b3521ce30b132011274678b32c8d8406eb1bac6856f7d067dd1bd70e521c" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e")))
 '(package-selected-packages
   (quote
    (molokai-theme erlang csv-mode markdown-mode web-mode flycheck-purescript psc-ide groovy-mode dumb-jump company zop-to-char zonokai-theme zenburn-theme yaml-mode which-key volatile-highlights vkill undo-tree swiper solarized-theme smartrep smartparens smart-mode-line-powerline-theme rainbow-mode rainbow-delimiters ov operate-on-number neotree move-text magit json-mode js2-mode jinja2-mode imenu-anywhere helm-projectile haskell-mode guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region exec-path-from-shell elixir-mode editorconfig easy-kill discover-my-major diminish diff-hl crux browse-kill-ring beacon anzu all-the-icons ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "SF Mono"))))
 '(neo-dir-link-face ((t (:foreground "deep sky blue" :slant normal :weight bold :height 120 :family "SF Mono"))))
 '(neo-file-link-face ((t (:foreground "White" :weight normal :height 120 :family "SF Mono")))))
(load-theme 'zonokai-blue)

;; Prevent the system from asking to load the theme everytime.
(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'powerline)

(require 'psc-ide)

(setq psc-ide-use-npm-bin t)

(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))

(set-face-background 'vertical-border "black")
(set-face-foreground 'vertical-border (face-background 'vertical-border))
(setq linum-format " %d")
(set-face-attribute 'fringe nil :background nil)

;; (global-set-key (kbd "M-TAB") 'company-complete)


