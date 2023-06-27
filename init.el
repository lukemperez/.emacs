;; ;; init.el --- Emacs Initialization File
;; ;;
;; ;; Copyright (c) 2015-2022, Luke Matthew Perez
;; ;;
;; ;; * This file is distributed in the hope that it will be useful,
;; ;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;; ;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; ;;   GNU General Public License for more details.
;; ;; * You should have received a copy of the GNU General Public License
;; ;;   along with this file.  If not, see <http://www.gnu.org/licenses/>.
;; ;; * This file is not part of GNU Emacs.
;; ;; * Nevertheless, this init file and related files are in the public
;; ;;   domain and free to use, extend, or modify.

;; ;; Depreciated code now that I use Config.org
;; ;; Load Custom File
;; ;; (setq custom-file "~/.emacs-custom.el")
;; ;;      (load custom-file)

(setq byte-compile-warnings '(cl-functions))
(require 'cl-lib)

;; Turn off mouse interface asap to avoid momentary display
(when window-system
  (menu-bar-mode +1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Set up MELPA so that you can load packages more efficiently.
;; * I use the stable release because I do not want to trouble-
;;   shoot my Emacs when I'm busy writing my dissertation, article,
;;   or book.
;; * For more information, visit <https://melpa.org>

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(setq-default indent-tabs-mode nil)

;; Turn off the blinking cursor
(blink-cursor-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

(show-paren-mode t)

(column-number-mode t)

(global-visual-line-mode)

;; (diminish 'visual-line-mode)
;; (setq uniquify-buffer-name-style 'forward)

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

;; Don't beep at me
(setq visible-bell t)

;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(eval-when-compile
  (require 'use-package))
  ;; (require 'diminish)                ;; if you use :diminish
  (require 'bind-key)		     ;; if you use :bind-key
  (setq use-package-verbose t)

;;Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))
(custom-set-variables)

;; Set default directory
(setq default-directory "/Users/lmp/")
