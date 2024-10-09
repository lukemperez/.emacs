(setq initial-major-mode #'org-mode
      initial-scratch-message "# This buffer is for notes you don't want to save\n\n")

(setq read-file-name-completion-ignore-case t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq TeX-auto-save nil)
(setq auto-save-list-file-prefix nil)

(defvar my/emacs-cache (concat user-emacs-directory ".cache/")
  "Folder to store cache files in.

Should end with a forward slash.")

(setq custom-file (concat my/emacs-cache "customize.el"))
(setq bookmark-default-file (concat my/emacs-cache "bookmarks"))
(setq recentf-save-file (concat my/emacs-cache "recentf"))
(setq nsm-settings-file (concat my/emacs-cache "network-security.data"))

(require 'ido)
(ido-mode t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package auctex
    :defer t
    :ensure t
    :config
    (setq TeX-auto-save nil)
)

(use-package tex-site
      :ensure auctex
      :defer t
      :config
       (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
       (setq TeX-auto-save nil
             Tex-parse-self t
             reftex-plug-into-AUCTeX t)
       (add-hook 'LaTeX-mode-hook #'Tex-PDF-mode)
       (setq TeX-source-correlate-method 'synctex)
       (setq TeX-source-correlate-mode t)
       (setq latex-run-command "xelatex"))

(setq auto-mode-alist (cons '("\\.latex$" . latex-mode) auto-mode-alist))

       ;; (eval-after-load "tex"
       ;;   '(add-to-list 'TeX-command-list '("latexmk" "latexmk -synctex=1 -shell-escape -pdf %s"
       ;;                                     TeX-run-TeX nil t :help "Process file with latexmk")))
       ;; (eval-after-load "tex"
       ;;   '(add-to-list 'Tex-command-list '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s"
       ;;                                     Tex-run-Tex nil t :help "Process file with xelatex")))

       ;; (add-hook 'Tex-mode-hook (lambda () (setq Tex-command-default "xelatex")))

(setq bibtex-dialect 'biblatex)

;; AucTeX

;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)
;; (setq TeX-PDF-mode t)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
;;
;; (add-hook 'LaTeX-mode-hook (lambda ()
  ;; (push
    ;; '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      ;; :help "Run latexmk on file")
    ;; TeX-command-list)))
;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))


;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
;;
;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
;; (setq TeX-view-program-list
     ;; '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; Set RefTeX to load automatically with AUCTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'markdown-mode-hook 'turn-on-reftex) ; unsure if this works.
(setq reftex-plug-into-AUCTeX t)

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography
      '("~/.bibs/refs.bib"
        "~/Dropbox/AcademicWork/Bibs/refs.bib"
        "~/Dropbox/AcademicWork/Bibs/primary.bib"))

(eval-after-load 'reftex-vars
  '(progn
     ;; (also some other reftex-related customizations)
     (setq reftex-cite-format
           '((?\C-m . "\\cite[]{%l}")
             (?F . "\\footcite[][]{%l}")
             (?T . "\\textcite[?]{%l}")
             (?P . "\\parencite[]{%l}")
             (?a . "\\autocite[?][]{%l}.")
             (?n . "\\nocite{%l}")
             (?p . "[@%l]")
             (?t . "@%l [ ]"))))
)

(setq LaTeX-biblatex-use-Biber t)
(setq TeX-command-BibTeX "Biber")

(require 'ox-md)
(require 'ox-beamer)
(require 'ox-latex)

;; Set our default Notes file.n
;; (setq org-default-notes-file "~/Dropbox/AcademicWork/Org/notes.org")

;; General Academic Notes
;; (global-set-key (kbd "C-c n") 
                ;; (lambda () (interactive) (find-file "~/Dropbox/AcademicWork/Org/notes.org")))

;; A literature review Org-file
;; (global-set-key (kbd "C-c l")
              ;; (lambda () (interactive) (find-file "~/Dropbox/AcademicWork/Org/literature.org")))

;; Dissertation Outline
;; (global-set-key (kbd "C-c d") 
;;                (lambda () (interactive) (find-file "~/Documents/Dissertation/dissertation.org")))

;; A Journal (work in progress)
;; (global-set-key (kbd "C-c j")
                ;; (lambda () (interactive) (find-file "~/Dropbox/AcademicWork/Org/journal.org")))

;; Research File
;; (global-set-key (kbd "C-C r")
              ;; (lambda () (interactive) (find-file "~/Dropbox/AcademicWork/Org/journal.org")))

;; First we need to require org-ref
;; (require 'org-ref)
;; (require 'bibtex)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.rmd\\'" . markdown-mode)
         ("\\.Rmd\\'" . markdown-mode))
  :init )

(use-package pandoc-mode
    :ensure t
    :ensure hydra
    :init
    (add-hook 'markdown-mode-hook 'pandoc-mode)
    (add-hook 'TeX-mode-hook 'pandoc-mode)
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
    (global-set-key (kbd "C-c p") 'pandoc-main-hydra/body)

  )

(use-package polymode
  :ensure t
  :mode
  ("\\.Snw" . poly-noweb+r-mode)
  ("\\.Rnw" . poly-noweb+r-mode)
  ("\\.Rmd" . poly-+r+mode)
  ("\\.md" . poly-markdown-mode)
  )

(require 'quarto-mode)

;; This allows us to switch themes as needed

(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "s-<f12>" 'switch-theme)
(bind-key "s-<f11>" 'disable-active-themes)

(use-package zenburn-theme
  :ensure t
  :load-path "themes"
  :init (setq zenburn-theme t)
  :config (load-theme 'zenburn t)
  )

;; (use-package atom-dark-theme
;; 	 :ensure t
;;      :defer t)
;; (use-package atom-dark-theme
;;   :config
;;   (load-theme 'atom-one-dark t)
;;   )

;; (use-package solarized-theme
        ;; :ensure t
        ;; :defer t
        ;; :config
        ;; (load-theme 'solarized-dark t))

(use-package ess
  :ensure t
  :init (require 'ess-site))

(use-package eshell-syntax-highlighting
:after eshell-mode
:ensure t ;; Install if not already installed.
:config
;; Enable in all Eshell buffers.
(eshell-syntax-highlighting-global-mode +1))

(defun my-eshell-prompt ()
  "Highlight eshell pwd and prompt separately."
  (mapconcat
   (lambda (list)
     (propertize (car list)
                 'read-only      t
                 'font-lock-face (cdr list)
                 'front-sticky   '(font-lock-face read-only)
                 'rear-nonsticky '(font-lock-face read-only)))
   `((,(abbreviate-file-name (eshell/pwd)) :foreground "#dcdccc")
     (,(if (zerop (user-uid)) " # " " $ ") :foreground "green"))
   ""))

(setq eshell-highlight-prompt nil
      eshell-prompt-function  #'my-eshell-prompt)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file. The eshell is renamed to match that directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 4))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)

  (delete-window))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; (require 'yaml-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; remove slashes when presenting italice
; (setq org-hide-emphasis-markers t)

;; (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces 'user
;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.35))))
;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
