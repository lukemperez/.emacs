
(require 'ido)
(ido-mode t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package flyspell
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    )
  :config
  ;; Sets flyspell correction to use two-finger mouse click
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  )

;; This tells Emacs were to find ispell when using the Mac
(setq ispell-program-name "/usr/local/bin/ispell")

(let (
      (my-paths
       '("~/bin"
         "/usr/local/bin"
         "/usr/bin"
         "/Library/TeX/texbin" ; add path to basictex bin
         "/usr/texbin" ; add path to basictex bin
         "/bin"
         )))

  (setenv "PATH" (concat (getenv "PATH") ":"
                         (mapconcat 'identity my-paths ":")))
  (setq exec-path (append my-paths (list "." exec-directory))))

(use-package tex-site
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (setq TeX-auto-save nil ; remove "/auto/" file generation
        TeX-parse-self t
        reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-mode t)
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list '("latexmk" "latexmk -synctex=1 -shell-escape -pdf %s" TeX-run-TeX nil t :help "Process file with latexmk"))
    )
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk"))
    )
  (add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "latexmk"))))

;; Set RefTeX to load automatically with AUCTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography
      '("~/Dropbox/AcademicWork/Bibs/refs.bib"
        "~/Dropbox/AcademicWork/Bibs/primary.bib"))

(eval-after-load 'reftex-vars
  '(progn
     ;; (also some other reftex-related customizations)
     (setq reftex-cite-format
           '((?\C-m . "\\cite[]{%l}")
             (?f . "\\footcite[][]{%l}")
             (?t . "\\textcite[?]{%l}")
             (?p . "\\parencite[]{%l}")
             (?a . "\\autocite[?]{%l}.")
             (?n . "\\nocite{%l}")))))

; remove slashes when presenting italice
(setq org-hide-emphasis-markers t)

(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

(require 'ox-md)
(require 'ox-beamer)
(require 'ox-latex)

;; Set our default Notes file.
(setq org-default-notes-file "~/Dropbox/AcademicWork/Org/notes.org")

;; General Academic Notes
(global-set-key (kbd "C-c n") 
                (lambda () (interactive) (find-file "~/Dropbox/AcademicWork/Org/notes.org")))

;; Dissertation Outline
(global-set-key (kbd "C-c d") 
                (lambda () (interactive) (find-file "~/Documents/Dissertation/dissertation.org")))

;; A 2018 Journal (work in progress)
(global-set-key (kbd "C-c j")
                (lambda () (interactive) (find-file "~/Dropbox/AcademicWork/Org/journal.org")))

;; DHFS workflow file
(global-set-key (kbd "C-c w")
                (lambda () (interactive) (find-file "~/Dropbox/AcademicWork/Org/dhfs.org")))

;; GTD Org File
(global-set-key (kbd "C-c z")
                (lambda () (interactive) (find-file "~/Dropbox/AcademicWork/Org/gtd.org")))

;; Basic Capture
(global-set-key (kbd "C-c c") 'org-capture)

;; Advanced capture (test)
(setq org-capture-templates
      '(;; testing source: http://www.ideaio.ch/posts/my-gtd-system-with-org-mode.html
        ("q" "Quote" entry (file+datetree "~/Dropbox/AcademicWork/Org/notes.org" "Concepts" "Quotes")
         "* %^{Title} %U         %?")
        ("y" "Connection" entry (file+datetree "~/Dropbox/AcademicWork/Org/notes.org" "Connecting")
         "* %^{Title} %U          %?")

))

;; First we need to require org-ref

;(use-package org-ref
;       :ensure t
;       :init
;       (setq reftex-default-bibliography '(~/Dropbox/AcademicWork/Bibs/refs.bib"))
;       (setq org-ref-default-bibliography '(~/Dropbox/AcademicWork/Bibs/refs.bib"))
;       (setq helm-bibtex-bibliography "~Dropbox/AcademicWork/Bibs/refs.bib"))

;; Next we need to configure some settings.
;; * We begin by setting up the default bibliography
;;   which I have saved in a Dropbox folder.
;; * Then we'll set up bibliographies for notes, and
;;   other purposes.

; (setq reftex-default-bibliography 
;   '("~/Dropbox/AcademicWork/Bibs/refs.bib "))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
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
  ("\\.Rmd" . poly-markdown+r+mode)
  ("\\.md" . poly-markdown-mode)
  )

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

(use-package paganini-theme
        :ensure t
        :defer t)

(use-package zenburn-theme
  :ensure t
      :defer t)

(use-package solarized-theme
        :ensure t
        :defer t)

(use-package github-theme
        :ensure t
        :config
        (load-theme 'github t)
)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq TeX-auto-save nil)
(setq auto-save-list-file-prefix nil)

(setq deft-extensions '("txt" "tex" "org" "md"))
(setq deft-directory "~/Dropbox/AcademicWork/notes")

(setq-default line-spacing 0.3)

(defun org-remove-headlines (backend)
  "Remove headlines with :no_title: tag."
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "no_title"))

(add-hook 'org-export-before-processing-hook #'org-remove-headlines)

(global-set-key "\M-p" 'org-insert-property-drawer)

;; Don't make new frames when opening a new file with Emacs
(setq ns-pop-up-frames nil)

(setq org-footnote-define-inline +1)

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

;;(require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
