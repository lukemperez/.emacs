
(setq user-full-name "Luke M Perez"
      user-mail-address "lukemperez@gmail.com")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

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
(diminish 'visual-line-mode)

(setq uniquify-buffer-name-style 'forward)

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

;; Don't beep at me
(setq visible-bell t)

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

(bind-key "M-s o" 'occur-dwim)

(require 'ido)
(ido-mode t)

(use-package centered-cursor-mode
        :ensure t
        :init)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(set-default-font "Inconsolata-15")

(use-package writeroom-mode
  :ensure t
  :init
  (progn
    (define-key global-map [f6] 'writeroom-mode)))

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

(use-package pandoc-mode
  :ensure t
  :ensure hydra
  :init 
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'TeX-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (global-set-key (kbd "C-c p") 'pandoc-main-hydra/body)

)

;; (add-hook 'markdown-mode-hook 'pandoc-mode)
;; (add-hook 'TeX-mode-hook 'pandoc-mode)
;; (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
;; (global-set-key (kbd "C-c p") 'pandoc-main-hydra/body)

;; (require 'poly-R)
;; (require 'poly-markdown)

;;; polymode + markdown
;; (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; polymode + R
;; (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
;; (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(use-package polymode
  :ensure t
  :mode
  ("\\.Snw" . poly-noweb+r-mode)
  ("\\.Rnw" . poly-noweb+r-mode)
  ("\\.Rmd" . poly-markdown+r+mode)
  ("\\.md" . poly-markdown-mode)
  )

(use-package markdown-mode
        :ensure t)

(let (
      (my-paths
       '("~/bin"
         "/usr/local/bin"
         "/usr/bin"
         "/Library/TeX/texbin" ; add path to basictex bin
         "/usr/texbin" ; add path to basictex bin
         "/Applications/Julia.app/Contents/Resources/julia/bin" ; path to julia bin
         "/bin"
         )))
  (setenv "PATH" (concat (getenv "PATH") ":"
                         (mapconcat 'identity my-paths ":")))
  (setq exec-path (append my-paths (list "." exec-directory))))

(use-package tex-site
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (setq TeX-auto-save t
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

;; if this isn't already set in your .emacs
(setq reftex-default-bibliography '("/Dropbox/AcademicWork/Bibs/refs.bib")) 

(setq reftex-bibpath-environment-variables
  '("/Users/lmp/Dropbox/AcademicWork/Bibs/refs.bib"))

(setq reftex-bibpath-environment-variables
'("/Users/lmp/Library/texmf/bibtex/bib"))
(setq reftex-default-bibliography '("/Users/lmp/Dropbox/AcademicWork/Bibs/refs.bib"))
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

(setq reftex-default-bibliography
      (quote
       ("user.bib" "local.bib" "main.bib")))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; Make RefTeX faster
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)

;; Make RefTeX work with Org-Mode
;; use 'C-c ]' instead of 'C-c [' because the latter is already
;; defined in orgmode to the add-to-agenda command.
(defun org-mode-reftex-setup ()
  (load-library "reftex") 
  (and (buffer-file-name)
  (file-exists-p (buffer-file-name))
  (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c ]") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; RefTeX formats for biblatex (not natbib)
(setq reftex-cite-format
      '(
        (?\C-m . "\\cite[]{%l}")
        (?t . "\\textcite{%l}")
        (?a . "\\autocite[]{%l}")
        (?p . "\\parencite{%l}")
        (?f . "\\footcite[][]{%l}")
        (?F . "\\fullcite[]{%l}")
        (?x . "[]{%l}")
        (?X . "{%l}")
        ))

(setq font-latex-match-reference-keywords
      '(("cite" "[{")
        ("cites" "[{}]")
        ("autocite" "[{")
        ("footcite" "[{")
        ("footcites" "[{")
        ("parencite" "[{")
        ("textcite" "[{")
        ("fullcite" "[{") 
        ("citetitle" "[{") 
        ("citetitles" "[{") 
        ("headlessfullcite" "[{")))

(setq reftex-cite-prompt-optional-args nil)
(setq reftex-cite-cleanup-optional-args t)

;; reftex in markdown mode

;; define markdown citation formats
(defvar markdown-cite-format)
(setq markdown-cite-format
      '(
        (?\C-m . "[@%l]")
        (?p . "[@%l]")
        (?t . "@%l")
        )
      )

;; wrap reftex-citation with local variables for markdown format
(defun markdown-reftex-citation ()
  (interactive)
  (let ((reftex-cite-format markdown-cite-format)
        (reftex-cite-key-separator "; @"))
    (reftex-citation)))

;; bind modified reftex-citation to C-c[, without enabling reftex-mode
;;https://www.gnu.org/software/auctex/manual/reftex/Citations-Outside-LaTeX.html#SEC31
(add-hook
 'markdown-mode-hook
 (lambda ()
   (define-key markdown-mode-map "\C-c[" 'markdown-reftex-citation)))

(use-package org-ref
        :ensure t
        :init
        (setq reftex-default-bibliography '("~/Dropbox/_AcademicWork/Bibs/refs.bib"))
        (setq org-ref-default-bibliography '("~/Dropbox/_AcademicWork/Bibs/refs.bib"))
        
        (setq helm-bibtex-bibliography "~Dropbox/_AcademicWork/Bibs/refs.bib"))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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

(setq deft-extensions '("txt" "tex" "org" "md"))
(setq deft-directory "~/Dropbox/AcademicWork/notes")

(global-set-key (kbd "C-c d") 
                (lambda () (interactive) (find-file "~/Documents/Dissertation/dissertation.org")))

(global-set-key (kbd "C-c a") 
                (lambda () (interactive) (find-file "~/Dropbox/AcademicWork/acnotes.org")))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-default-notes-file "~/Dropbox/AcademicWork/acnotes.org")
