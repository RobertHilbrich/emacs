(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-clean-confirm nil)
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("TeXify" "texify --pdf --tex-option=-synctex=-1 %t" TeX-run-command nil
      (latex-mode))
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files"))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   (quote
    (("Sumatra PDF"
      ("SumatraPDF.exe -reuse-instance"
       (mode-io-correlate " -forward-search %b %n")
       " %o")))))
 '(TeX-view-program-selection (quote ((output-pdf "Sumatra PDF"))))
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(fringe-mode (quote (0)) nil (fringe))
 '(global-linum-mode t)
 '(iedit-toggle-key-default nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(linum-format "%d ")
 '(markdown-header-scaling t)
 '(markdown-header-scaling-values (quote (1.5 1.3 1.1 1.0 1.0 1.0)))
 '(minimap-window-location (quote right))
 '(show-paren-mode t)
 '(transient-mark-mode t))

;;; For graphic emacs
(if (display-graphic-p)
    (if (eq system-type 'darwin)
	(progn
	  (tool-bar-mode -1)
	  (menu-bar-mode t)
	  (scroll-bar-mode -1)
	  (load "server")
	  (unless (server-running-p) (server-start)))
      (progn
	(tool-bar-mode -1)
	(menu-bar-mode -1)
	(scroll-bar-mode -1)
	(load "server")
	(unless (server-running-p) (server-start)))
      )
  (progn
    (menu-bar-mode -1)
  ))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 128 :width normal))))
 '(ac-completion-face ((t (:foreground "dim gray"))))
 '(hl-line ((t (:background "gray18"))))
 '(ido-first-match ((t (:foreground "#ccff66"))))
 '(ido-incomplete-regexp ((t (:foreground "#ffffff"))))
 '(ido-indicator ((t (:foreground "#ffffff"))))
 '(ido-only-match ((t (:foreground "#ffcc33"))))
 '(ido-subdir ((t (:foreground "#66ff00"))))
 '(iedit-occurrence ((t (:inherit default :background "dark slate gray"))))
 '(linum ((t (:inherit (shadow default) :foreground "gray25" :height 90))))
 '(region ((t (:background "brown" :foreground "white")))))

;;; Packages
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;;; User-Info
(setq user-mail-address "Robert.Hilbrich@dlr.de")
(setq user-full-name "Robert Hilbrich")

;;; Disable Autosave
(setq auto-save-default nil)

;; Disable Backup Files
(setq make-backup-files nil)

;; Smooth Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Allow y/n for yes/no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Recent Files
(require 'recentf)
(recentf-mode)
(setq recentf-max-menu-items 10)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;; Markdown Mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; LaTeX
(setq-default TeX-master nil) ; Query for master file.
(add-hook 'LaTeX-mode-hook 
	  (function (lambda ()
		      (require 'reftex)
		      (setq reftex-plug-into-AUCTeX t)
		      (reftex-mode)
		      (require 'tex-mik)
		      (setq TeX-auto-save t)
		      (setq TeX-parse-self t)
		      (setq TeX-auto-untabify t)
		      (setq-default TeX-master nil)
		      (setq TeX-save-query nil)
		      (setq TeX-command-default "TeXify")
		      (setq sentence-end "[.?!][]\"')}]*\\($\\| \\| \\)[
;;]*") ;; Da ist ein "Newline in der Zeile!"
		      (setq sentence-end-double-space nil)
		      (flyspell-mode)
		      (LaTeX-math-mode)
		      (TeX-PDF-mode t)
		      (TeX-fold-mode)
		      (visual-line-mode)
		      (define-key LaTeX-mode-map (kbd "<f5>") 'TeX-command-master) 
		      (define-key LaTeX-mode-map (kbd "<f6>") 'TeX-next-error)
		      (define-key LaTeX-mode-map (kbd "<f7>") 'TeX-view)
		      (setq fill-column 12000)
		      (auto-fill-mode)
	  ))
)

;;; Wenn ich bei M-q einen Satz pro Zeile haben will (also ein Enter nach jedem Satz-Ende!)
(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1)))
        tmp-end)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker --- use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (or
               (bolp)
               (looking-at "\\s *$"))
        (LaTeX-newline)))
    (set-marker to-marker nil)))
(ad-activate 'LaTeX-fill-region-as-paragraph)

;; Spell-Check-Directory
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(require 'ispell)
(setq ispell-program-name "aspell")
(setq ispell-dictionary "german8")

;; IDO Mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode t)

;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; Abbrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; YAS
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)

;; Auto-Complete
(require 'auto-complete-config)
(ac-config-default)

;; Have YAS choose values use a popup menu
(require 'popup)
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

;; iEdit Mode
(require 'iedit)
(global-set-key (kbd "C-c i") 'iedit-mode)

;; Hl line mode
(global-hl-line-mode)

;;; Python stuff
(require 'python-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;;;
;; OSX specific stuff
;;;;
(when (eq system-type 'darwin)

  ;; set the functionality of the apple keys 
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t)  

  ;; set the default font properly
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 160)
)

;;;;
;; LINUX specific stuff
;;;;
(when (eq system-type 'gnu/linux)
  ;; set the default font properly
  (set-face-attribute 'default nil :family "Consolas")
  (set-face-attribute 'default nil :height 140)
)



;; Goto Homedirectory
(setq default-directory (concat (getenv "HOME") "/"))

;;; Default-Mode setzen
(setq default-major-mode 'text-mode)
(text-mode)
(turn-on-auto-fill)
