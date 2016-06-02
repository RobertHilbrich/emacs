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
 '(fringe-mode (quote (0)) nil (fringe))
 '(global-linum-mode t)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(linum-format "%d")
 '(menu-bar-mode nil)
 '(minimap-window-location (quote right))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))

(load "server")
(unless (server-running-p) (server-start))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 128 :width normal))))
 '(company-preview ((t (:foreground "dim gray"))))
 '(company-preview-common ((t (:foreground "dim gray"))))
 '(hl-line ((t (:background "gray25"))))
 '(ido-first-match ((t (:foreground "#ccff66"))))
 '(ido-incomplete-regexp ((t (:foreground "#ffffff"))))
 '(ido-indicator ((t (:foreground "#ffffff"))))
 '(ido-only-match ((t (:foreground "#ffcc33"))))
 '(ido-subdir ((t (:foreground "#66ff00"))))
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
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
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

;; YAS
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)

;; Have YAS choose values use a popup menu
(require 'popup)
; add some shotcuts in popup menu mode
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

;; Hl line mode
(global-hl-line-mode)

;;; Elisp stuff
(add-hook 'emacs-lisp-mode-hook 'company-mode)

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
