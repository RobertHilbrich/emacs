(package-initialize)

;;; For graphic emacs
(if (display-graphic-p)
    (progn 
      ;; OSX
      (if (eq system-type 'darwin)
	  (progn
	    (tool-bar-mode -1)
	    (menu-bar-mode nil)
	    (scroll-bar-mode -1)
	    (setq mac-option-modifier nil)
	    (setq mac-command-modifier 'meta)
	    (setq x-select-enable-clipboard t)  
	    (set-face-attribute 'default nil :family "Monaco")
	    (set-face-attribute 'default nil :height 180)
	    (load "server")
	    (unless (server-running-p) (server-start))
	    (setq exec-path (append exec-path '("/usr/local/bin")))
	    (exec-path-from-shell-initialize)
	  ))
 
      ;; Linux
      (if (eq system-type 'gnu/linux)
	  (progn
	    (tool-bar-mode -1)
	    (menu-bar-mode -1)
	    (scroll-bar-mode -1)
	    (set-face-attribute 'default nil :family "Monospace")
	    (set-face-attribute 'default nil :height 128)
	    (custom-set-faces '(mode-line ((t (:background "#444444" :foreground "#f6f3e8" :box (:line-width 2 :color "#444444"))))))
	    (load "server")
	    (unless (server-running-p) (server-start))
	    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags/")
	    (require 'rtags)))

      ;; Windows
      (if (eq system-type 'windows-nt)
	  (progn
	    (setq ffip-find-executable "c:\\\\cygwin64\\\\bin\\\\find")
	    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")
	    (tool-bar-mode -1)
	    (menu-bar-mode -1)
	    (scroll-bar-mode -1)
	    (set-face-attribute 'default nil :family "Consolas")
	    (set-face-attribute 'default nil :height 128)
	    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
	    (setq ispell-dictionary "german8")
	    (setq python-shell-completion-native-enable nil) ; necessary to have ipython working without warnings in windows
	    (load "server")
	    (unless (server-running-p) (server-start))
    )))
  
  ;; Terminal
  (progn
    (menu-bar-mode -1)
  ))

;; correct the key bindings
;; (if (eq system-uses-terminfo t)
;;     (progn
;;       (define-key input-decode-map "\e[D"  [C-left])
;;       (define-key input-decode-map "\e[C"  [C-right])
;;       (define-key input-decode-map "\e[A"  [C-up])
;;       (define-key input-decode-map "\e[B"  [C-down])
;;       (define-key input-decode-map "\e[4~" [end])
;;       ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-auto-untabify t)
 '(TeX-clean-confirm nil)
 '(TeX-parse-self t)
 '(TeX-save-query nil)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(abbrev-file-name "~/.emacs.d/abbrev_defs")
 '(ac-modes
   (quote
    (emacs-lisp-mode lisp-mode lisp-interaction-mode java-mode javascript-mode js-mode php-mode css-mode makefile-mode sh-mode xml-mode web-mode)))
 '(auctex-latexmk-inherit-TeX-PDF-mode t)
 '(auto-save-default nil)
 '(auto-window-vscroll nil t)
 '(c-basic-offset 4)
 '(company-global-modes (quote (c-mode c++-mode)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(compilation-scroll-output (quote first-error))
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote (default)))
 '(default-major-mode (quote text-mode) t)
 '(delete-selection-mode t)
 '(eldoc-echo-area-use-multiline-p nil)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-python-command "python3")
 '(flycheck-flake8-maximum-line-length 160)
 '(flycheck-global-modes (quote (elpy-mode python-mode latex-mode c++-mode c-mode)))
 '(flymake-fringe-indicator-position nil)
 '(fringe-mode nil nil (fringe))
 '(global-company-mode t)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(iedit-toggle-key-default nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(ispell-silently-savep t)
 '(jedi:complete-on-dot t)
 '(linum-format "%d")
 '(make-backup-files nil)
 '(markdown-coding-system (quote utf-8))
 '(markdown-command "multimarkdown")
 '(markdown-command-needs-filename nil)
 '(markdown-content-type "text/html; charset=utf-8")
 '(markdown-enable-math t)
 '(markdown-enable-wiki-links t)
 '(markdown-header-scaling t)
 '(markdown-header-scaling-values (quote (1.5 1.3 1.1 1.0 1.0 1.0)))
 '(package-selected-packages
   (quote
    (exec-path-from-shell flycheck-color-mode-line helm company-irony company-irony-c-headers python-mode python-info python-docstring py-autopep8 markdown-toc markdown-preview-mode magit jedi iedit google-c-style ggtags flycheck-irony elpy auto-complete-exuberant-ctags auto-complete-clang-async auto-complete-auctex auctex-latexmk ac-etags ac-c-headers)))
 '(py-autopep8-options (quote ("--max-line-length=160")))
 '(python-indent-guess-indent-offset-verbose nil)
 '(python-shell-enable-font-lock nil)
 '(python-shell-font-lock-enable nil)
 '(python-shell-interpreter "python3")
 '(recentf-auto-cleanup 300)
 '(recentf-exclude
   (quote
    ("COMMIT_EDITMSG\\'" ".*-autoloads\\.el\\'" "[/\\]\\elpa/")))
 '(recentf-max-menu-items 10)
 '(recentf-mode t)
 '(safe-local-variable-values
   (quote
    ((flycheck-clang-include-path . "/home/hilb_ro/Development/SUMO/sumo/src")
     (ispell-check-comments . off)
     (ispell-dictionary . "english"))))
 '(scroll-conservatively 10000)
 '(scroll-step 1)
 '(sentence-end "[.?!][]\"')}]*\\($\\| \\| \\)[
;;]*")
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t)))
 '(user-full-name "Robert Hilbrich")
 '(user-mail-address "Robert.Hilbrich@dlr.de"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-completion-face ((t (:foreground "gray30"))))
 '(company-preview ((t (:inherit hl-line :foreground "gray30"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "gray30"))))
 '(fringe ((t (:background "gray15"))))
 '(hl-line ((t (:background "gray18"))))
 '(ido-first-match ((t (:foreground "#ccff66"))))
 '(ido-incomplete-regexp ((t (:foreground "#ffffff"))))
 '(ido-indicator ((t (:foreground "#ffffff"))))
 '(ido-only-match ((t (:foreground "#ffcc33"))))
 '(ido-subdir ((t (:foreground "#66ff00"))))
 '(iedit-occurrence ((t (:inherit default :background "dark slate gray"))))
 '(linum ((t (:inherit (shadow default) :foreground "gray40" :height 90))))
 '(mode-line ((t (:background "#444444" :foreground "#f6f3e8" :box (:line-width 2 :color "#444444")))))
 '(region ((t (:background "brown" :foreground "white"))))
 '(widget-field ((t (:background "gray25")))))

;; We only want the line wrap indicator on the right side
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil right-curly-arrow)) ;; right indicator only

;;; Packages
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("melpa"        . "http://melpa.org/packages/")
			 ("elpy"         . "http://jorgenschaefer.github.io/packages/")))



;; Allow y/n for yes/no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Recent Files
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;; Pop tag mark
(global-set-key "\M-*" 'pop-tag-mark)

;; Auto-Complete
(add-hook 'after-init-hook (lambda () (ac-config-default)))

;; Yas
(add-hook 'after-init-hook (lambda () (yas-global-mode)))

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
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

;; iEDIT
(global-set-key (kbd "C-c i") 'iedit-mode)

;; Hl line mode
(global-hl-line-mode)

;;; Python Mode
(add-hook 'after-init-hook (lambda ()
			     (progn
			       (elpy-enable)
			       (when (executable-find "ipython")
			       	 (setq python-shell-interpreter "ipython"
				       python-shell-interpreter-args "-i --simple-prompt"))
			       (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
			       (add-hook 'python-mode-hook
					 (lambda () (progn
						      (local-set-key (kbd "M-+") 'pop-tag-mark)
						      (if (display-graphic-p)
							  (linum-mode)))))
			       (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))))


;;; Markdown Mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;; LaTeX Mode
(add-hook 'LaTeX-mode-hook (lambda ()
			     (reftex-mode)
			     (visual-line-mode)
			     (LaTeX-math-mode)
			     (flyspell-mode)
			     (TeX-fold-mode)
			     (linum-mode)
			     (setq fill-column 12000)
			     (auto-fill-mode)
			     (define-key LaTeX-mode-map (kbd "<f5>") 'TeX-command-master)
			     (define-key LaTeX-mode-map (kbd "<f6>") 'TeX-next-error)
			     (define-key LaTeX-mode-map (kbd "<f7>") 'TeX-view)
			     (auctex-latexmk-setup)
		     	     (setq TeX-command-default "Latexmk")
			     (setq TeX-command-force "Latexmk")
			     (if (eq system-type 'windows-nt)
			     	 (progn
			     	   (setq TeX-view-program-list '("SumatraPDF" ("SumatraPDF.exe -reuse-instance" (mode-io-correlate " -forward-search %b %n") " %o")))
			     	   (setq TeX-view-program-selection '((output-pdf "SumatraPDF")))))
			     ))

; Wenn ich bei M-q einen Satz pro Zeile haben will (also ein Enter nach jedem Satz-Ende!)
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

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-refresh-status-buffer nil) ; make magit potentially faster

;; Goto Homedirectory
(setq default-directory (concat (getenv "HOME") "/")) 

(markdown-mode)
(visual-line-mode)
