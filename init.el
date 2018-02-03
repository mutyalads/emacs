;; init.el --- Emacs configuration

(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)



(global-unset-key (kbd "C-z"))


;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)
(setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(defvar myPackages
  '(better-defaults
    ein
    elpy
    flycheck
    material-theme
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode -1) ;; disable/enable line numbers globally

;; PYTHON CONFIGURATION
;; --------------------------------------

;; (elpy-enable)
;; (elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake 'elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; dired tree

;; desktop save mode
(desktop-save-mode 1)

;; init.el ends here

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; (use-package pkg-info)

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package sbt-mode
  :ensure t
  :pin melpa)

(use-package yafolding
  :ensure t
  :pin melpa)

(use-package restclient
  :ensure t
  :pin melpa)

(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

(use-package xref-js2
  :disabled
  :ensure t)

(use-package rjsx-mode
  :ensure t)

;;; minor modes for scala developement
(use-package focus
  :ensure t
  :pin melpa)

;; eldoc mode, arguments for methods
(use-package eldoc
  :ensure t
  :pin melpa
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

;; colour code nested brackets
(use-package rainbow-delimiters
  :ensure t
  :pin melpa)
;; helps check syntax for bash python sql etc etc
(use-package flycheck
  :disabled
  :ensure t
  :pin melpa)
;; helps print documentation in minibuffer


;; logview mode for all logs
(use-package logview
  :ensure t
  :pin melpa)

;;ace jump must have
(use-package ace-jump-mode
  :ensure t
  :pin melpa)

(use-package which-key
  :ensure t
  :pin melpa)
(which-key-mode)

;; smart parenthesis
(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

;; back space
(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode)
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

;; Formatting
(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; snippets https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

;; templates

;;; code compilation
;;flycheck
(use-package flycheck-cask
  :commands flycheck-cask-setup
  :config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup)))


;;; git
(use-package magit
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))
;; git gutter
(use-package git-gutter
 :ensure t
 :pin melpa)
;; git timemachine https://github.com/pidu/git-timemachine
(use-package git-timemachine
 :ensure t
  :pin melpa)
;; git timemachine hook ensime
(add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0)))

;;; Navigation
(use-package projectile
  :demand
  :init   (setq
	   projectile-use-git-grep t
	   projectile-enable-caching t
	   projectile-indexing-method 'alien)
  :config (projectile-global-mode t)
  :bind   (("s-r" . projectile-grep)))

;; go to last change
(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))
;; company-mode
(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))

;; flex search similar to intellij search
(use-package flx-ido
  :demand
  :init
  (setq
   ido-enable-flex-matching t
   ;; C-d to open directories
   ;; C-f to revert to find-file
   ido-show-dot-for-dired nil
   ido-enable-dot-prefix t)
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1))

;; higlight symbol
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))
(require 'ensime-expand-region)


;;; ### FRONT END ###

;; JS https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html

;; JS2-mode # todo: add hook 
(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))
;;  (define-key 'js2-mode-map (kbd "C-k") #'js2r-kill))
;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so

(use-package js2-refactor
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package company-tern
  :ensure t)
(use-package tern
  :ensure t)
(use-package origami
  :ensure t)
;; js hooks
(add-hook 'js2-mode-hook (lambda ()
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(defun json-hooks-all ()
                   (restclient-mode)
		   (flycheck-mode)
		   (yafolding-mode))
		   
;; json hooks
(use-package json-mode
  :mode ("\\.json$" . json-mode))
(add-hook 'json-mode-hook 'json-hooks-all)

;; projectile-speedbar
(use-package projectile-speedbar
  :ensure t)
(bind-key "M-<f2>" 'projectile-speedbar-open-current-buffer-in-tree)

(use-package sr-speedbar
  :ensure t)

(use-package neotree
  :ensure t)


;; INSTALLING HASKELL
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

(use-package haskell-mode
  :ensure t
  :bind ([F8] . haskell-navigate-imports))

(use-package hindent
  :ensure t)

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key rjsx-mode yafolding use-package undo-tree smartparens restclient rainbow-delimiters py-autopep8 projectile-speedbar neotree material-theme magit logview js2-refactor highlight-symbol goto-chg git-timemachine git-gutter focus flycheck-cask flx-ido expand-region ensime elpy ein company-tern better-defaults auto-package-update ace-jump-mode))))
;; M-x haskell-mode-stylish-buffer


(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))


;;C-c C-o to invoke the compiler
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))



;;; themes
;;material-theme

;;; fonts
(setq org-src-fontify-natively t)

;;; macros

;;; shortcuts - key bindings
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-c C-d") 'duplicate-line)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)



(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))




;;; hooks
;;scala-mode-hook (todo:// clean up and add to scala-mode)
(add-hook 'scala-mode-hook
          (lambda ()
	    (setq show-trailing-whitespace t)
            (show-paren-mode)
            (smartparens-mode)
            (yas-minor-mode)
            (git-gutter-mode)
            (company-mode)
            (scala-mode:goto-start-of-code)
	    (rainbow-delimiters-mode)
	    (yafolding-mode)
	    )
	  )

;;(add-hook 'prog-mode-hook
  ;;        (lambda () (yafolding-mode)))


;;; shortcuts
(bind-key "C-<Backspace>" 'contextual-backspace)
(bind-key "C-c C-l" 'org-insert-link-global)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c C-o" 'org-open-at-point-global)
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)
(bind-key "S-C-<left>" 'shrink-window-horizontally)
(bind-key "S-C-<right>" 'enlarge-window-horizontally)
(bind-key "S-C-<down>" 'shrink-window)
(bind-key "S-C-<up>" 'enlarge-window)


;; speeding up windows system
(when (eq system-type 'windows-nt)
  (setq w32-pipe-read-delay 0)
  (setq recentf-auto-cleanup 'never)
  (setq w32-get-true-file-attributes nil)
  )
;; setting mac settings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setenv "PATH"
	(concat
	 "/usr/local/bin" ":"
	 (getenv "PATH")
	 ))
  )


;;(bind-key "<C-return>" 'origami-toggle-node)

;; toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; functions
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

 (defun kill-dired-buffers ()
	 (interactive)
	 (mapc (lambda (buffer) 
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
             (kill-buffer buffer))) 
         (buffer-list)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))


;;;init.el ends
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
