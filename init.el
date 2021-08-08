(require 'package)

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 100)
(defvar efs/default-variable-font-size 100)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Add melpa to your packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
)
(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t            
        cider-overlays-use-font-lock t)         
  (cider-repl-toggle-pretty-printing))

(use-package cider-eval-sexp-fu
  :defer t)

(use-package clj-refactor
  :defer t
  :ensure t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package smartparens
  :defer t
  :ensure t
  :diminish smartparens-mode
  :init
  (setq sp-override-key-bindings
        '(("C-<right>" . nil)
          ("C-<left>" . nil)
          ("C-)" . sp-forward-slurp-sexp)
          ("M-<backspace>" . nil)
          ("C-(" . sp-forward-barf-sexp)))
  :config
  (sp-use-smartparens-bindings)
  (sp--update-override-key-bindings)
  :commands (smartparens-mode show-smartparens-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-q")
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config (setq lsp-prefer-flymake nil))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

;; Enable nice rendering of documentation on hover
(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Add company-lsp backend for metals
(use-package company-lsp)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
(use-package lsp-treemacs

;;  :config
;;  (lsp-metals-treeview-enable t)
;;  (setq lsp-metals-treeview-show-when-views-received t)
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline all-the-icons doom-themes org-bullets sass-mode pdf-tools smartparens-config smartparens clj-refactor cider-eval-sexp-fu cider clojure-mode neotree direx dirtree shell-pop idea-darkula-theme immaterial-theme material-theme which-key ace-jump-mode logview rainbow-delimiters expand-region undo-tree flx-ido projectile magit company-lsp yasnippet lsp-ui lsp-metals lsp-mode flycheck sbt-mode scala-mode use-package))
 '(shell-pop-autocd-to-working-dir t)
 '(shell-pop-cleanup-buffer-at-process-exit t)
 '(shell-pop-full-span t)
 '(shell-pop-restore-window-configuration t)
 '(shell-pop-shell-type
   '("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2B2B2B" :foreground "#A9B7C6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Monaco")))))

(use-package projectile
  :demand
  :init   (setq
	   projectile-use-git-grep t
	   projectile-enable-caching t
	   projectile-indexing-method 'alien)
  :config (projectile-mode t)
  :bind   (
	   ("s-r" . projectile-grep)
	   )
  :bind-keymap("C-c p" . projectile-command-map)
 )


(use-package magit
  :commands magit-status magit-blame
  :init (setq
	 magit-revert-buffers nil)
  :bind (("s-g" . magit-status)
	 ("s-b" . magit-blame))
  )
(global-set-key (kbd "C-x g") 'magit-status)


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



(use-package flx-ido
  :demand
  :init
  (setq
   ido-enable-flex-matching t
   ;; C-d to open directories
   ;; C-f to revert to find-file
   ido-show-dot-for-dired nil
   ido-enable-dot-prefix t)
  :config  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1))


(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

(use-package expand-region
  :commands 'er/expand-region
  :bind (("C-=" . er/expand-region)
	 ("C-\-" . er/contract-region)
	 ("C-\"" . er/mark-inside-quotes))
  )


(use-package rainbow-delimiters
  :ensure t)

(add-hook 'scala-mode 'rainbow-delimiters-mode)


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

(use-package shell-pop
  :ensure t
  :pin melpa)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
      (org-roam-directory (file-truename "~/workspace/rudra_it/orgfiles/secondbrain"))
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol))
(setq org-roam-graph-executable "/usr/local/bin/dot")

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

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

;;; to make tramp faster switching of projectile
(defadvice projectile-on (around exlude-tramp activate)
  "This should disable projectile when visiting a remote file"
  (unless  (--any? (and it (file-remote-p it))
                   (list
                    (buffer-file-name)
                    list-buffers-directory
                    default-directory
                    dired-directory))
    ad-do-it))

(setq projectile-mode-line "Projectile")


(set-frame-font "Fira Code Retina 18" nil t)
(bind-key "C-<Backspace>" 'contextual-backspace)
(bind-key "C-c C-l" 'org-insert-link-global)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c C-o" 'org-open-at-point-global)
(bind-key "C-+" 'text-scale-increase)
(bind-key "C-_" 'text-scale-decrease)
(bind-key "S-C-<left>" 'shrink-window-horizontally)
(bind-key "S-C-<right>" 'enlarge-window-horizontally)
(bind-key "S-C-<down>" 'shrink-window)
(bind-key "S-C-<up>" 'enlarge-window)
(bind-key "C-;" 'comment-line)
(bind-key "C-c C-y" 'term-paste)
(global-set-key [mouse-4] 'previous-buffer)
(global-set-key [mouse-3] 'next-buffer)
;; C-t for pop up bash


;;neotree
(global-set-key [f8] 'neotree-toggle)



(which-key-mode)
(global-set-key [f1] 'ace-jump-mode)
(global-set-key (kbd "C-w") 'kill-ring-save)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;  (load-theme 'material t)
(tool-bar-mode -1)
(menu-bar-mode -1)            ; Disable the menu bar
(tooltip-mode -1)           ; Disable tooltips
(scroll-bar-mode -1)
(set-fringe-mode 10)        ; Give some breathing room
(set-display-table-slot standard-display-table 
                        'vertical-border (make-glyph-code 8203))
(setq visible-bell t)


(defun ssh-to-host (num)
  (interactive "P")
  (let* ((buffer-name (format "*host%02d*" num))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (switch-to-buffer buffer)
      (term "/bin/bash")
      (term-send-string
       (get-buffer-process (rename-buffer buffer-name))
       (format "ssh host%02d.foo.com\r" num)))))
