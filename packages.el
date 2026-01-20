;;; package.el -*- lexical-binding: t -*-
(require 'package)

(setq package-archives
      '(("elpa"  . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (setq package-archive-contents nil))

(unless (package-installed-p 'use-package)
  (error "use-package is not installed. Run M-x package-refresh-contents then M-x package-install RET use-package RET."))

(require 'use-package)
(setq use-package-always-ensure nil)

(use-package ahk-mode)
(use-package lua-mode)

(use-package lsp-lua
  :after lsp-mode
  :hook (lua-mode . lsp-deferred)
  :config
  (setq lsp-lua-workspace-path "~/.config/nvim"))

(use-package fennel-mode
  :mode ("\\.fnl\\'" . fennel-mode)
  :hook (fennel-mode . lsp-deferred)
  :config
  (setq lsp-fennel-enable t))
(use-package powershell)
(use-package fontawesome)
(use-package multiple-cursors)

(use-package gruvbox-theme)
(use-package hydra)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package general
  :demand t
  :config
  (general-evil-setup t)

  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; leader bindings
  (my/leader-keys
    "."   '(find-file :which-key "Open File")
    "fs"  '(save-buffer :which-key "Save")
    "bb"  '(consult-buffer :which-key "Switch Buffer")
    "fb"  '(consult-bookmark :which-key "Bookmarks")
    "ft"  '((lambda () (interactive) (jump-to-register ?T)) :which-key "Todo")
    "fa"  '(my/consult-everything :which-key "Find Anything")
    "ff"  '(consult-find :which-key "Find File")
    "fr"  '(consult-recent-file :which-key "Recent File")
    "ps"  '(consult-ripgrep :which-key "Project Search")
    "pb"  '(consult-project-buffer :which-key "Project Buffers")
    "pf"  '(my/consult-project-find :which-key "Project Files")
    "oa"  '(org-agenda :which-key "Agenda")
    "oc"  '(org-capture :which-key "Capture")
    "of"  '(org-roam-node-find :which-key "Roam Find")
    "oi"  '(org-roam-node-insert :which-key "Roam Insert")
    "od"  '(org-roam-dailies-capture-today :which-key "Roam Today")

    ;; build run debug
    "pc"  '(projectile-compile-project :package projectile :which-key "Build")
    "pp"  '(projectile-switch-project :package projectile :which-key "Switch Project")
    "pr"  '(projectile-run-project :package projectile :which-key "Run")
    "d"   '(dap-debug :which-key "Debug")
    "m"   '(dap-hydra :which-key "Debug Menu")

    "g"   '(magit :which-key "Magit")

    "hr"  '(reload :which-key "Reload Config"))

  (general-define-key
    "M-b" 'neotree-toggle
    "M-m" 'mu4e
    "M-o" 'lsp-ui-imenu
    "M-r" 'compile)

  ;; "K" - dispatch to LSP docs if available, else standard lookup
  (defun my/show-docs ()
    (interactive)
    (if (and (bound-and-true-p lsp-mode)
             (fboundp 'lsp-ui-doc-glance))
        (lsp-ui-doc-glance)
      (message "LSP mode not active or lsp-ui-doc-glance not available. Cannot show LSP docs.")))
  (general-define-key :states 'normal "K" 'my/show-docs)

  ;; quick access
  (set-register ?T (cons 'file "~/Notes/inbox.org")))

(use-package which-key
  :config
  (which-key-mode))

(use-package vertico
  :init
  (vertico-mode))

(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 300
        recentf-max-menu-items 50
        recentf-auto-cleanup 'never))

(use-package consult
  :init
  (setq consult-preview-key '(:debounce 0.2 any)
        consult-project-root-function
        (lambda ()
          (when (fboundp 'projectile-project-root)
            (projectile-project-root))))
  :config
  (defun my/consult-project-find ()
    "Project-aware file finder with a consult fallback."
    (interactive)
    (if (fboundp 'consult-project-find)
        (consult-project-find)
      (let ((default-directory (or (and (fboundp 'projectile-project-root)
                                        (projectile-project-root))
                                   default-directory)))
        (consult-find default-directory))))

  (defun my/consult-everything ()
    "Telescope-like picker for buffers, projects, recents, and bookmarks."
    (interactive)
    (if (boundp 'consult--source-buffer)
        (let ((consult-buffer-sources
               '(consult--source-buffer
                 consult--source-project-buffer
                 consult--source-project-recent-file
                 consult--source-recent-file
                 consult--source-bookmark)))
          (consult-buffer))
      (consult-buffer))))

(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner (expand-file-name "banner.txt" user-emacs-directory))
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-navigator-icons t))

(use-package gcmh
  :ensure t
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 100 1024 1024)) ;; 100MB during activity
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :demand t
  :config
  (when (or (memq window-system '(mac ns x))
            (eq system-type 'darwin))
    (exec-path-from-shell-initialize)))

;; Ensure Homebrew/bin is in PATH for ARM Macs
(when (eq system-type 'darwin)
  (let ((hb "/opt/homebrew/bin"))
    (when (and (file-directory-p hb)
               (not (member hb exec-path)))
      (add-to-list 'exec-path hb)
      (setenv "PATH" (concat hb ":" (getenv "PATH"))))))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-l") ; Super-l as prefix
  :hook (;; replace XXX-mode with concrete major-mode
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (my/leader-keys "l" '(:keymap lsp-command-map :package lsp-mode :which-key "LSP Actions"))
  (general-define-key
   :states 'normal
   :keymaps 'lsp-mode-map
   "gd" 'lsp-find-definition
   "gr" 'lsp-find-references))

(use-package lsp-pyright
  :after lsp-mode)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable t
        lsp-ui-doc-show-with-lsp t ; Explicitly prioritize LSP-provided docs
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-max-width 80))

(use-package neotree
  :defer t
  :config
  (setq neo-window-width 25
        neo-smart-open t
        neo-show-hidden-files t
        neo-theme 'nerd-icons)
  (add-hook 'neo-after-create-hook (lambda (&rest _) (text-scale-set -1))))

(use-package consult-lsp
  :after (consult lsp))

(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-hydra)
  :config
  (dap-auto-configure-mode)
  (dap-ui-mode 1)
  ;; Binds for dap
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :which-key "debugger")))

(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t)
  :bind
  (:map company-active-map
        ("<tab>" . company-select-next)
        ("<backtab>" . company-select-previous)
        ("RET" . company-complete-selection)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package svg-lib)
(use-package svg-tag-mode
  :hook ((text-mode . svg-tag-mode)
         (prog-mode . svg-tag-mode))
  :config
  (defun svg-tag-make (tag &rest args)
    (let* ((face (or (plist-get args :face) 'svg-tag-default-face))
           (font-family (svg-tag--face-attribute face :family))
           (font-weight (or (plist-get args :font-weight)
                            (svg-tag--face-attribute face :weight)))
           (stroke (or (plist-get args :stroke) 0))
           (foreground (svg-tag--face-attribute face :foreground))
           (background (svg-tag--face-attribute face :background))
           (inverse (plist-get args :inverse))
           (tag (string-trim tag))
           (beg (or (plist-get args :beg) 0))
           (end (or (plist-get args :end)))
           (args (svg-tag--plist-delete args :stroke))
           (args (svg-tag--plist-delete args :foreground))
           (args (svg-tag--plist-delete args :background))
           (args (svg-tag--plist-delete args :face))
           (args (svg-tag--plist-delete args :inverse))
           (args (svg-tag--plist-delete args :beg))
           (args (svg-tag--plist-delete args :end)))
      (apply #'svg-lib-tag (substring tag beg end) nil
             :stroke stroke
             :font-family font-family
             :font-weight font-weight
             :foreground (if inverse background foreground)
             :background (if inverse foreground background)
             args)))
  (defun my/svg-tag-with-face (tag face)
    (apply #'svg-tag-make tag
           (plist-put (copy-sequence my/svg-tag-base-options) :face face))))

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (lisp-mode . smartparens-strict-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((css-mode lua-mode emacs-lisp-mode) . rainbow-mode))

(use-package tree-sitter
  :defer t
  :init
  (setq tree-sitter-syntax-highlight-enable t
        tree-sitter-langs-auto-update nil
        tree-sitter-langs-install-latest-grammar-set nil)
  :hook (prog-mode . tree-sitter-mode)
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs))

(use-package undo-fu)
(use-package undo-fu-session
  :after undo-fu
  :config
  (setq undo-fu-session-directory (expand-file-name "hist-undo" user-emacs-directory)
        undo-fu-session-file-limit nil
        undo-fu-session-linear t)
  (undo-fu-session-global-mode))

(defconst my/org-default-directory (expand-file-name "~/Notes"))

(defun my/org-path (file)
  "Return FILE under the default Org directory."
  (expand-file-name file my/org-default-directory))

(use-package org
  :init
  (setq org-directory my/org-default-directory)
  :config
  (define-key org-mode-map (kbd "M-t") #'org-todo)
  (define-key org-mode-map (kbd "M-p") #'org-priority)
  (define-key org-mode-map (kbd "A-<down>") #'org-priority-down)
  (define-key org-mode-map (kbd "A-<up>") #'org-priority-up)
  (setq org-ellipsis "..."
        org-hide-emphasis-markers t
        org-log-done 'time
        org-log-into-drawer t
        org-use-fast-todo-selection t
        org-cycle-separator-lines 2
        org-agenda-span 'week
        org-agenda-start-on-weekday 1
        org-agenda-start-with-log-mode t
        org-agenda-files (list (my/org-path "inbox.org")
                               (my/org-path "projects.org")
                               (my/org-path "next.org")
                               (my/org-path "tickler.org"))
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELLED(c@)"))
        org-priority-highest ?A
        org-priority-lowest ?C
        org-priority-default ?B
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-archive-location (my/org-path "archive/%s_archive::")
        org-capture-templates
        `(("i" "Inbox" entry
           (file ,(my/org-path "inbox.org"))
           "* TODO %?\n%U\n%a\n")
          ("n" "Note" entry
           (file ,(my/org-path "notes.org"))
           "* %?\n%U\n%a\n")
          ("t" "Tickler" entry
           (file ,(my/org-path "tickler.org"))
           "* %?\n%U\n"))))

(use-package org-roam
  :after org
  :config
  (setq org-roam-directory (file-truename (my/org-path "roam"))
        org-roam-completion-everywhere t
        org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n\n")
           :unnarrowed t))
        org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?\n%U\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n\n"))))
  (org-roam-db-autosync-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-fold-stars '(("" . "")
                              ("" . "")
                              ("" . "")
                              ("" . "")
                              ("" . "")))
  (setq org-modern-list '((?- . "•") (?+ . "◦") (?* . "∙"))))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "M-p") #'org-agenda-priority))

(use-package org-capture
  :after org
  :config
  (define-key org-capture-mode-map (kbd "M-RET") #'org-capture-finalize)
  (define-key org-capture-mode-map (kbd "M-.") #'org-capture-kill)
  (define-key org-capture-mode-map (kbd "M-s") #'org-capture-finalize)
  (define-key org-capture-mode-map (kbd "M-w") #'org-capture-kill))

(use-package rust-mode)
(use-package cmake-mode)
(use-package zig-mode)
(use-package haskell-mode)

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.org\\.php\\'" "\\.html\\'" "\\.htm\\'" "\\.vue\\'" "\\.tsx\\'" "\\.jsx\\'")
  :config
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package typescript-mode)

(use-package yaml-mode)
(use-package toml-mode)
(use-package json-mode)
(use-package dockerfile-mode)

;; Documentation
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


(use-package vterm
  :config
  ;; ensure vterm doesn't stole these keys
  (setq vterm-keymap-exceptions
        (append vterm-keymap-exceptions
                '("M-q" "M-w" "M-z" "M-c" "M-v" "M-s" "M-j" "M-h" "M-k" "M-l" "M-\\" "M-f" "M-p" "M-o" "M-n" "M-t"
                  "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9"
                  "A-h" "A-j" "A-k" "A-l"
                  "A-<left>" "A-<right>" "A-<up>" "A-<down>"
                  "M-A-<left>" "M-A-<right>" "M-A-<up>" "M-A-<down>")))

  (define-key vterm-mode-map (kbd "M-q") 'save-buffers-kill-terminal)
  (define-key vterm-mode-map (kbd "M-w") 'my/close-window-or-workspace)
  (define-key vterm-mode-map (kbd "M-z") 'undo-fu-only-undo)
  (define-key vterm-mode-map (kbd "M-c") 'clipboard-kill-ring-save)
  (define-key vterm-mode-map (kbd "M-v") 'clipboard-yank)
  (define-key vterm-mode-map (kbd "M-s") 'save-buffer)
  (define-key vterm-mode-map (kbd "M-j") 'my/terminal-toggle)
  ;; Navigation
  (define-key vterm-mode-map (kbd "A-h") 'windmove-left)
  (define-key vterm-mode-map (kbd "A-j") 'windmove-down)
  (define-key vterm-mode-map (kbd "A-k") 'windmove-up)
  (define-key vterm-mode-map (kbd "A-l") 'windmove-right)
  ;; Resizing
  (define-key vterm-mode-map (kbd "A-<left>") 'windsize-left)
  (define-key vterm-mode-map (kbd "A-<right>") 'windsize-right)
  (define-key vterm-mode-map (kbd "A-<up>") 'windsize-up)
  (define-key vterm-mode-map (kbd "A-<down>") 'windsize-down)
  (define-key vterm-mode-map (kbd "M-A-<left>") 'windsize-left)
  (define-key vterm-mode-map (kbd "M-A-<right>") 'windsize-right)
  (define-key vterm-mode-map (kbd "M-A-<up>") 'windsize-up)
  (define-key vterm-mode-map (kbd "M-A-<down>") 'windsize-down))

(use-package perspective
  :init
  (persp-mode 1))

;; silence warning
(defvar my/warnings-to-kill nil)
(defun my/auto-kill-warnings-buffer (orig-fn &rest args)
  "Advice to kill *Warnings* buffer immediately after it's shown."
  (apply orig-fn args)
  (when-let ((buf (get-buffer "*Warnings*")))
    (when (get-buffer-window buf 'visible)
      (kill-buffer buf))))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package magit)

(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package ligature
  :config
  (global-ligature-mode 0))

(use-package windsize
  :config
  (setq windsize-cols 8
        windsize-rows 4))

(defhydra hydra-window-resize (:hint nil)
  "
  Window Resize: _h_: narrow _j_: shorten _k_: lengthen _l_: widen _=_: balance _q_: quit
  "
  ("h" windsize-left)
  ("j" windsize-down)
  ("k" windsize-up)
  ("l" windsize-right)
  ("=" balance-windows)
  ("q" nil "quit" :exit t))

(use-package mu4e
  :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :config
  (setq mu4e-change-filenames-when-moving t)
  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mailsync")
  (setq mu4e-maildir "~/.local/share/mail/yrwqid@gmail.com")

  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")

  (setq mu4e-html2text-command "w3m -T text/html")

  (setq mu4e-headers-auto-update t
        mu4e-view-show-images t
        mu4e-compose-signature-auto-include nil
        mu4e-use-fancy-chars t)

  (setq mu4e-maildir-shortcuts
      '((:maildir "/Inbox"    :key ?i)
        (:maildir "/[Gmail]/Sent Mail" :key ?s)
        (:maildir "/[Gmail]/Trash"     :key ?t)
        (:maildir "/[Gmail]/Drafts"    :key ?d)
        (:maildir "/[Gmail]/All Mail"  :key ?a))))
