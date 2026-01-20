(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Developer/Personal" "~/Developer/Work" "~/Desktop" "~/Developer/Repos"))
  (setq projectile-completion-system 'default))

;; global settings
(setq lsp-auto-guess-root t)
(setq lsp-prefer-flymake nil) ; Use flycheck instead (as installed in packages.el)
(setq lsp-enable-snippet t)
(setq lsp-keep-workspace-alive nil)

;; tuning
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; ui
(with-eval-after-load 'lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-max-width 50
        lsp-ui-doc-max-height 13
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t))

;; debugger
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (require 'dap-lldb)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools)
  (require 'dap-node)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(use-package rust-mode
  :hook (rust-mode . lsp-deferred))

(use-package zig-mode
  :hook (zig-mode . lsp-deferred))

(use-package python-mode
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (setq lsp-pyright-langserver-command "basedpyright")
                          (lsp-deferred))))

(use-package cc-mode
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (objc-mode . lsp-deferred)))

(use-package web-mode
  :hook (web-mode . lsp-deferred))

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred))

(use-package js
  :hook (js-mode . lsp-deferred))

(use-package sh-script
  :hook (sh-mode . lsp-deferred))

(provide 'dev-tools)
