;;;; init.el -*- lexical-binding: t -*-

(unless (>= emacs-major-version 29)
  (error "This config requires Emacs 29 or higher"))

(prefer-coding-system 'utf-8)

(setq default-directory user-emacs-directory)
(defconst my/elisp-dir (expand-file-name "el/" user-emacs-directory))

;; Global Indentation (2 spaces for everything)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default json-mode-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default typescript-indent-level 2)
(setq-default rust-indent-offset 2)
(setq-default python-indent-offset 2)
(setq evil-shift-width 2)

;; org
(setq org-directory "~/Notes")
(require 'org-indent nil t)
(setq org-startup-indented t
      org-indent-indentation-per-level 2
      org-adapt-indentation t
      org-edit-src-content-indentation 0)
(add-hook 'org-mode-hook #'org-indent-mode)

;; cache/misc files
(setq custom-file   (expand-file-name "var/cc.el" user-emacs-directory))
(setq savehist-file (expand-file-name "var/mh.el" user-emacs-directory))

(with-eval-after-load 'multiple-cursors
  (setq mc/list-file (expand-file-name "var/mc.el" user-emacs-directory)))

;; package loading 1
(defvar my/package-loaded nil)

(defun my/load-package ()
  (unless my/package-loaded
    (load-file (expand-file-name "packages.el" user-emacs-directory))
    (setq my/package-loaded t)
    (message "package loaded!")))

(my/load-package)

;; package loading 2
(defvar my/reload-failed nil)

(defvar my/reload-list
  '("theme.el"
    "statusline.el"
    "dired-ui.el"
    "essentials.el"
    "ui-extras.el"
    "dev-tools.el"
    "jcfk/dired-nnn.el"
    ))

(defun reload ()
  (interactive)
  (setq my/reload-failed nil)
  (dolist (file my/reload-list)
    (let ((path (expand-file-name file my/elisp-dir)))
      (condition-case err
          (progn
            (load path)
            (message "loaded: %s" path))
        (error
         (push file my/reload-failed)
         (message "Error loading %s: %s" file (error-message-string err)))))))

;; initial package load
(reload)

;; zoom keybindings
(global-set-key (kbd "M-ű") 'text-scale-increase)
(global-set-key (kbd "M-á") 'text-scale-decrease)

;; MacOS support
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  (setq mac-right-option-modifier nil)

  ;; Standard Mac shortcuts (Cmd is Meta)
  (global-set-key (kbd "M-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "M-w") 'my/close-window-or-workspace)
  (global-set-key (kbd "M-j") 'my/terminal-toggle)
  (global-set-key (kbd "M-s") 'save-buffer)
  (global-set-key (kbd "M-c") 'clipboard-kill-ring-save)
  (global-set-key (kbd "M-v") 'clipboard-yank)
  (global-set-key (kbd "M-z") 'undo-fu-only-undo)
  (global-set-key (kbd "M-Z") 'undo-fu-only-redo)

  ;; Window navigation (Opt + h/j/k/l)
  (global-set-key (kbd "A-h") 'windmove-left)
  (global-set-key (kbd "A-j") 'windmove-down)
  (global-set-key (kbd "A-k") 'windmove-up)
  (global-set-key (kbd "A-l") 'windmove-right))

(load-theme 'gruvbox-dark-soft t)

;; accessories
(setq visible-bell nil
      ring-bell-function 'ignore
      server-client-instructions nil
      inhibit-startup-screen t
      initial-buffer-choice nil
      confirm-kill-processes nil
      confirm-nonexistent-file-or-buffer nil)

(setq make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

(setq history-length 25)
(savehist-mode 1)

;; y/n thing
(setq use-short-answers t)

;; makes tabs show 2 spaces wide
(setq tab-width 2)

;; use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; word wrapping off
(setq-default truncate-lines t)

;; hide dollar sign at the end of truncated lines
(set-display-table-slot standard-display-table 0 ?\ )

;; trim trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; no cursor blink
(blink-cursor-mode 0)

;; show matching pairs
(show-paren-mode 1)

;; show cursor line
(global-hl-line-mode 0)

;; ensures replacement in selected region
(delete-selection-mode 1)

;; disable that yellOw selection thing
(setq select-enable-secondary nil)

;; disable shift selection
(setq shift-select-mode nil)

;; no cursor on inactive windows
(setq-default cursor-in-non-selected-windows nil)

;; scroll line by line
(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively 10000)

;; no unwanted gui elements
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq use-dialog-box nil
      use-file-dialog nil)

;; frame base customization
(setq default-frame-alist
      '((left-fringe . 0)
        (right-fringe . 0)
        (internal-border-width . 24)
        (vertical-scroll-bars . nil)))

(setq frame-inhibit-implied-resize t)

(add-to-list 'default-frame-alist
             (cons 'width (/ (display-pixel-width)
                             (* (frame-char-width) 2))))

(add-to-list 'default-frame-alist
             (cons 'height (/ (display-pixel-height)
                              (floor (* (frame-char-height) 1.5)))))

;; no unwanted single line format
;; i use tab-bar-format for making a statusline
(setq-default mode-line-format nil)
(setq-default header-line-format nil)

;; window base customization
(setq window-divider-default-bottom-width 2)
(setq window-divider-default-right-width 4)
(setq window-divider-default-places t)
(window-divider-mode 1)

;; auto split vertically (side-by-side)
;; when emacs itself spawns a window
(setq split-width-threshold 0
      split-height-threshold nil)

(setq display-buffer-base-action
      '((display-buffer-reuse-window display-buffer-same-window)))

(defun buf-window-swap () (interactive)
  ;; check if there are exactly two windows
  (if (/= (count-windows) 2)
      (message "need exactly two windows to swap.")

    ;; otherwise, get the list of windows
    (let* ((wins (window-list))
           ;; get the buffers of first two windows
           (buf1 (window-buffer (car wins)))
           (buf2 (window-buffer (cadr wins))))

      ;; swap the buffers displayed in the first two windows
      (set-window-buffer (car wins) buf2)
      (set-window-buffer (cadr wins) buf1))))

(defun buf-dired-list ()
  (let (bufs)
    (dolist (buf (buffer-list) bufs)
      (with-current-buffer buf
        (when (derived-mode-p 'dired-mode)
          (push buf bufs))))))

(defun buf-dired-limit ()
  (let* ((current (current-buffer))
         (dired-bufs (buf-dired-list))
         (kill-cands (remove current dired-bufs)))
    (when (> (length dired-bufs) 4)
      (setq kill-cands
            (sort kill-cands
                  (lambda (a b)
                    (time-less-p (buffer-local-value 'buffer-display-time a)
                                 (buffer-local-value 'buffer-display-time b)))))
      (while (> (+ 1 (length kill-cands)) 4)
        (when (buffer-live-p (car kill-cands))
          (kill-buffer (car kill-cands)))
        (setq kill-cands (cdr kill-cands))))))

(add-hook 'dired-mode-hook #'buf-dired-limit)

;; organize ibuffer list
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Special" (or (name . "^\\*.*\\*$")))
         ("Directory" (mode . dired-mode)))))

(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; rename scratch buffer
(defun buf-create-startup (&optional base-buffer)
  (let ((name "untitled.el")
        buf)
    (setq buf (or (get-buffer name)
                  base-buffer
                  (get-buffer-create name)))
    (with-current-buffer buf
      (set-visited-file-name name nil t)
      (rename-buffer name t)
      (emacs-lisp-mode))
    buf))
