(defun my/delete-window (&optional window)
  "Delete WINDOW. If it's the only window, kill the buffer instead."
  (interactive)
  (let ((win (or window (selected-window))))
    (if (one-window-p t)
        (kill-buffer (window-buffer win))
      (delete-window win))))

(defun jump-to-match-paren ()
  "If on a paren, jump to its match."
  (interactive)
  (cond
   ((looking-at-p "\\s(") (forward-sexp 1))
   ((looking-back "\\s)" 1) (backward-sexp 1))
   (t (message "Not on a parenthesis."))))

(defun my/pull-line ()
  (interactive)
  (let* ((start (point))
         (end (line-end-position))
         (text (string-trim-left (buffer-substring start end))))
    (delete-region start end)
    (insert text)
    ;; restore cursor pos
    (goto-char start)))

(defun my/kill-line ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (looking-at "^$")
        (kill-whole-line)
      (kill-line))))

;; unrelated stuff
(defun my/machine-capture ()
  (interactive)
  (let ((choice (ido-completing-read "buffer: " (seq-remove (lambda (name) (string-prefix-p " " name))
                                                         (mapcar #'buffer-name (buffer-list))))))
    (with-current-buffer choice
      (clipboard-kill-ring-save (point-min) (point-max))
      (message "buffer content copied to clipboard!"))))

(defun my/machine-export ()
  (interactive)
  (if buffer-file-name
      (let* ((backup-dir  (expand-file-name "hist-back/" user-emacs-directory))
             (backup-name (read-string "file-name: " (file-name-nondirectory buffer-file-name)))
             (backup-path (expand-file-name backup-name backup-dir)))
        (make-directory backup-dir t)
        (copy-file buffer-file-name backup-path t)
        (message "file exported to %s" backup-path))
    (message "no file associated with this buffer")))

;; compiler stuff linux
(defvar my/term-process nil
  "the currently running foot terminal process.")

(defun my/machine-instructions ()
  (interactive)
  (let* ((file (file-name-nondirectory buffer-file-name))
         (base (file-name-sans-extension file))
         (dir (file-name-directory buffer-file-name))
         (cmd (format "make %s && ./%s; sleep 10" base base)))

    ;; kill previous terminal if it's still running
    (when (and my/term-process
               (process-live-p my/term-process))
      (kill-process my/term-process)
      (setq my/term-process nil))

    ;; launch new, store the process
    (setq my/term-process
          (start-process
           "ghostty"
           nil
           "ghostty" "-e" "bash" "-c"
           (format "cd %s && %s" dir cmd)))))
(defun my/terminal-toggle ()
  "Smart toggle for vterm popup.
If hidden: show and focus.
If shown but not focused: focus.
If shown and focused: hide."
  (interactive)
  (let* ((buffer-name "*vterm*")
         (vterm-buffer (get-buffer buffer-name))
         (window (get-buffer-window buffer-name)))
    (cond
     ((and window (eq window (selected-window)))
      (delete-window window))
     (window
      (select-window window))
     (t
      (let ((win (split-window-below (floor (* (window-height) 0.65)))))
        (select-window win)
        (if vterm-buffer
            (switch-to-buffer vterm-buffer)
          (vterm)))))))

(defun my/close-window-or-workspace ()
  "Context-aware close:
1. If in a vterm popup, hide it.
2. If multiple windows exist, delete the current window.
3. If last window, kill the current workspace/perspective."
  (interactive)
  (let ((win (selected-window)))
    (cond
     ((eq major-mode 'vterm-mode)
      (delete-window win))
     ((not (one-window-p))
      (delete-window win))
     (t
      (if (and (featurep 'perspective) (persp-curr))
          (persp-kill (persp-name (persp-curr)))
        (kill-current-buffer))))))

(defun my/key-test ()
  "Displays the next key pressed in the minibuffer."
  (interactive)
  (let ((key (read-key "Press any key (like Opt+Left)...")))
    (message "Emacs heard: %s" (key-description (vector key)))))

(require 'windmove)
