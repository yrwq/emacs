(defun get-mark-count ()
  "return a string like [+N] if N files are marked in Dired."
  (when (derived-mode-p 'dired-mode)
    (let ((count (dired-marked-files-count)))
      (when (> count 0)
        (format "[+%d] " count)))))

(defun get-git-branch ()
  "Return current git branch and status."
  (when buffer-file-name
    (let* ((backend (ignore-errors (vc-backend buffer-file-name)))
           (state (when backend (ignore-errors (vc-state buffer-file-name))))
           (revision (when backend (ignore-errors (vc-working-revision buffer-file-name))))
           ;; Try to get branch name from vc-mode string which usually has it
           (branch (when (and vc-mode (string-match "Git[:-]\\(.+\\)" vc-mode))
                     (match-string 1 vc-mode))))

      ;; Fallback to shortened revision if branch can't be parsed
      (unless (and branch (not (string-empty-p branch)))
        (setq branch (when revision
                       (if (> (length revision) 7)
                           (substring revision 0 7)
                         revision))))

      (when branch
        (let ((status-sym (cond ((eq state 'edited) "*")
                                ((eq state 'added) "+")
                                ((eq state 'removed) "-")
                                (t ""))))
          (format "%s%s" branch status-sym))))))

(defun my/tab-bar-gap ()
  (propertize " \n" 'display '(height 0.6) 'face 'default))

(defun my/tab-bar-divider ()
  (propertize " \n" 'display '(raise 0) 'face 'line-blank))

(defun my/tab-bar-top ()
  (let* ((base-window (or (minibuffer-selected-window) (selected-window)))
         (base-buffer (window-buffer base-window)))
    (with-current-buffer base-buffer
      (let* ((state (if (bound-and-true-p evil-mode) evil-state 'emacs))
             (state-face (cond
                          ((eq state 'normal) 'line-state-normal)
                          ((eq state 'insert) 'line-state-insert)
                          ((eq state 'visual) 'line-state-visual)
                          ((eq state 'replace) 'line-state-replace)
                          (t 'line-state-emacs)))
             (mode-name (format-mode-line mode-name))

             ;; modules
             (mod-indicator (propertize "    " 'face state-face))
             (mod-git (let ((git (get-git-branch)))
                        (when (and git (not (string-empty-p git)))
                          (let ((icon (substring-no-properties
                                       (nerd-icons-octicon "nf-oct-git_branch"
                                                           :height 0.8 :v-adjust -0.05))))
                            (propertize (format " %s %s " icon git) 'face 'line-module-git)))))
             (file-name (file-name-nondirectory (or buffer-file-name (buffer-name))))
             (mod-file (propertize (format " %s " file-name) 'face 'line-module-file))
             (line         (format-mode-line "%l"))
             (col          (format-mode-line "%c"))
             (str-right    (format " Ln %s, Col %s " line col))
             (right-width  (string-width str-right))
             (padding      `((space :align-to (- right ,(1+ right-width)))))
             (mod-lang (propertize (format " %s " mode-name) 'face 'line-module-lang))
             (result (concat
                      mod-indicator
                      (or mod-git "")
                      mod-file
                      mod-lang
                      (propertize " " 'display padding 'face 'line-module-position)
                      (propertize str-right 'face 'line-module-position))))
        result))))

(setq tab-bar-format
      '(my/tab-bar-gap
        my/tab-bar-top))

(tab-bar-mode 1)

;; for constant updates to support line/column indicator
(defun my/tab-bar-update ()
  (when (and (display-graphic-p)
             (bound-and-true-p tab-bar-mode))
    (setq tab-bar-format (copy-sequence tab-bar-format))
    (force-window-update (selected-window))))

(add-hook 'post-command-hook #'my/tab-bar-update)
