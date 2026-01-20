(defmacro define-face-specs (&rest specs)
  `(dolist (spec ',specs)
     (face-spec-set (car spec) `((t ,(cadr spec))))))

(define-face-specs
 (rainbow-delimiters-depth-1-face    (:foreground "#fb4934")) ;; red
 (rainbow-delimiters-depth-2-face    (:foreground "#b8bb26")) ;; green
 (rainbow-delimiters-depth-3-face    (:foreground "#fabd2f")) ;; yellow
 (rainbow-delimiters-depth-4-face    (:foreground "#83a598")) ;; blue
 (rainbow-delimiters-depth-5-face    (:foreground "#d3869b")) ;; purple
 (rainbow-delimiters-depth-6-face    (:foreground "#8ec07c")) ;; aqua
 (rainbow-delimiters-depth-7-face    (:foreground "#fe8019")) ;; orange
 (rainbow-delimiters-depth-8-face    (:foreground "#fb4934"))
 (rainbow-delimiters-depth-9-face    (:foreground "#b8bb26"))
 (rainbow-delimiters-depth-10-face   (:foreground "#fabd2f"))
 (rainbow-delimiters-depth-11-face   (:foreground "#83a598"))
 (rainbow-delimiters-depth-12-face   (:foreground "#d3869b"))
 (rainbow-delimiters-base-error-face (:foreground "#fb4934" :weight bold))

 (font-lock-builtin-face             (:foreground "#fe8019")) ;; orange
 (font-lock-comment-face             (:foreground "#928374" :slant italic)) ;; gray
 (font-lock-comment-delimiter-face   (:foreground "#928374"))
 (font-lock-doc-string-face          (:foreground "#928374"))
 (font-lock-constant-face            (:foreground "#d3869b")) ;; purple
 (font-lock-function-name-face       (:foreground "#b8bb26")) ;; green
 (font-lock-keyword-face             (:foreground "#fb4934")) ;; red
 (font-lock-string-face              (:foreground "#b8bb26")) ;; green
 (font-lock-type-face                (:foreground "#fabd2f")) ;; yellow
 (font-lock-variable-name-face       (:foreground "#ebdbb2")) ;; foreground
 (font-lock-preprocessor             (:foreground "#8ec07c")) ;; aqua
 (font-lock-warning-face             (:foreground "#fb4934" :bold t))

 (cursor                             (:background "#ebdbb2"))
 (mc/cursor-face                     (:background "#ebdbb2" :foreground "#282828"))
 (minibuffer-prompt                  (:foreground "#8ec07c" :bold t))
 (hl-line                            (:background "#3c3836"))
 (isearch                            (:background "#fabd2f" :foreground "#282828"))
 (highlight                          (:background "#3c3836"))
 (lazy-highlight                     (:background "#665c54"))
 (show-paren-mismatch                (:background "#fb4934" :foreground "#282828"))
 (show-paren-match                   (:background "#928374" :foreground "#ebdbb2"))
 (region                             (:background "#504945"))

 (link                               (:foreground "#83a598" :underline t))
 (button                             (:foreground "#83a598" :underline t))
 (escape-glyph                       (:foreground "#fe8019"))
 (help-key-binding                   (:foreground "#fabd2f" :box (:line-width 1 :color "#fabd2f")))
 (warning                            (:foreground "#fe8019" :bold t))
 (window-divider                     (:foreground "#3c3836"))
 (window-divider-first-pixel         (:foreground "#3c3836"))
 (window-divider-last-pixel          (:foreground "#3c3836"))
 (vertical-border                    (:foreground "#3c3836"))
 (internal-border                    (:background "#282828"))
 (fringe                             (:background "#282828"))
 (tab-bar                            (:background "#282828" :foreground "#ebdbb2"))

 (dired-directory                    (:foreground "#83a598" :bold t))
 (dired-header                       (:foreground "#fabd2f" :bold t))
 (dired-ignored                      (:foreground "#928374"))
 (dired-marked                       (:foreground "#fe8019" :bold t))
 (dired-mark                         (:foreground "#fe8019"))
 (dired-symlink                      (:foreground "#8ec07c"))
 (dired-broken-symlink               (:foreground "#fb4934" :background "#3c3836"))

 (default
  (:foreground "#ebdbb2"
   :background "#282828"
   :family "IosevkaTerm Nerd Font"
   :height 180))

 (icon
  (:foreground  "#b8bb26"
   :background  "#3c3836"))

 (icon-title
  (:foreground  "#ebdbb2"
   :background  "#3c3836"))

 (line-state
  (:foreground  "#fabd2f"
   :background  "#3c3836"
   :weight bold))

 (line-text
  (:foreground  "#ebdbb2"
   :background  "#3c3836"
   :weight bold))

 (line-blank
  (:foreground  "#3c3836"
   :background  "#3c3836"))

 (line-state-normal
  (:foreground "#282828"
   :background "#b8bb26"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#b8bb26")))

 (line-state-insert
  (:foreground "#282828"
   :background "#fb4934"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#fb4934")))

 (line-state-visual
  (:foreground "#282828"
   :background "#d3869b"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#d3869b")))

 (line-state-replace
  (:foreground "#282828"
   :background "#fe8019"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#fe8019")))

 (line-state-emacs
  (:foreground "#282828"
   :background "#83a598"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#83a598")))

 (line-module-file
  (:foreground "#ebdbb2"
   :background "#504945"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#504945")))

 (line-module-state
  (:foreground "#282828"
   :background "#fabd2f"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#fabd2f")))

 (line-module-mode
  (:foreground "#ebdbb2"
   :background "#3c3836"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#3c3836")))

 (line-module-git
  (:foreground "#ebdbb2"
   :background "#504945"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#504945")))

 (line-module-position
  (:foreground "#ebdbb2"
   :background "#3c3836"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#3c3836")))

 (line-module-lang
  (:foreground "#ebdbb2"
   :background "#3c3836"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#3c3836")))

 (line-module-buffers
  (:foreground "#ebdbb2"
   :background "#3c3836"
   :weight bold
   :height 140
   :box (:line-width 4 :color "#3c3836")))

 (line-module-buffers-current
  (:foreground "#fabd2f"
   :background "#3c3836"
   :weight bold
   :underline t
   :height 140
   :box (:line-width 4 :color "#3c3836")))

 (line-module-right
  (:foreground "#928374"
   :background "#3c3836"
   :height 140
   :box (:line-width 4 :color "#3c3836"))))

;; Font Configuration
(defun my/setup-fonts ()
  "Configure Aporetic as main font and Iosevka for ligatures/symbols."
  (when (display-graphic-p)
    (set-face-attribute 'default nil
                       :family "AporeticSerifMonoNerdFont"
                       :height 180)

    (set-face-attribute 'default nil
                       :family "AporeticSerifMonoNerdFont"
                       :height 180)))

(add-hook 'after-init-hook #'my/setup-fonts)
(add-hook 'after-make-frame-functions (lambda (frame) (my/setup-fonts)))
