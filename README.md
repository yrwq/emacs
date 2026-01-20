# emacs

originally derived from [https://github.com/irhl/sunroom](https://github.com/irhl/sunroom)

quick start
- Leader key is `SPC` (normal mode). Fallback leader is `C-SPC`.
- Reload config: `SPC h r` or `M-x reload`.
- Most of the UI comes from `el/` files loaded by `reload`.

layout
- `init.el`: base settings, OS keys, startup behavior, and the reload list.
- `packages.el`: package setup, keybindings, and most feature config.
- `el/theme.el`: custom gruvbox-ish theme + fonts.
- `el/statusline.el`: statusline built on the tab-bar.
- `el/essentials.el`: helper commands (terminal toggle, window close, etc.).
- `el/dired-ui.el`: Dired visuals and behavior tweaks.
- `el/dev-tools.el`: LSP, DAP, and language hooks.
- `el/ui-extras.el`: text visuals, inline outline highlighting, SVG tags.

ui
- statusline is the tab-bar (mode-line is disabled).
- statusline modules: Evil state, git branch, current file, major mode,
  and line/column on the right.

global keys (macOS)

> M is Cmd, A is Opt

- `M-q`: quit
- `M-w`: close window or current workspace
- `M-j`: toggle vterm popup
- `M-s`: save buffer
- `M-c` / `M-v`: copy / paste
- `M-z` / `M-Z`: undo / redo
- `A-h/j/k/l`: window navigation
- `M-b`: toggle Neotree
- `M-o`: LSP symbols/imenu
- `M-r`: compile

leader keys (telescope-style)
- `SPC f a`: find (buffers + project + recents + bookmarks)
- `SPC f f`: find file
- `SPC f r`: recent files
- `SPC p f`: project files
- `SPC p b`: project buffers
- `SPC p s`: project ripgrep
- `SPC b b`: switch buffer
- `SPC f b`: bookmarks

org + org-roam (daily workflow)
files
- `~/Notes/inbox.org`: capture inbox
- `~/Notes/projects.org`: project trees and tasks
- `~/Notes/next.org`: next actions
- `~/Notes/tickler.org`: scheduled reminders
- `~/Notes/notes.org`: general notes
- `~/Notes/roam/`: org-roam notes
- `~/Notes/roam/daily/`: daily notes
- `~/Notes/archive/`: archives

keys
- `SPC o c`: capture
- `SPC o a`: agenda
- `SPC o f`: roam find
- `SPC o i`: roam insert
- `SPC o d`: roam daily

org editing
- `M-t`: change TODO state
- `M-p`: set priority
- `M-<down>` / `M-<up>`: priority down / up

capture buffer (mac-friendly)
- `M-RET` or `M-s`: finalize capture
- `M-.` or `M-w`: abort capture

project + dev stack
- lsp + lsp-ui (breadcrumbs disabled, hover docs on demand).
- dap configured for python, c/c++, rust, node.
- flycheck enabled globally.
- tree-sitter for syntax highlighting.
- company + company-box for completion UI.

file manager (dired)
- details hidden by default.
- shows a clean PATH header and bounce scroll behavior.
- custom helpers: copy path, open files with external apps.

terminal workflow
- `M-j` toggles a vterm split; same key works inside vterm.

custom visuals
- `ui-extras` replaces `defun/defmacro/defvar` with `fn/macro/var` in Emacs Lisp.
- inline outline highlighting with delimiters (see `el/ui-extras.el`).
