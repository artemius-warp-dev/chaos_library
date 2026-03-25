;;; claude-code.el --- Erlang development with Claude Code IDE -*- lexical-binding: t; -*-

;;; Commentary:
;; Erlang-specific configuration with LSP/ELP, Flycheck, and Claude Code IDE
;; integration via claude-code-ide.el (MCP/WebSocket bridge).
;;
;; Usage: emacs -q --load ~/.emacs.d/claude-code.el

;;; Code:

;;;; Load Base Configuration
(load-file (expand-file-name "~/.emacs.d/init.el"))

;;;; Erlang Mode
(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode)
  :hook ((erlang-mode . electric-pair-mode)
         (erlang-mode . display-line-numbers-mode)
         (erlang-mode . column-number-mode)))

;;;; LSP Configuration
(use-package lsp-mode
  :commands lsp
  :hook (erlang-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l"
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting nil
        lsp-log-io nil
        lsp-idle-delay 0.500
        ;; Use ELP (erlang-language-platform) instead of erlang-ls
        lsp-erlang-server 'erlang-language-platform
        ;; OTP version must match the installed runtime (OTP 26)
        lsp-erlang-elp-otp-download-version "26.2"
        ;; Use the user-local ELP binary (v1.1.0+) which has the `server` subcommand.
        ;; The system /usr/local/bin/elp is v0.23.6 and lacks it.
        lsp-erlang-elp-server-command '("/home/artgits/.local/bin/elp" "server"))
  :config
  (require 'lsp-erlang))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 20
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-enable t)
  :config
  ;; Enable soft wrap for lsp-ui-doc popups
  (add-hook 'lsp-ui-doc-frame-hook
            (lambda (_frame window)
              (with-current-buffer (window-buffer window)
                (visual-line-mode 1)
                (setq-local truncate-lines nil)))))

(use-package lsp-origami
  :after lsp-mode
  :hook ((lsp-mode . lsp-origami-try-enable)
         (erlang-mode . origami-mode)))

;;;; Flycheck Configuration
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.33)))

;;;; [Note 2] rgrep: reuse existing window instead of splitting
(add-to-list 'display-buffer-alist
             `(,(rx bos (or "*grep*" "*rgrep*") eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.33)))

;;;; [Note 4] rgrep literal search for strings with special chars ([ ] | etc.)
;; Use M-x counsel-rg, then append  -- -F  to the prompt for literal (fixed-string) matching.
;; Example: UsualMarginComponent -- -F

;;;; [Note 5] Highlight symbol at point across buffer
(use-package auto-highlight-symbol
  :ensure t
  :hook (prog-mode . auto-highlight-symbol-mode)
  :init
  (setq ahs-idle-interval 0.3
        ahs-default-range 'ahs-range-whole-buffer))

;;;; [Note 6] Quick access to Emacs cheatsheet
(defun my-open-emacs-cheatsheet ()
  "Open the Emacs keybinding cheatsheet without stealing focus."
  (interactive)
  (display-buffer (find-file-noselect
                   (expand-file-name "~/Documents/code/emacs-cheatsheet.md"))))

(global-set-key (kbd "C-c h c") #'my-open-emacs-cheatsheet)

;;;; [Note 7] Git branch in mode-line
;; vc-mode is in the default mode-line but may need a refresh nudge.
(setq vc-handled-backends '(Git SVN))
(add-hook 'find-file-hook #'vc-refresh-state)

(defun my-flycheck-errors-buffer-setup ()
  "Enable word wrap in Flycheck error list."
  (visual-line-mode 1)
  (setq-local truncate-lines nil))

(add-hook 'flycheck-error-list-mode-hook #'my-flycheck-errors-buffer-setup)

;;;; [Note 3] eat: C-Insert to copy, S-Insert to paste in semi-char-mode
(with-eval-after-load 'eat
  (define-key eat-semi-char-mode-map (kbd "C-<insert>")
    (lambda ()
      (interactive)
      (if (use-region-p)
          (progn (kill-ring-save (region-beginning) (region-end))
                 (deactivate-mark)
                 (message "Copied"))
        (message "No region selected"))))
  (define-key eat-semi-char-mode-map (kbd "S-<insert>") #'eat-yank))

;;;; Claude Code IDE — Graphical Fixes

;; Issue #131: Unicode spinner/bullet characters (⏺ ✢ ✳) have "ambiguous" East Asian width.
;; Emacs miscalculates their display width causing text to jump during generation.
(dolist (range '((#x23FA . #x23FA)   ; ⏺ record button (Claude bullet)
                 (#x2700 . #x27BF)   ; Dingbats block
                 (#x2200 . #x22FF))) ; Mathematical operators block
  (set-char-table-range char-width-table range 1))

;; Issue #131: Pin the ⏺ bullet to STIX Two Math font for correct advance width.
(defun my-claude-fix-bullet-font ()
  (set-fontset-font t '(#x23FA . #x23FA) "STIX Two Math" nil 'append))
(my-claude-fix-bullet-font)
(add-hook 'after-setting-font-hook #'my-claude-fix-bullet-font)

;; Issue #176: Claude output contains heavy trailing whitespace; hide it in terminal buffers.
(add-hook 'eat-mode-hook  (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'vterm-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

;;;; Claude Code IDE Integration
(use-package websocket :ensure t)
(use-package transient :ensure t)
(use-package eat :ensure t)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)

  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-c a s" . claude-code-ide)
         ("C-c a c" . claude-code-ide-continue)
         ("C-c a r" . claude-code-ide-resume)
         ("C-c a k" . claude-code-ide-stop)
         ("C-c a l" . claude-code-ide-list-sessions)
         ("C-c a t" . claude-code-ide-toggle)
         ("C-c a p" . claude-code-ide-send-prompt)
         ("C-c a @" . claude-code-ide-insert-at-mentioned)
         ("C-c a h" . claude-code-ide-toggle-recent)
         ("C-c a b" . claude-code-ide-switch-to-buffer)
         ("C-c a e" . claude-code-ide-send-escape)
         ("C-c a n" . claude-code-ide-insert-newline))

  :init
  ;; claude lives in ~/.local/bin which may not be on exec-path with -q
  (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
  (setenv "PATH" (concat (expand-file-name "~/.local/bin") ":"
                         (getenv "PATH")))
  (setq claude-code-ide-cli-path           (expand-file-name "~/.local/bin/claude")
        claude-code-ide-terminal-backend   'eat
        claude-code-ide-window-side        'right
        claude-code-ide-window-width       90
        claude-code-ide-use-side-window    t
        claude-code-ide-focus-on-open      t
        claude-code-ide-use-ide-diff       t
        ;; [Note 1] Preserve scroll position when switching away and back to Claude buffer
        claude-code-ide-eat-preserve-position t
        ;; [Note 3] Suppress reflow on height-only resize (prevents selection/scroll glitch #1422)
        claude-code-ide-prevent-reflow-glitch t
        ;; flycheck is loaded above; 'auto will pick it up automatically
        claude-code-ide-diagnostics-backend 'auto
        claude-code-ide-system-prompt
        "You are working on an Erlang backend service at Exante. \
Follow team conventions: cowboy_rest handlers, authdbc auth, \
repo+q for DB (no raw SQL), 99% test coverage via Common Test, \
erlfmt 120-char formatting.")

  :config
  (claude-code-ide-emacs-tools-setup)

  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c a"   "claude"
      "C-c a s" "start"
      "C-c a c" "continue"
      "C-c a r" "resume"
      "C-c a k" "stop"
      "C-c a l" "list sessions"
      "C-c a t" "toggle"
      "C-c a p" "send prompt"
      "C-c a @" "insert @mention"
      "C-c a h" "hide/show"
      "C-c a b" "switch to buffer"
      "C-c a e" "send escape"
      "C-c a n" "insert newline")))

(provide 'claude-code)
;;; claude-code.el ends here
