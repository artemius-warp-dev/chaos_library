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
            (lambda ()
              (with-current-buffer (window-buffer (selected-window))
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

(defun my-flycheck-errors-buffer-setup ()
  "Enable word wrap in Flycheck error list."
  (visual-line-mode 1)
  (setq-local truncate-lines nil))

(add-hook 'flycheck-error-list-mode-hook #'my-flycheck-errors-buffer-setup)

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
         ("C-c a @" . claude-code-ide-insert-at-mentioned))

  :init
  ;; claude lives in ~/.local/bin which may not be on exec-path with -q
  (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
  (setenv "PATH" (concat (expand-file-name "~/.local/bin") ":"
                         (getenv "PATH")))
  (setq claude-code-ide-cli-path      (expand-file-name "~/.local/bin/claude")
        claude-code-ide-terminal-backend 'eat
        claude-code-ide-window-side   'right
        claude-code-ide-window-width  90
        claude-code-ide-use-side-window t
        claude-code-ide-focus-on-open  t
        claude-code-ide-use-ide-diff   t
        ;; flycheck is loaded above; 'auto will pick it up automatically
        claude-code-ide-diagnostics-backend 'auto)

  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'claude-code)
;;; claude-code.el ends here
