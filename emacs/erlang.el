;;; erlang.el --- Erlang development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Erlang-specific configuration with LSP, Claude IDE, and development tools
;; Usage: emacs -q --load ~/.emacs.d/erlang.el

;;; Code:

;;;; Load Base Configuration
;; First load the base init.el for general Emacs setup
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
        lsp-idle-delay 0.500))

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
(add-to-list 'load-path "/home/artgits/Documents/code/github/claudemacs")

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :init
  ;; Window configuration
  (setq claude-code-ide-window-side 'right
        claude-code-ide-window-width 100
        claude-code-ide-focus-on-open nil
        claude-code-ide-focus-claude-after-ediff nil
        claude-code-ide-show-claude-window-in-ediff nil
        claude-code-ide-use-ide-diff nil)
  
  ;; Terminal configuration
  (setq claude-code-ide-terminal-backend 'vterm
        claude-code-ide-vterm-anti-flicker t
        claude-code-ide-vterm-render-delay 0.01
        claude-code-ide-terminal-initialization-delay 0.15)
  
  ;; Buffer naming
  (setq claude-code-ide-buffer-name-function
        (lambda (directory)
          (if directory
              (format "*Claude:%s*" (file-name-nondirectory (directory-file-name directory)))
            "*Claude:Global*")))
  
  ;; System prompt for Erlang
  (setq claude-code-ide-system-prompt "You are an expert in Erlang development.")
  
  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'erlang)
;;; erlang.el ends here
