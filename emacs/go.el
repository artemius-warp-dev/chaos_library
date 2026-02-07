;;; go.el --- Go development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Go-specific configuration with LSP, formatting, and development tools
;; Usage: emacs -q --load ~/.emacs.d/go.el

;;; Code:

;;;; Load Base Configuration
;; First load the base init.el for general Emacs setup
(load-file (expand-file-name "~/.emacs.d/init.el"))

;;;; Go Mode
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (go-mode . electric-pair-mode)
         (go-mode . display-line-numbers-mode)
         (go-mode . column-number-mode))
  :config
  ;; Go uses tabs for indentation
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq indent-tabs-mode t)))
  
  ;; Format and organize imports on save
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'gofmt-before-save nil t)
              (add-hook 'before-save-hook #'lsp-format-buffer nil t)
              (add-hook 'before-save-hook #'lsp-organize-imports nil t))))

;;;; LSP Configuration for Go
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l"
        lsp-prefer-flymake nil
        lsp-enable-snippet nil
        lsp-log-io nil
        lsp-idle-delay 0.500))



;;;; Flycheck for Go
(use-package flycheck
  :ensure t
  :hook (go-mode . flycheck-mode))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.33)))



(use-package web-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (setq web-mode-enable-auto-indentation nil))

;;;; Go-specific Functions
(defun my-go-run ()
  "Run the current Go file."
  (interactive)
  (let ((compile-command (concat "go run " buffer-file-name)))
    (compile compile-command)))

(defun my-go-build ()
  "Build the current Go project."
  (interactive)
  (compile "go build"))

(defun my-go-test ()
  "Test the current Go project."
  (interactive)
  (compile "go test ./..."))

(defun my-go-test-current ()
  "Test the current Go function."
  (interactive)
  (let ((test-name (which-function)))
    (if test-name
        (compile (format "go test -run %s" test-name))
      (message "No test function found at point"))))

;;;; Go Key Bindings
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") 'my-go-run)
            (local-set-key (kbd "C-c C-b") 'my-go-build)
            (local-set-key (kbd "C-c C-t") 'my-go-test)
            (local-set-key (kbd "C-c C-f") 'my-go-test-current)))

(provide 'go)
;;; go.el ends here
