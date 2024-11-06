;; Global configuration
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(delete-selection-mode 1)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq visible-bell t)
(xterm-mouse-mode 1)
(global-set-key (kbd "C-c C-d") 'kill-whole-line)
(global-set-key (kbd "C-c d") 'toggle-window-dedicated)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(load-theme 'tango-dark)  ;; or 'tango, 'wombat, etc.
;; Font configuration
(defvar runemacs/default-font-size 125)
(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; Custom Faces
(custom-set-faces
 '(line-number ((t (:foreground "white"))))
 '(line-number-current-line ((t (:foreground "green"))))
 '(linum ((t (:foreground "yellow")))))

;; Initialize package sources
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ;; UX/UI
;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-neotree-config)
;;   (doom-themes-treemacs-config)
;;   (doom-themes-org-config))

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Install and configure go-mode
(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (before-save . gofmt-before-save))
  :config
  (setq tab-width 4)
  (setq indent-tabs-mode 1)
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer nil t)
              (add-hook 'before-save-hook #'lsp-organize-imports nil t))))


;; Enable company-mode for auto-completion
(use-package company
  :ensure t
  :hook (go-mode . company-mode))

;; Enable flycheck for real-time syntax checking
(use-package flycheck
  :ensure t
  :hook (go-mode . flycheck-mode))

;; Configure lsp-mode and lsp-ui for Go
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional: projectile for project management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

;; Optional: magit for git integration
;; Magit for Git
(use-package magit
  :bind ("C-c m s" . magit-status))

(use-package xwidget
  :ensure t)

;; Load org-preview-html for HTML5 previews
(use-package org-preview-html
  :ensure t
  :config
  (setq org-preview-html-enable-xwidgets t)  ;; Ensure xwidget support
  (define-key org-mode-map (kbd "C-c C-p") 'org-preview-html-mode))


;; Function to run the current Go file
(defun my-go-run ()
  "Run the current Go file."
  (interactive)
  (let ((compile-command (concat "go run " buffer-file-name)))
    (compile compile-command)))

;; Function to build the current Go project
(defun my-go-build ()
  "Build the current Go project."
  (interactive)
  (compile "go build"))

;; Function to test the current Go project
(defun my-go-test ()
  "Test the current Go project."
  (interactive)
  (compile "go test ./..."))

;; Add key bindings for Go commands
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") 'my-go-run)
            (local-set-key (kbd "C-c C-b") 'my-go-build)
            (local-set-key (kbd "C-c C-t") 'my-go-test)))

;; End of configuration
