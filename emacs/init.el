;;; init.el --- Base Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; General Emacs configuration for all editing tasks

;;; Code:

;;;; Package System Setup
(require 'package)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("elpa"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install helper function
(defun require-or-install (pkg)
  "Ensure PKG is installed and loaded."
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;; Bootstrap use-package
(require-or-install 'use-package)
(setq use-package-always-ensure t)

;;;; UI & Editor Basics
(setq inhibit-startup-message t
      make-backup-files nil
      visible-bell t
      indent-tabs-mode nil
      tab-width 4)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(delete-selection-mode 1)
(xterm-mouse-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode t)

;; Disable line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;;; Key Bindings
(global-set-key (kbd "C-c C-d") 'kill-whole-line)
(global-set-key (kbd "C-c d") 'toggle-window-dedicated)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;;; Font & Theme
(defvar runemacs/default-font-size 125)
(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:foreground "white"))))
 '(line-number-current-line ((t (:foreground "green")))))

;;;; Completion & Navigation
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x b"   . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

(use-package which-key
  :defer 2
  :config
  (which-key-mode))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends '(company-capf company-yasnippet)))

;;;; Project Management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;;;; Additional File Modes
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;;;; Snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;;;; Auto Revert
(global-auto-revert-mode 1)
(setq auto-revert-interval 1
      auto-revert-check-vc-info t)

;;;; Git Integration
(use-package magit
  :bind ("C-c m s" . magit-status))

;;;; LSP Configuration
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
        lsp-ui-sideline-enable t))

;;;; Custom Utility Functions
(defun toggle-window-dedicated ()
  "Toggle window dedication to its buffer."
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (set-window-dedicated-p window (not (window-dedicated-p window)))
    (message "%s: %s"
             (current-buffer)
             (if (window-dedicated-p window)
                 "Can't touch this!"
               "Is up for grabs."))))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(claude-code-ide-cli-extra-flags "")
 '(claude-code-ide-cli-path "/home/artgits/.local/bin/claude")
 '(claude-code-ide-focus-claude-after-ediff t)
 '(claude-code-ide-focus-on-open t)
 '(claude-code-ide-show-claude-window-in-ediff t)
 '(claude-code-ide-switch-tab-on-ediff t)
 '(claude-code-ide-system-prompt nil)
 '(claude-code-ide-use-ide-diff t)
 '(claude-code-ide-use-side-window t)
 '(claude-code-ide-window-height 20)
 '(claude-code-ide-window-side 'right)
 '(claude-code-ide-window-width 90)
 '(package-selected-packages
   '(agent-shell claude-code-ide company counsel dracula-theme eat
		 elixir-mode erlang flycheck go-mode helm-lsp
		 lsp-origami lsp-ui magit org-preview-html projectile
		 vterm web-mode yaml-mode yasnippet)))
