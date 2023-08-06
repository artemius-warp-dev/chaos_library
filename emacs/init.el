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
(setq visible-bell t)
(xterm-mouse-mode 1)
(global-set-key (kbd "C-c C-d") 'kill-whole-line)
(global-set-key (kbd "C-c d") 'toggle-window-dedicated)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defvar runemacs/default-font-size 125)
(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; Package configuration
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package setup
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; UX/UI
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Ivy and Counsel
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Magit for Git
(use-package magit
  :bind ("C-c m s" . magit-status))

;; LSP Mode for language support
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-elixir-server "/home/artgits/Documents/programms/elixir-ls/release/language_server.sh"
        lsp-auto-guess-root t
        lsp-elixir-suggest-specs nil)
  (setq lsp-file-watch-ignored-directories
      '("[/\\\\]\\.git$"
        "[/\\\\]\\.elixir_ls$"
        "[/\\\\]_build$"
        "[/\\\\]assets$"
        "[/\\\\]cover$"
        "[/\\\\]node_modules$"
        "[/\\\\]submodules$")))

;; LSP UI for better experience
(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-use-webkit t
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable t
        lsp-ui-sideline-diagnostic-max-lines 10)
  :bind
  ("C-c s" . lsp-ui-doc-show)
  ("C-c h" . lsp-ui-doc-hide))

;; Company-LSP for auto-completion
(use-package company-lsp
  :commands company-lsp
  :config
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  (push 'company-lsp company-backends))

;; Elixir mode
(use-package elixir-mode
  :bind ("C-c f" . elixir-format)
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (push '(">=" . ?\u2265) prettify-symbols-alist)
              (push '("<=" . ?\u2264) prettify-symbols-alist)
              (push '("!=" . ?\u2260) prettify-symbols-alist)
              (push '("==" . ?\u2A75) prettify-symbols-alist)
              (push '("=~" . ?\u2245) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("->" . ?\u2192) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("|>" . ?\u25B7) prettify-symbols-alist))))

;; Org Roam
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/notes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (require 'org-roam-protocol))


;; Custom Functions
(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet which-key treemacs-projectile treemacs-magit shrink-path
               reformatter rainbow-delimiters org-roam nerd-icons
               lsp-ui json-reformat json-mode ivy-rich
               highlight-indentation helpful flycheck elixir-mode
               doom-themes docker diminish dash-functional counsel
               company-lsp command-log-mode all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
