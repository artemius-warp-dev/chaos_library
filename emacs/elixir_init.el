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

;; Font configuration
(defvar runemacs/default-font-size 125)
(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; Custom Faces
(custom-set-faces
 '(line-number ((t (:foreground "white"))))
 '(line-number-current-line ((t (:foreground "green"))))
 '(linum ((t (:foreground "yellow")))))

;; Package setup
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

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Ivy and Counsel
(use-package ivy
  :diminish
  :config (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Magit for Git
(use-package magit
  :bind ("C-c m s" . magit-status))


(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-modeline-code-actions-segments '(count icon name))

  :init
  '(lsp-mode))


(use-package elixir-mode
  :ensure t
  :custom
  (lsp-elixir-server-command '("/home/artgits/Documents/programms/lexical/_build/dev/package/lexical/bin/start_lexical.sh")))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))  ;; Enable yasnippet globally

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends '(company-capf company-yasnippet)))  ;; Add company-yasnippet backend


;; Syntax highlighting and prettification for Elixir
(use-package elixir-mode
  :bind ("C-c f" . elixir-format)
  :hook (elixir-mode . (lambda ()
                         (push '(">=" . ?\u2265) prettify-symbols-alist)
                         (push '("<=" . ?\u2264) prettify-symbols-alist)
                         (push '("!=" . ?\u2260) prettify-symbols-alist)
                         (push '("==" . ?\u2A75) prettify-symbols-alist)
                         (push '("=~" . ?\u2245) prettify-symbols-alist)
                         (push '("<-" . ?\u2190) prettify-symbols-alist)
                         (push '("->" . ?\u2192) prettify-symbols-alist)
                         (push '("|>" . ?\u25B7) prettify-symbols-alist)))
  :hook (elixir-mode . lsp-deferred))

;; Optional UI for lsp-mode
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable t))


;; Custom function to toggle window dedication
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
 '(package-selected-packages
   '(org-preview-html yafolding exec-path-from-shell go-mode yasnippet
                      which-key treemacs-projectile treemacs-magit
                      shrink-path reformatter rainbow-delimiters
                      org-roam nerd-icons lsp-ui json-reformat
                      json-mode ivy-rich highlight-indentation helpful
                      flycheck elixir-mode doom-themes docker diminish
                      dash-functional counsel company command-log-mode
                      all-the-icons)))
