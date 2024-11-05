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
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'my/flycheck-display-error-messages))

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





;; ;; LSP configuration for Elixir
;; (use-package lsp-mode
;;   :commands lsp
;;   :ensure t
;;   :diminish lsp-mode
;;   :init
;;   ;; Add elixir-ls to the exec-path
;;   ;; (add-to-list 'exec-path "/home/artgits/Documents/programms/elixir-ls/release")
;;   (setq lsp-auto-guess-root t)
;;   ;; Ignore certain directories for performance
;;   (setq lsp-file-watch-ignored-directories
;;         '("[/\\\\]\\.git$"
;;           "[/\\\\]\\.elixir_ls$"
;;           "[/\\\\]_build$"
;;           "[/\\\\]assets$"
;;           "[/\\\\]cover$"
;;           "[/\\\\]node_modules$"
;;           "[/\\\\]submodules$"))
;;   :hook
;;   (elixir-mode . lsp-deferred))




(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :commands lsp
  :hook (elixir-mode . lsp)
  :init
  (setq lsp-log-io t))

  ;; Register the LSP client after lsp-mode is loaded
  (with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "/ssh:root@62.106.66.58#56777:/root/spiral/elixir-ls/release/language_server.sh")
                    :major-modes '(elixir-mode)
                    :remote? t
                    :server-id 'elixir-ls-remote)))

;; (require 'eglot)

;; ;; This is optional. It automatically runs `M-x eglot` for you whenever you are in `elixir-mode`:
;; (add-hook 'elixir-mode-hook 'eglot-ensure)

;; ;; Be sure to edit the path appropriately; use the `.bat` script instead for Windows:
;; (add-to-list 'eglot-server-programs '(elixir-mode "/home/artgits/Documents/programms/elixir-ls/release/language_server.sh"))



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

;; ;; Go configuration with LSP
;; (use-package go-mode
;;   :ensure t
;;   :hook (go-mode . lsp-deferred)
;;   :config
;;   (setq lsp-go-build-flags ["-tags=integration,unit"])
;;   (add-hook 'go-mode-hook
;;             (lambda ()
;;               (add-hook 'before-save-hook #'lsp-format-buffer nil t)
;;               (add-hook 'before-save-hook #'lsp-organize-imports nil t))))

;; ;; Org Roam for note-taking
;; (use-package org-roam
;;   :custom
;;   (org-roam-directory (file-truename "~/Documents/notes"))
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ("C-c n j" . org-roam-dailies-capture-today))
;;   :config
;;   (require 'org-roam-protocol))

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
