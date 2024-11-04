(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 160))

(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(setq warning-minimum-level :error)

(setq gc-cons-threshold 134217728)

(set-language-environment "UTF-8")

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

(add-hook 'prog-mode-hook 'whitespace-mode)
(setq whitespace-line-column 999) ; can't think of a sensible value to put in here yet
(eval-after-load 'whitespace
  (lambda ()
    (delete 'newline-mark whitespace-style)))


(show-paren-mode 1)

(setq x-select-enable-clipboard t)

(save-place-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq scroll-conservatively 1000)

(setq split-width-threshold 120)
(setq split-height-threshold 80)

(setq ring-bell-function 'ignore)

(tool-bar-mode -1)
(tab-bar-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method 'nil)

(global-prettify-symbols-mode t)

(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
(electric-pair-mode t)

(customize-set-variable
  'tramp-ssh-controlmaster-options
  (concat
    "-o ControlPath=/tmp/%%C"))

(defun split-and-follow-horizontally ()
      (interactive)
      (split-window-below)
      (balance-windows)
      (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
      (interactive)
      (split-window-right)
      (balance-windows)
      (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)

(cua-mode t)
(setq cua-keep-region-after-copy t)
(setq epg-pinentry-mode 'loopback)

(setq apropos-do-all t)

(global-set-key (kbd "<mouse-movement>") 'ignore)
(global-set-key (kbd "<tab-bar> <mouse-movement>") 'ignore)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(unless (package-installed-p 'swiper)
  (package-refresh-contents))
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

(use-package ivy
  :init
  (ivy-mode t))

(use-package counsel
  :config
  (counsel-mode t))

(use-package swiper
  :bind ("C-s" . 'swiper))

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
        '((counsel-M-x . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode t))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-material t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package projectile
  :init
  (projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-sort-order 'recentf))

(use-package projectile-ripgrep)

(use-package magit
  :config
  (setq magit-diff-hide-trailing-cr-characters t)
  (setq git-commit-summary-max-length 50))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom))

(use-package avy
  :bind ("C-:" . 'avy-goto-char))

;;; copypasted from witchmacs
(use-package switch-window
      :config
      (setq switch-window-input-style 'minibuffer)
      (setq switch-window-increase 4)
      (setq switch-window-threshold 2)
      (setq switch-window-shortcut-style 'qwerty)
      (setq switch-window-qwerty-shortcuts
    '("a" "s" "d" "f" "j" "k" "l"))
      :bind
      ([remap other-window] . switch-window))

(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(defconst nerd-font-installed (expand-file-name "nerd" user-emacs-directory))
(use-package nerd-icons
  :config
  (unless (file-exists-p nerd-font-installed)
  (nerd-icons-install-fonts t)
  (write-region "" nil nerd-font-installed)))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 2) ; square on default res... usually
  (setq doom-modeline-hud t)
  (setq doom-modeline-position-column-line-format "L%l C%c")
  (setq doom-modeline-buffer-encoding nil))

(use-package treemacs
  :functions
    treemacs-follow-mode
  treemacs-fringe-indicator-mode
  treemacs-filewatch-mode
  treemacs-git-mode
  treemacs-hide-gitignored-files-mode
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        nil
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 16)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;;; copypasted from witchmacs
(use-package company
  :functions
    company-select-next
  company-select-previous
    company-abort
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort)
  :hook
  (prog-mode . company-mode))

(use-package yasnippet-snippets)
(use-package yasnippet
  :config (yas-global-mode 1)
  :bind ("C-c y" . 'company-yasnippet))

;;; copypasted from lsp-mode's installation page
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :config
  (setq lsp-modeline-diagnostics-enable t)
;  (setq lsp-idle-delay 0.1)
  :hook (
     (prog-mode . lsp)
    ;(XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode
  :functions
    lsp-ui-peek-find-definitions
  lsp-ui-peek-find-references
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
;; if you are helm user
;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list
  :init
  (lsp-treemacs-sync-mode 1))

;; optionally if you want to use debugger
(use-package dap-mode
  :custom
  (dap-auto-configure-mode t))
;(require 'dap-gdb-lldb);;  (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))
;;;

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package flycheck
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)
  (global-flycheck-mode))

(use-package beacon
  :init
  (beacon-mode t)
  :config
  (setq beacon-push-mark nil)
  (setq beacon-size 15)
  (setq beacon-blink-duration 0.1)
  (setq beacon-blink-delay 0.1))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package page-break-lines
  :hook (dashboard-mode . page-break-lines-mode))
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-banner-logo-title "Welcome to Koishiemacs")
  (setq dashboard-image-banner-max-height 450)
  (setq dashboard-startup-banner "~/.emacs.d/koishi.png")
  (setq dashboard-vertically-center-content t)
  (setq dashboard-startupify-list '(dashboard-insert-newline
                    dashboard-insert-banner-title
                    dashboard-insert-newline
                    dashboard-insert-init-info
                    dashboard-insert-items
                    dashboard-insert-banner))
  (setq dashboard-items '((projects  . 5)
              (recents   . 3))))
;                         (bookmarks . 5)
;                         (projects  . 5)
;                         (agenda    . 5)



(use-package solaire-mode
  :config
  (solaire-global-mode t)
  (add-hook 'dashboard-mode-hook (lambda () (solaire-mode 0))))

(use-package emojify
  :config
  (global-emojify-mode-line-mode)
  :hook (after-init . global-emojify-mode))

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

(load "~/.emacs.d/custom.el")
