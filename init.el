; -*- lexical-binding: nil; -*-
; Hi! If you're contributing or something, make sure to set Git's
; status.showuntrackedfiles config to "no".

(when (version< emacs-version "30")
  (warn "Koishiemacs would love to be running on Emacs 30 or above!")
  (warn "This init file should break. Try installing the editorconfig and which-key packages manually then restart."))

; Now, define our custom-file.
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

; Then, set some opinionated better defaults.
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 160))

(show-paren-mode 1)
(save-place-mode t)
(editorconfig-mode t)
(which-key-mode t)

(cua-mode t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (not (fboundp 'tab-bar-mode))
  (tab-bar-mode t))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (not (fboundp 'delete-selection-mode))
  (delete-selection-mode t))
(global-tab-line-mode t)
(global-hl-line-mode t)

; Tab changes
(setq-default tab-width 2
              standard-indent 2
              electric-indent-inhibit t
              indent-tabs-mode nil)
(setq c-basic-offset tab-width)

; Misc. options
(setq gc-cons-threshold 134217728  ; Hopefully speed up Emacs.
;     warning-minimum-level :error ; Silence warnings (not recommended)
      scroll-conservatively 1000   ; Don't jump around while scrolling.
      split-width-threshold 120    ; Set constraints for when newly
      split-height-threshold 80    ; Created windows should open a split.
      ring-bell-function 'ignore   ; Don't beep.
      cua-keep-region-after-copy t ; Don't unselect after copying.
      epg-pinentry-mode 'loopback  ; Make gpg pinentry work inside Emacs.
      apropos-do-all t             ; Search more on apropos.
      completion-ignore-case t     ; Make completion case-insensitive.
      read-buffer-completion-ignore-case t    ; Make some minibuffer
      read-file-name-completion-ignore-case t ; entries case-insensitive.
      backward-delete-char-untabify-method 'nil) ; Backspace normally.

; thanks, better-defaults!
(unless backup-directory-alist
  (setq backup-directory-alist
        `((".*" . ,(concat user-emacs-directory "backups")))))
; thanks as well, prelude!
(defconst autosave-folder (expand-file-name "autosaves/" user-emacs-directory))
(unless (file-exists-p autosave-folder)
  (make-directory autosave-folder))
(setq auto-save-file-name-transforms
      `(("\\(.+/\\)*\\(.*?\\)" ,(expand-file-name "autosaves/" user-emacs-directory))))
; reuse current dired buffer by pressing a
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(add-hook 'prog-mode-hook 'whitespace-mode)
(setq whitespace-line-column 999) ; can't think of a sensible value to put in here yet
(eval-after-load 'whitespace
  (lambda ()
    (delete 'newline-mark whitespace-style)))

(pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-interpolate-page t
      pixel-scroll-precision-interpolation-factor 1.5
      pixel-scroll-precision-use-momentum t)

(customize-set-variable
  'tramp-ssh-controlmaster-options
  (concat
    "-o ControlPath=/tmp/%%C"))

(require 'org)
(add-to-list 'org-modules 'org-tempo t)
(setq org-hide-emphasis-markers t)

; https://stackoverflow.com/questions/10969617/hiding-markup-elements-in-org-mode
(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))
(define-key org-mode-map (kbd "C-c e") 'org-toggle-emphasis)

; thanks, witchmacs!
(defalias 'yes-or-no-p 'y-or-n-p)

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

; thanks, magnars! emacs rocks.
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(bind-key "C-x M-e" 'eval-and-replace)

; keybinds
(bind-key "M-n" 'pixel-scroll-interpolate-down)
(bind-key "M-p" 'pixel-scroll-interpolate-up)
(bind-key (kbd "<mouse-movement>") 'ignore)
(bind-key (kbd "<tab-bar> <mouse-movement>") 'ignore)

; thanks, better-defaults!
(bind-key "C-x C-b" 'ibuffer)
(bind-key "M-z" 'zap-up-to-char)

(bind-key "C-c <left>" 'windmove-left)
(bind-key "C-c <right>" 'windmove-right)
(bind-key "C-c <down>" 'windmove-down)
(bind-key "C-c <up>" 'windmove-up)

(bind-key "C-M-<left>" 'shrink-window-horizontally)
(bind-key "C-M-<right>" 'enlarge-window-horizontally)
(bind-key "C-M-<down>" 'shrink-window)
(bind-key "C-M-<up>" 'enlarge-window)


(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(bind-key "C-x o" 'ace-window)

; conditional stuff for system types

; thanks, prelude!
; windows
(when (file-exists-p "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH"))))

; wsl
(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
      (cmd-args '("/c" "start")))
  (when (file-exists-p cmd-exe)
    (setq browse-url-generic-program  cmd-exe
          browse-url-generic-args     cmd-args
          browse-url-browser-function 'browse-url-generic
          search-web-default-browser 'browse-url-generic))))

;;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;; Uncomment the lines below, then `C-x C-e` them to get the goods.

;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go "https://github.com/tree-sitter/tree-sitter-go")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (make "https://github.com/alemuller/tree-sitter-make")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;         (yaml "https://github.com/ikatyang/tree-sitter-yaml")
;;         (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (js-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

; now, package stuff
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

; use-package is installed by default on emacs 29
(unless (package-installed-p 'vertico)
  (package-install 'use-package)
  (package-refresh-contents))
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;      use-package-expand-minimally t)

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode minibuffer-mode)
  :config
  (require 'smartparens-config)
  ; thanks, https://github.com/zcjava/emacs.d_configuration !
  (sp-local-pair 'elisp-mode "'" nil :actions nil)
  (sp-local-pair 'elisp-mode "`" nil :actions nil)
  :bind
  ("C-M-a" . 'sp-beginning-of-sexp)
  ("C-M-e" . 'sp-end-of-sexp)
  ("C-<down>" . 'sp-down-sexp)
  ("C-<up>" . 'sp-backward-up-sexp))

(use-package origami
  :config
  (global-origami-mode)
  :bind
  ("C-c o" . 'origami-toggle-node))

;; Enable Vertico.
(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode)
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;corfu
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil))

;; "I recommend to give Orderless completion a try, which is more flexible and powerful than the default completion styles."

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  :config
  ;; this is vulnerable to ACE but uh... you should be safe editing your own files right
  (setq corfu-auto t
        corfu-quit-no-match 'separator)
  (add-hook 'eshell-mode-hook (lambda ()
                              (setq-local corfu-auto nil)
                              (corfu-mode))))

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
  (doom-themes-org-config)
  (custom-set-faces
   '(tab-bar ((t (:weight bold))))
   '(tab-line-tab-special ((t nil)))))

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
  (setq magit-diff-hide-trailing-cr-characters t
        git-commit-summary-max-length 50
        magit-section-initial-visibility-alist
        '((untracked . show)
          (unstaged . show)
          (unpushed . show)
          (unpulled . show)
          (stashes . show))))

(use-package git-gutter-fringe
  :config
  (setq git-gutter:update-interval 0.2)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  :hook
  (text-mode . git-gutter-mode)
  (org-mode . git-gutter-mode))

(use-package avy
  :bind ("C-;" . 'avy-goto-char)
  :config
  (setq avy-style 'de-bruijn))

(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo/")))))

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
  (setq doom-modeline-position-column-line-format '("L%l C%c"))
  (setq doom-modeline-buffer-encoding nil))

;;; copypasted from lsp-mode's installation page
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :config
  (setq lsp-copilot-enabled nil)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-warn-no-matched-clients nil)
;  (setq lsp-idle-delay 0.1)
  :hook (
     (prog-mode . lsp)
    ;(XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (lsp-completion-provider :none))

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
;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list
  :after (treemacs)
  :init
  (lsp-treemacs-sync-mode 1))

;; optionally if you want to use debugger
(use-package dap-mode
  :custom
  (dap-auto-configure-mode t))
;(require 'dap-gdb-lldb);;  (use-package dap-LANGUAGE) to load the dap adapter for your language

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

(use-package lsp-origami
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

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
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-deferred-git-apply-delay        0.5
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

;;;; My god.

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("C-S-s" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("C-s" . consult-line)                  ;; needed by consult-line to detect isearch
         ("C-S-s" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package consult-flycheck)


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C->" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package flycheck
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

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
  :config
  (global-page-break-lines-mode t))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook (lambda () (hl-line-mode -1)))
  (setq dashboard-projects-backend 'projectile
        dashboard-banner-logo-title "Welcome to Koishiemacs"
        dashboard-image-banner-max-height 300
        dashboard-startup-banner (expand-file-name "koishi.png" user-emacs-directory)
        dashboard-center-content t
        dashboard-page-separator "
"
        dashboard-startupify-list '(dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-banner)
        dashboard-items '((recents   . 5)
                          (projects  . 5)))
  (add-hook 'dashboard-after-initialize-hook
            (lambda () (dashboard-jump-to-recents)))
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (when (eq (buffer-local-value 'major-mode (current-buffer)) 'dashboard-mode)
                (dashboard-refresh-buffer)
                (hl-line-mode -1)))))
;                         (bookmarks . 5)
;                         (projects  . 5)
;                         (agenda    . 5)

(use-package solaire-mode
  :config
  (solaire-global-mode t)
  (add-hook 'dashboard-mode-hook (lambda () (solaire-mode 0))))

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

(use-package emmet-mode
  :hook (mhtml-mode . emmet-mode))

(load (expand-file-name "custom.el" user-emacs-directory))
