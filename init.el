;;; init.el --- kovrik's Emacs config

;;; Commentary:

;;; Code:
(setq debug-on-error t)

;; ==========================================================================
;; Package management
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                           ("org"          . "http://orgmode.org/elpa/")
                           ("melpa"        . "http://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ("marmalade"    . "http://marmalade-repo.org/packages/"))))
(package-initialize)
(add-to-list 'package-pinned-packages '(queue   . "gnu")          t)
(add-to-list 'package-pinned-packages '(company . "gnu")          t)
(add-to-list 'package-pinned-packages '(cider   . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(geiser  . "melpa-stable") t)

;; Make sure to have downloaded archive description.
;; or use package-archive-contents
(or (file-exists-p (concat package-user-dir "/archives"))
    (package-refresh-contents))

;; ==========================================================================
;; use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(require 'diminish)
(require 'bind-key)
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(defun ensure-packages-installed (&rest packages)
  "Assure every package in PACKAGES is installed, ask for installation if it’s not.  Return a list of installed packages or nil for every skipped package."
  (dolist (p packages)
    (when (and (not (package-installed-p p))
               (y-or-n-p (format "Package %s is missing.  Install it? " p)))
      (package-install p))))

(ensure-packages-installed
 'queue
 'async
 'autopair
 'browse-kill-ring
 'dash
 'epl
 'f
 'fold-dwim
 'fringe-helper
 'goto-chg
 'highlight
 'highlight-escape-sequences
 'highlight-parentheses
 'idle-highlight-mode
 'markdown-mode
 'pkg-info
 's)

;; ==========================================================================
;; Themes
(ensure-packages-installed
 'ample-theme
 'darcula-theme
 'leuven-theme
 'material-theme
 'minimal-theme
 'noctilux-theme
 'soft-stone-theme
 'solarized-theme
 'sublime-themes
 'twilight-bright-theme
 'zenburn-theme
 )
;; ==========================================================================

;; ==========================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(git-gutter:hide-gutter t)
 '(package-selected-packages
   (quote
    (corral ample-theme auto-compile autopair browse-kill-ring cider clojure-mode-extra-font-locking color-theme-solarized company company-quickhelp darcula-theme dired+ elisp--witness--lisp erc-hl-nicks evil-leader evil-nerd-commenter evil-numbers evil-org evil-search-highlight-persist evil-surround expand-region f fixme-mode flycheck flycheck-clojure flycheck-pos-tip fold-dwim git-gutter helm-projectile helm-swoop highlight-escape-sequences highlight-parentheses idle-highlight-mode ido-ubiquitous ido-vertical-mode leuven-theme magit markdown-mode material-theme neotree noctilux-theme racket-mode rainbow-delimiters smart-mode-line soft-stone-theme solarized-theme sublime-themes use-package zenburn-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal))))
 '(diff-added ((t (:foreground "#ffffff" :background "#43a047"))))
 '(diff-changed ((t (:foreground "#000000" :background "#ffc107"))))
 '(diff-removed ((t (:foreground "#ffffff" :background "#e53935"))))
 '(erc-direct-msg-face ((t (:foreground "#e53935"))))
 '(erc-input-face ((t (:foreground "dark green"))))
 '(erc-timestamp-face ((t (:foreground "#9966ca"))))
 '(eval-sexp-fu-flash ((t (:background "#039BE5"))))
 '(evil-search-highlight-persist-highlight-face ((t (:background "#ffffbb" :foreground "#000000"))))
 '(flycheck-fringe-error ((t (:background "#e53935" :foreground "#e53935"))))
 '(flycheck-fringe-info ((t (:background "#43a047" :foreground "#43a047"))))
 '(flycheck-fringe-warning ((t (:background "#ffc107" :foreground "#ffc107"))))
 '(font-lock-keyword-face ((t (:foreground "#8B008B"))))
 '(font-lock-string-face ((t (:foreground "dark green"))))
 '(git-gutter:added ((t (:foreground "#43a047" :background "#43a047"))))
 '(git-gutter:deleted ((t (:foreground "#e53935" :background "#e53935"))))
 '(git-gutter:modified ((t (:foreground "#ffc107" :background "#ffc107"))))
 '(helm-selection ((t (:background "#bdeebd" :foreground "#000000"))))
 '(helm-source-header ((t (:background "#607d8b" :foreground "#ffffff" :height 120))))
 '(hl-line ((t (:background "#ccddee"))))
 '(link ((t (:foreground "#bb00f8"))))
 '(magit-section-title ((t (:height 1.2 :family nil))))
 '(magit-item-highlight ((t (:background "#ffffbb"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#000000"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#ff0000"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#0000b8"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff8c00"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#009800"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#ff00ff"))))
 '(vertical-border ((t (:background "#ffffff")))))
;; =========================================================================
;; PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin" ":/usr/local/Cellar" (getenv "USERPROFILE")))
(setq exec-path (append exec-path '("/usr/local/bin")))
;; =========================================================================

;; =========================================================================
;; Init and globals

;; Backups directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(when window-system
  (tooltip-mode    -1)
  (tool-bar-mode   -1)
  (menu-bar-mode   -1)
  (scroll-bar-mode -1))

(setq-default indent-tabs-mode nil
              tab-width 2)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      sentence-end-double-space nil
      initial-scratch-message nil
      visible-bell nil
      ring-bell-function 'ignore
      use-dialog-box nil)

(autopair-global-mode)
(column-number-mode t)
(global-font-lock-mode t)
(show-paren-mode t)
(blink-cursor-mode -1)
(winner-mode t)
(desktop-save-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key [escape]    'keyboard-quit)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-h") 'find-function-at-point)
;; Reload file from disk
(global-set-key (kbd "C-c r") 'revert-buffer)

(defun linum-on  () "Turn 'linum-mode' on."  (linum-mode 1))
(defun linum-off () "Turn 'linum-mode' off." (linum-mode -1))
;; =========================================================================
;; Colors and Fonts
;; (load-theme 'twilight-bright)
;; (load-theme 'noctilux)
;; (load-theme 'material-light)

(let ((myfont
       (cond
         ((find-font (font-spec :name "Meslo LG S"))      "Meslo LG S 10")
         ((and (eq 'windows-nt system-type)
               (find-font (font-spec :name "Consolas")))  "Consolas 11")
         ((find-font (font-spec :name "Monaco"))          "Monaco 13")
         ((find-font (font-spec :name "Menlo"))           "Menlo 13")
         ((find-font (font-spec :name "Source Code Pro")) "Source Code Pro 13"))))
  (print (format "Using font: %s" myfont))
  (set-face-attribute 'default nil :font myfont)
  (set-frame-font      myfont  nil t))

(global-hl-line-mode)

;; =========================================================================
(use-package paradox :ensure t)
;; =========================================================================

;; =========================================================================
(use-package color-theme
  :ensure t
  :config (color-theme-initialize))
;; =========================================================================

;; =========================================================================
(use-package smart-mode-line
  :ensure t
  :config (progn
            (setq sml/theme 'light)
            (sml/setup)))
;; =========================================================================

;; =========================================================================
(use-package dired
  :config (progn
            (put 'dired-find-alternate-file 'disabled nil)
            (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)))
;; =========================================================================

;; =========================================================================
;; FIXME Not working with Helm Find Files
(use-package tramp
  :ensure t
  :config (progn
            (setq tramp-default-method "ssh")))
;; =========================================================================

;; =========================================================================
(use-package eshell
  :config (progn
            (setq eshell-cmpl-cycle-completions nil
                  eshell-save-history-on-exit   t
                  eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")))
;; =========================================================================

;; =========================================================================
(use-package fixme-mode
  :ensure t
  :config (fixme-mode t))
;; =========================================================================

;; =========================================================================
(use-package linum
  :ensure t
  :config (progn
            (setq linum-format (if (display-graphic-p) " %3d" "%4d "))
            (add-hook 'prog-mode-hook #'linum-on)
            (add-hook 'text-mode-hook #'linum-on)))
;; =========================================================================

;; =========================================================================
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))
;; =========================================================================

;; =========================================================================
;; FIX eval-sexp-fu error: apply: Symbol's value as variable is void: hi
(use-package eval-sexp-fu :ensure t)
;; =========================================================================

;; =========================================================================
(use-package corral
  :ensure t
  :config (progn
            (setq corral-preserve-point t)
            (global-set-key (kbd "M-9")  'corral-parentheses-backward)
            (global-set-key (kbd "M-0")  'corral-parentheses-forward)
            (global-set-key (kbd "M-[")  'corral-brackets-backward)
            (global-set-key (kbd "M-]")  'corral-brackets-forward)
            (global-set-key (kbd "M-{")  'corral-braces-backward)
            (global-set-key (kbd "M-}")  'corral-braces-forward)
            (global-set-key (kbd "M-\"") 'corral-double-quotes-backward)))
;; =========================================================================

;; =========================================================================
;; (use-package paredit
;;   :ensure t
;;   :init (progn
;;           (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;;           (add-hook 'clojure-mode-hook    #'paredit-mode)
;;           (add-hook 'lisp-mode-hook       #'paredit-mode)
;;           (add-hook 'scheme-mode-hook     #'paredit-mode)
;;           (add-hook 'racket-mode-hook     #'paredit-mode)))
;; =========================================================================

;; =========================================================================
(use-package rainbow-delimiters
  :ensure t
  :config (progn
            (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
            (add-hook 'clojure-mode-hook    #'rainbow-delimiters-mode)
            (add-hook 'lisp-mode-hook       #'rainbow-delimiters-mode)
            (add-hook 'scheme-mode-hook     #'rainbow-delimiters-mode)
            (add-hook 'racket-mode-hook     #'rainbow-delimiters-mode)))
;; =========================================================================

;; =========================================================================
(use-package clojure-mode
  :defer  t
  :ensure t
  :config (progn
            (use-package cider :pin melpa :ensure t)
            (use-package clojure-mode-extra-font-locking :ensure t)
            ;; (use-package clojure-snippets :ensure t)
            ;; (use-package cider-eval-sexp-fu :ensure t :config (require 'cider-eval-sexp-fu))

            ;; FIXME
            ;; (use-package kibit-mode
            ;;   :pin melpa-stable
            ;;   :ensure t)

            (use-package flycheck-clojure
              :pin melpa
              :ensure t
              :config (flycheck-clojure-setup))

            (add-hook 'clojure-mode-hook 'flycheck-mode)

            ;; Cider
            (add-hook 'clojure-mode-hook    'turn-on-eldoc-mode)
            (add-hook 'cider-mode-hook      'cider-turn-on-eldoc-mode)
            (add-hook 'cider-mode-hook      'company-mode)
            (add-hook 'cider-repl-mode-hook 'company-mode)

            (setq nrepl-log-messages t
                  nrepl-hide-special-buffers t
                  cider-prefer-local-resources t
                  ;; cider-repl-pop-to-buffer-on-connect nil
                  cider-popup-stacktraces nil
                  cider-repl-popup-stacktraces t)

            (defun cider-find-var-no-prompt ()
              "cider-find-var at point without prompt"
              (interactive)
              (cider-find-var t nil))

            (define-key clojure-mode-map (kbd "C-h")   #'cider-find-var-no-prompt)
            (define-key clojure-mode-map (kbd "C-SPC") #'cider-eval-defun-at-point)
            (define-key clojure-mode-map (kbd "C-M-x") #'cider-eval-defun-at-point)))
;; =============================================================

;; =============================================================
;; FIXME Not working
;; FIXME Prevent from random buffer switching on errors
;; FIXME Company mode not working well
;; FIXME Flycheck not working well
(use-package geiser :ensure t :defer  t)

(use-package racket-mode
  :defer  t
  :ensure t
  :config (progn
            (add-hook 'racket-mode-hook #'company-quickhelp--disable)))
;; =============================================================

;; =============================================================
(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))
;; =============================================================

;; =============================================================
(use-package projectile
  :defer  t
  :ensure t
  :config (progn
            (when (eq system-type 'windows-nt)
              (setq projectile-indexing-method 'alien))

            (setq projectile-keymap-prefix (kbd "C-c p")
                  projectile-completion-system 'helm
                  projectile-enable-caching t)

            ;; FIXME Why it executes Find Files after projectile-vc?
            (define-key projectile-mode-map (kbd "C-S-p") 'helm-projectile-switch-project)
            (projectile-global-mode)))
;; =============================================================

;; =============================================================
(use-package helm
  :ensure t
  :diminish helm-mode
  :init (progn
          (use-package helm-swoop :ensure t)
          (require 'helm-config)
          (require 'helm-misc)
          (require 'helm-locate)

          (setq helm-split-window-in-side-p           t
                helm-move-to-line-cycle-in-source     t
                helm-ff-search-library-in-sexp        t
                helm-ff-file-name-history-use-recentf t
                helm-ff-auto-update-initial-value     nil
                helm-quick-update                     t
                helm-yank-symbol-first                t
                helm-bookmark-show-location           t
                helm-recentf-fuzzy-match              t
                helm-buffers-fuzzy-matching           t
                helm-locate-fuzzy-match               t
                helm-M-x-fuzzy-match                  t
                helm-semantic-fuzzy-match             t
                helm-imenu-fuzzy-match                t
                helm-apropos-fuzzy-match              t
                helm-lisp-fuzzy-completion            t
                helm-scroll-amount                    8)

          ;; rebind tab to do persistent action
          (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
          ;; make TAB works in terminal
          (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
          ;; list actions using C-z
          (define-key helm-map (kbd "C-z") 'helm-select-action)
          (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)

          (when (executable-find "curl") (setq helm-google-suggest-use-curl-p t))

          ;; (global-set-key (kbd "M-x") 'helm-M-x)

          (autoload 'helm-descbinds      "helm-descbinds" t)
          (autoload 'helm-eshell-history "helm-eshell"    t)
          (autoload 'helm-esh-pcomplete  "helm-eshell"    t)

          (defun my-define-eshell-mode-map-keys ()
            (interactive)
            (define-key eshell-mode-map (kbd "TAB")     #'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history))

          (add-hook 'eshell-mode-hook 'my-define-eshell-mode-map-keys)

          (helm-mode t)
          (helm-adaptive-mode t)
          ;; (helm-autoresize-mode 1)

          (use-package helm-projectile
            :ensure t
            :config (progn (helm-projectile-on))))
  :bind (("C-c h"   . helm-command-prefix)
         ("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("M-y"     . helm-show-kill-ring)
         ("M-s o"   . helm-swoop)
         ("M-s /"   . helm-multi-swoop)
         ("C-x c!"  . helm-calcul-expression)
         ("C-x c:"  . helm-eval-expression-with-eldoc)))
;; =============================================================

;; =============================================================
(use-package neotree
  :ensure t
  :bind ("<f2>" . neotree-toggle)
  :config (progn
            (defun neotree-evil-keys ()
              (interactive)
              (define-key evil-normal-state-local-map (kbd "S-h") 'neotree-hidden-file-toggle)
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q")   'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))

            (setq neo-theme 'ascii)
            (add-hook 'neotree-mode-hook #'neotree-evil-keys)))
;; =============================================================

;; =============================================================
(use-package company
  :pin gnu
  :ensure t
  :config (progn
            (use-package company-quickhelp
              :ensure t
              :config (progn
                        (setq company-quickhelp-delay 0.5)
                        (company-quickhelp-mode 1))
              (require 'company-etags)
              (add-to-list 'company-etags-modes 'clojure-mode))

            (setq company-show-numbers t
                  company-minimum-prefix-length 2
                  company-require-match 'never
                  company-dabbrev-downcase nil
                  company-dabbrev-ignore-case t)

            (define-key company-active-map (kbd "ESC") 'company-abort)
            (add-hook 'after-init-hook 'global-company-mode)))
;; =============================================================

;; =============================================================
(use-package org
  :ensure t
  :defer  t
  :config (progn
            (let ((agenda-files (cl-remove-if-not 'file-exists-p '("~/org/todo.org"))))
              (setq org-agenda-files agenda-files))

            (use-package org-bullets
              :ensure t
              :config (progn
                        (defun org-bullets-on () (org-bullets-mode 1))
                        (add-hook 'org-mode-hook #'org-bullets-on)))

            (add-hook 'org-mode-hook #'linum-off)

            ;; Let's have pretty source code blocks
            (setq org-edit-src-content-indentation 0
                  org-src-tab-acts-natively t
                  org-src-fontify-natively t
                  org-src-preserve-indentation t
                  org-startup-indented t
                  org-confirm-babel-evaluate nil
                  org-log-done 'time
                  org-enforce-todo-dependencies t
                  org-enforce-todo-checkbox-dependencies t
                  org-catch-invisible-edits 'error
                  org-babel-clojure-backend 'cider)

            (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                                     (clojure    . t)
                                                                     (scheme     . t)
                                                                     (sql        . t))))
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb)
         ("<f12>" . org-agenda)))
;; =======================================================================

;; =======================================================================
(use-package evil
  :ensure t
  :init (progn
          (setq evil-default-cursor t)
          (use-package evil-leader
            :ensure t
            :init (global-evil-leader-mode)
            :config (progn
                      (setq evil-leader/in-all-states t)
                      (evil-leader/set-leader ",")
                      (evil-leader/set-key "SPC" 'lazy-highlight-cleanup)
                      (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)
                      (evil-leader/set-key "f"   'find-file-at-point)
                      (evil-leader/set-key "a"   'align-regexp)))
          (evil-mode 1))
  :config (progn
            ;; set evil-mode mode names
            (setq evil-normal-state-tag   (propertize " -- NORMAL --")
                  evil-emacs-state-tag    (propertize " -- EMACS --")
                  evil-insert-state-tag   (propertize " -- INSERT --")
                  evil-motion-state-tag   (propertize " -- MOTION --")
                  evil-visual-state-tag   (propertize " -- VISUAL --")
                  evil-operator-state-tag (propertize " -- OPEN --"))

            ;; Emacs keys in INSERT mode
            (setcdr evil-insert-state-map nil)
            (setq evil-move-cursor-back t)

            ;; :q command to just kill buffer, but do not close window
            (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
            (evil-ex-define-cmd "ls"     'ibuffer-list-buffers)

            ;; ESC quits
            (defun minibuffer-keyboard-quit ()
              "Abort recursive edit.
               In Delete Selection mode, if the mark is active, just deactivate it;
               then it takes a second \\[keyboard-quit] to abort the minibuffer."
              (interactive)
              (if (and delete-selection-mode transient-mark-mode mark-active)
                  (setq deactivate-mark  t)
                (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
                (abort-recursive-edit)))

            (global-set-key                             [escape]  'evil-exit-emacs-state)
            (define-key evil-normal-state-map           [escape]  'keyboard-quit)
            (define-key evil-visual-state-map           [escape]  'keyboard-quit)
            (define-key minibuffer-local-map            [escape]  'minibuffer-keyboard-quit)
            (define-key minibuffer-local-ns-map         [escape]  'minibuffer-keyboard-quit)
            (define-key minibuffer-local-completion-map [escape]  'minibuffer-keyboard-quit)
            (define-key minibuffer-local-must-match-map [escape]  'minibuffer-keyboard-quit)
            (define-key minibuffer-local-isearch-map    [escape]  'minibuffer-keyboard-quit)
            (define-key evil-insert-state-map           [escape]  'evil-normal-state)
            (define-key evil-normal-state-map           [next]    'evil-scroll-down)
            (define-key evil-normal-state-map           [prior]   'evil-scroll-up)
            (define-key evil-normal-state-map           (kbd "j") 'evil-next-visual-line)
            (define-key evil-normal-state-map           (kbd "k") 'evil-previous-visual-line)

            ;; Rotate windows
            (define-key evil-motion-state-map (kbd "C-w <left>")  'evil-window-rotate-downwards)
            (define-key evil-motion-state-map (kbd "C-w <down>")  'evil-window-rotate-downwards)
            (define-key evil-motion-state-map (kbd "C-w <up>")    'evil-window-rotate-upwards)
            (define-key evil-motion-state-map (kbd "C-w <right>") 'evil-window-rotate-upwards)

            (use-package evil-numbers :ensure t)
            (use-package evil-search-highlight-persist
              :ensure t
              :config (progn
                        (global-evil-search-highlight-persist t)))

            (use-package evil-nerd-commenter
              :ensure t
              :config (progn
                        (evilnc-default-hotkeys)
                        (defun comment-line-and-go-to-next ()
                          (interactive)
                          (evilnc-comment-or-uncomment-lines 1)
                          (evil-next-line))
                        (global-set-key (kbd "C-;") 'comment-line-and-go-to-next)
                        (global-set-key (kbd "C-/") 'comment-line-and-go-to-next)))
            (use-package evil-org :ensure t)))
;; =======================================================================

;; =======================================================================
(use-package magit
  :ensure t
  :defer  t
  :pin melpa-stable
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config (progn
            (use-package git-commit-mode :ensure t)
            (use-package git-rebase-mode :ensure t)
            (use-package git-gutter
              :ensure t
              :config (progn
                        (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
                        (global-set-key (kbd "C-x r")   'git-gutter:revert-hunk)
                        (global-set-key (kbd "C-x p")   'git-gutter:previous-hunk)
                        (global-set-key (kbd "C-x n")   'git-gutter:next-hunk)

                        (add-to-list 'git-gutter:update-hooks    'focus-in-hook)
                        (add-to-list 'git-gutter:update-commands 'other-window)

                        (custom-set-variables '(git-gutter:hide-gutter t))
                        (git-gutter:linum-setup)
                        (global-git-gutter-mode 1)))

            (when (eq system-type 'windows-nt)
                (progn
                  (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
                  (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" (getenv "PATH")))))

            (setq magit-diff-options '("-w")
                  magit-status-buffer-switch-function 'switch-to-buffer
                  magit-diff-refine-hunk t
                  ;; magit-process-popup-time 20
                  ediff-window-setup-function 'ediff-setup-windows-plain
                  ediff-split-window-function 'split-window-horizontally
                  ediff-diff-options "-w")

            ;; Vim-like movement between changes
            (defun ediff-vim-like-navigation ()
              (ediff-setup-keymap)
              (define-key ediff-mode-map "j" 'ediff-next-difference)
              (define-key ediff-mode-map "k" 'ediff-previous-difference))
            (add-hook 'ediff-mode-hook 'ediff-vim-like-navigation)
            ;; Restore previous windows state after Ediff quits
            (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
  :bind (("C-x g" . magit-status)))
;; =======================================================================

;; =======================================================================
(use-package flycheck
  :ensure t
  :config (progn
            (use-package flycheck-pos-tip
              :ensure t
              :config (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
            (add-hook 'after-init-hook #'global-flycheck-mode)))
;; =======================================================================

;; =======================================================================
(use-package undo-tree
  :ensure t
  :config (progn
            (global-undo-tree-mode)
            (setq undo-tree-visualizer-timestamps t
                  undo-tree-visualizer-diff       t)
            ;; disable some defaults
            (define-key undo-tree-map (kbd "C-/") nil)))
;; =======================================================================

;; =======================================================================
(use-package sauron
  :ensure t
  :config (progn
            (setq sauron-modules '(sauron-erc sauron-org sauron-notifications)
                  sauron-separate-frame nil
                  ;; sauron-hide-mode-line t
                  sauron-max-line-length 180
                  sauron-watch-nicks '("kovrik" "kovrik`" "kovrik``"))
            (sauron-start)))
;; =======================================================================

;; =======================================================================
;; TODO Ask for confirmation before close
(use-package eyebrowse
  :pin melpa-stable
  :ensure t
  :config (progn
            (eyebrowse-mode t)
            (eyebrowse-setup-evil-keys)
            (setq eyebrowse-new-workspace t)))
;; =======================================================================

;; =======================================================================
;; FIXME Hangs sometimes
(use-package shackle
  :ensure t
  :config (progn
            (setq shackle-lighter " |#|"
                  shackle-rules '(("\\`\\*magit.*?\\*\\'" :regexp t :same t)
                                  (erc-mode :same t)))
            (shackle-mode t)))
;; =======================================================================

;; =======================================================================
(use-package erc
  :ensure t
  :defer  t
  :config (progn
            (use-package erc-hl-nicks
              :ensure t
              :config (add-hook 'erc-mode-hook #'erc-hl-nicks-mode))

            (erc-autojoin-mode t)
            (erc-scrolltobottom-enable)
            (setq erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs"))
                  erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
                  erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                            "324"  "329"  "332"  "333"  "353" "477")
                  erc-server "irc.freenode.net"
                  erc-port 6667
                  erc-nick "kovrik"
                  erc-track-position-in-mode-line t
                  erc-input-line-position -2
                  erc-prompt-for-password nil
                  erc-header-line-face-method nil
                  erc-server-coding-system '(utf-8 . utf-8))))
;; =======================================================================

;; =======================================================================
;; Misc
(progn
  (defun my/hsplit-last-buffer (prefix)
    "Split the window horizontally and display the previous buffer.  Args: PREFIX."
    (interactive "p")
    (split-window-vertically)
    (other-window 1 nil)
    (when (= prefix 1) (switch-to-next-buffer)))

  (defun my/vsplit-last-buffer (prefix)
    "Split the window vertically and display the previous buffer.  Args: PREFIX."
    (interactive "p")
    (split-window-horizontally)
    (other-window 1 nil)
    (when (= prefix 1) (switch-to-next-buffer)))

  (bind-key "C-x 2"  #'my/hsplit-last-buffer)
  (bind-key "C-x -"  #'my/hsplit-last-buffer)
  (bind-key "C-x _"  #'my/hsplit-last-buffer)

  (bind-key "C-x 3"  #'my/vsplit-last-buffer)
  (bind-key "C-x \\" #'my/vsplit-last-buffer)
  (bind-key "C-x |"  #'my/vsplit-last-buffer))

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region.  You need to have `nxml-mode`.
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules.  Args: BEGIN END"
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end)))

(defun my-find-file ()
  "If currently in a project, then use Projectile to fuzzy find a file.
Use Helm otherwise."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-find-files-1 "./")))
(global-set-key (kbd "C-S-n") 'my-find-file)

;; =======================================================================
;; Colors stuff
(defun calc-relative-luminance (hex)
  "Calculate relative luminance by colour's HEX value."
  (let ((rgb           (color-name-to-rgb hex))
        (itu-r-bt-709 '(0.2126 0.7152 0.0722)))
    (if rgb
        (apply '+ (mapcar* '* rgb itu-r-bt-709))
      (error "Invalid HEX colour value: '%s'" hex))))

;; TODO Add xah-syntax-color-hex-off function
(defun xah-syntax-color-hex-on ()
  "Syntax color hex color spec such as #ffff11110000 or #ff1100 in current buffer."
  (interactive)
  (font-lock-add-keywords nil
                          '(("#\\([ABCDEFabcdef[:digit:]]\\{12\\}\\|[ABCDEFabcdef[:digit:]]\\{6\\}\\)"
                             (0 (put-text-property (match-beginning 0) (match-end 0)
                                                   'face (list :background (match-string-no-properties 0)
                                                               :foreground (if (> (calc-relative-luminance (match-string-no-properties 0)) 0.5)
                                                                               "#000000"
                                                                             "#ffffff")))))))
  (font-lock-fontify-buffer))
;; =======================================================================
(setq debug-on-error nil)
(provide 'init)
;;; init.el ends here
