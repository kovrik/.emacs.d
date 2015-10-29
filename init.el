;;; init.el --- kovrik's Emacs config
;;; Commentary:
;;; Code:
(setq debug-on-error t)
;; Prevent frequent GCs during init
(setq gc-cons-threshold 100000000)

(let ((file-name-handler-alist nil))

;; Package management
(require 'package)
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("melpa"        . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-pinned-packages '(queue . "gnu") t)

;; Make sure to have downloaded archive description
(or (file-directory-p (expand-file-name (concat package-user-dir "/archives")))
    (package-refresh-contents))

;; use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'diminish)
(require 'bind-key)
(require 'uniquify)
(setq use-package-verbose t
      load-prefer-newer t
      use-package-always-ensure t)
(use-package auto-compile :config (auto-compile-on-load-mode))

(defun my-ensure-packages-installed (packages)
  "Assure every package in PACKAGES is installed, ask for installation if it’s not.  Return a list of installed packages or nil for every skipped package."
  (dolist (p packages)
    (when (and (not (package-installed-p p))
               (y-or-n-p (format "Package %s is missing.  Install it? " p)))
      (package-install p))))

(my-ensure-packages-installed '(queue async browse-kill-ring dash epl f fold-dwim fringe-helper goto-chg highlight highlight-escape-sequences highlight-parentheses idle-highlight-mode markdown-mode pkg-info s))

(defun my-add-hooks (hooks function)
  (dolist (hook hooks)
    (add-hook hook function)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(package-selected-packages
   (quote
    (auto-compile browse-kill-ring cider clojure-mode-extra-font-locking company company-quickhelp dired+ elisp--witness--lisp erc-hl-nicks evil-leader evil-numbers evil-org evil-search-highlight-persist evil-surround expand-region f fixme-mode flycheck flycheck-clojure flycheck-pos-tip fold-dwim helm-projectile helm-swoop highlight-escape-sequences highlight-parentheses idle-highlight-mode ido-ubiquitous ido-vertical-mode magit markdown-mode racket-mode rainbow-delimiters smart-mode-line use-package))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;; '(default ((t (:weight normal :background "#fefefe" :foreground "black"))))
;; '(cursor  ((t (:background "purple"))))
;; '(diff-added     ((t (:foreground "#ffffff" :background "#c6efce"))))
;; '(diff-hl-insert ((t (:foreground "#c6efce" :background "#c6efce"))))
;; '(diff-changed   ((t (:foreground "#000000" :background "#ffeb9c"))))
;; '(diff-hl-change ((t (:foreground "#ffeb9c" :background "#ffeb9c"))))
;; '(diff-removed   ((t (:foreground "#ffffff" :background "#ffc7ce"))))
;; '(diff-hl-delete ((t (:foreground "#ffc7ce" :background "#ffc7ce"))))
;; '(erc-direct-msg-face ((t (:foreground "#e53935"))))
;; '(erc-input-face ((t (:foreground "dark green"))))
;; '(erc-timestamp-face  ((t (:foreground "#9966ca"))))
;; '(evil-search-highlight-persist-highlight-face ((t (:background "#ffdd00" :foreground "#000000"))))
;; '(eyebrowse-mode-line-active ((t (:foreground "#e53935" :weight bold))))
;; '(flycheck-fringe-error   ((t (:background "#e53935" :foreground "#e53935"))))
;; '(flycheck-fringe-info    ((t (:background "#43a047" :foreground "#43a047"))))
;; '(flycheck-fringe-warning ((t (:background "#ffc107" :foreground "#ffc107"))))
;; '(font-lock-constant-face ((t (:foreground "#0000df"))))
;; '(font-lock-keyword-face  ((t (:foreground "#8b008b"))))
;; '(font-lock-string-face   ((t (:foreground "#007442"))))
;; '(helm-selection ((t (:background "#bdeebd" :foreground "#000000"))))
;; '(helm-source-header ((t (:background "#607d8b" :foreground "#ffffff" :height 100))))
;; '(hl-line ((t (:background "#ccddee"))))
;; '(link ((t (:foreground "#bb00f8"))))
;; '(rainbow-delimiters-depth-1-face ((t (:foreground "#000000"))))
;; '(rainbow-delimiters-depth-2-face ((t (:foreground "#ff0000"))))
;; '(rainbow-delimiters-depth-3-face ((t (:foreground "#0000b8"))))
;; '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff8c00"))))
;; '(rainbow-delimiters-depth-5-face ((t (:foreground "#009800"))))
;; '(rainbow-delimiters-depth-6-face ((t (:foreground "#ff00ff")))))

(use-package zenburn-theme)
(custom-set-faces
 '(diff-added             ((t (:foreground "#ffffff" :background "#c6efce"))))
 '(diff-changed           ((t (:foreground "#000000" :background "#ffeb9c"))))
 '(diff-removed           ((t (:foreground "#ffffff" :background "#ffc7ce"))))
 '(diff-hl-insert         ((t (:foreground "#c6efce" :background "#c6efce"))))
 '(diff-hl-change         ((t (:foreground "#ffeb9c" :background "#ffeb9c"))))
 '(diff-hl-delete         ((t (:foreground "#ffc7ce" :background "#ffc7ce"))))
 '(font-lock-string-face  ((t (:foreground "#ffb7b7"))))
 '(font-lock-warning-face ((t (:foreground "#ff9090"))))
 '(magit-hash             ((t (:foreground "#ffe0a0"))))
 '(magit-popup-key        ((t (:foreground "#c6efce"))))
 '(org-done               ((t (:foreground "#c6efce"))))
 '(org-link               ((t (:foreground "#ffe0a0" :underline nil))))
 '(org-todo               ((t (:foreground "#ffb7b7"))))
 '(region                 ((t (:background "#ffe5b0" :foreground "black")))))

;; PATH
(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns))
            (exec-path-from-shell-initialize)))

(when (eq 'windows-nt system-type)
  (setq exec-path (append (parse-colon-path (getenv "PATH"))
                          (parse-colon-path (getenv "USERPROFILE")) exec-path)))

;; Globals
(prefer-coding-system 'utf-8)
(when window-system
  (tooltip-mode    -1)
  (tool-bar-mode   -1)
  (menu-bar-mode   -1)
  (scroll-bar-mode -1))

(setq-default indent-tabs-mode nil
              tab-width 2)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      sentence-end-double-space nil
      ring-bell-function 'ignore
      use-dialog-box nil
      visible-bell nil
      uniquify-buffer-name-style 'forward
      show-trailing-whitespace t
      ns-use-srgb-colorspace nil)

(fringe-mode '(4 . 0))
(column-number-mode)
(desktop-save-mode)
(global-font-lock-mode)
(global-hl-line-mode)
(winner-mode)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(bind-keys ([escape]   . keyboard-quit)
           ("RET"      . newline-and-indent)
           ("C-c r"    . revert-buffer)
           ("C-c n"    . narrow-to-region)
           ("C-c w"    . widen)
           ("<M-up>"   . backward-page)
           ("<M-down>" . forward-page))

(defun my-nlinum-on  () "Turn 'nlinum-mode' on."  (nlinum-mode  1))
(defun my-nlinum-off () "Turn 'nlinum-mode' off." (nlinum-mode -1))

;; Fonts
(let ((my-font (cl-find-if (lambda (f) (and f (member (font-get f :name)
                                                      (font-family-list))))
                           (list
                            (font-spec :name "Meslo LG S" :size 12)
                            (font-spec :name "Consolas"   :size 13)
                            (font-spec :name "Monaco"     :size 12)))))
  (when my-font
    (message (format "Using %s %s font." (font-get my-font :name) (font-get my-font :size)))
    (set-face-attribute 'default nil :font my-font)
    (set-frame-font      my-font  nil t)))

;; One-line packages
(use-package nlinum :defer t)
(use-package bug-hunter :defer t)
(use-package command-log-mode :defer t)
(use-package restclient :defer t)
(use-package iedit :defer t)
(use-package rainbow-mode :defer t :diminish rainbow-mode)
(use-package focus :defer t)
(use-package color-theme :defer t :config (color-theme-initialize))
(use-package fixme-mode :config (fixme-mode t))
(use-package diff-hl :config (global-diff-hl-mode t))
(use-package page-break-lines :config (global-page-break-lines-mode))

(use-package find-func
  :bind (("C-S-h" . find-function-at-point)
         ("C-h f" . find-function)
         ("C-h k" . find-function-on-key)
         ("C-h v" . find-variable)
         ("C-h l" . find-library)))

(use-package spaceline
  :config (progn
            (require 'spaceline-config)
            (setq powerline-height 14
                  powerline-default-separator 'bar
                  spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
            (set-face-attribute 'powerline-active1   nil :foreground "white")
            (set-face-attribute 'powerline-inactive1 nil :foreground "white")
            (spaceline-spacemacs-theme)))

;; TODO Configure and try
;; (use-package hydra)
;; TODO Configure and try
(use-package smartparens
  :config (progn
            (require 'smartparens-config)
            (use-package evil-smartparens :config (evil-smartparens-mode))
            (show-smartparens-global-mode t)
            (smartparens-strict-mode)
            (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)))

(use-package clojure-mode
  :defer  t
  :pin melpa-stable
  :config (progn
            (use-package cider :pin melpa :defer t)
            (use-package clojure-mode-extra-font-locking)
            (use-package flycheck-clojure
              :pin melpa
              :config (progn
                        (flycheck-clojure-setup)
                        (setq flycheck-checkers (delete 'clojure-cider-typed flycheck-checkers))))

            (add-hook 'clojure-mode-hook 'flycheck-mode)
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'cider-mode-hook   'cider-turn-on-eldoc-mode)

            (setq nrepl-log-messages           t
                  nrepl-hide-special-buffers   t
                  cider-prefer-local-resources t
                  cider-repl-popup-stacktraces t
                  cider-popup-stacktraces      nil)
            (defun cider-find-var-no-prompt ()
              "cider-find-var at point without prompt"
              (interactive)
              (cider-find-var t nil))
            (bind-keys :map clojure-mode-map ((kbd "C-h")   . cider-find-var-no-prompt)
                                             ((kbd "C-M-x") . cider-eval-defun-at-point))))

(use-package geiser :defer t)

(use-package racket-mode :defer t :config (add-hook 'racket-mode-hook #'company-quickhelp--disable))

(use-package eval-in-repl
  :defer t
  :config (progn
            ;; Elisp
            (require 'eval-in-repl-ielm)
            (define-key emacs-lisp-mode-map       (kbd "<C-return>") 'eir-eval-in-ielm)
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
            (define-key info-mode-map             (kbd "<C-return>") 'eir-eval-in-ielm)

            ;; Clojure
            ;; (require 'cider) ; if not done elsewhere
            (require 'eval-in-repl-cider)
            (define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)

            ;; Geiser support (for Racket and Guile Scheme)
            ;; When using this, turn off racket-mode and scheme supports
            ;; (require 'eval-in-repl-geiser)
            ;; (add-hook 'geiser-mode-hook
                      ;; '(lambda () (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser)))
            (require 'racket-mode)
            (require 'eval-in-repl-racket)
            (define-key racket-mode-map (kbd "<C-return>") 'eir-eval-in-racket)

            ;; Scheme support (if not using Geiser))
            ;; (require 'eval-in-repl-scheme)
            ;; (add-hook 'scheme-mode-hook
            ;;           '(lambda () (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme)))
))

(use-package rainbow-delimiters
  :config (progn
            (setq rainbow-delimiters-max-face-count 9
                  rainbow-delimiters-outermost-only-face-count 8)
            (my-add-hooks '(emacs-lisp-mode-hook
                            clojure-mode-hook
                            lisp-mode-hook
                            scheme-mode-hook
                            racket-mode-hook)
                          #'rainbow-delimiters-mode)))

(use-package evil
  :init (progn
          (use-package evil-leader
            :init (global-evil-leader-mode)
            :config (progn
                      (setq evil-leader/in-all-states t)
                      (evil-leader/set-leader ",")
                      (evil-leader/set-key "SPC" 'lazy-highlight-cleanup)
                      (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)
                      (evil-leader/set-key "f"   'find-file-at-point)
                      (evil-leader/set-key "a"   'align-regexp)
                      (evil-leader/set-key "s"   'delete-trailing-whitespace)
                      (evil-leader-mode t))))
  :config (progn
            (use-package evil-org :defer t)
            (use-package evil-numbers)
            (use-package evil-anzu)
            (use-package evil-surround :config (global-evil-surround-mode 1))
            (use-package evil-visualstar :config (global-evil-visualstar-mode))
            (use-package evil-search-highlight-persist
              :config (global-evil-search-highlight-persist t))
            (use-package evil-matchit
              :config (my-add-hooks '(nxml-mode-hook
                                      html-mode-hook
                                      web-mode-hook) #'evil-matchit-mode))
            (use-package evil-commentary
              :config (progn
                        (defun my-comment-line-and-go-to-next ()
                          "Comment current line and go to next."
                          (interactive)
                          (evil-commentary-line (line-beginning-position)
                                                (line-end-position)
                                                'line)
                          (evil-next-line))
                        (evil-commentary-mode))
              :bind (("C-/" . my-comment-line-and-go-to-next)))

            ;; Emacs keys in INSERT mode
            (setcdr evil-insert-state-map nil)
            (setq evil-move-cursor-back t
                  evil-default-cursor   t)

            ;; kill buffer, but don't close window
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
                (when (get-buffer "*Completions*")
                  (delete-windows-on "*Completions*"))
                (abort-recursive-edit)))

            (global-set-key                             [escape] 'evil-exit-emacs-state)
            (define-key evil-visual-state-map           [escape] 'keyboard-quit)
            (define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)
            (define-key evil-insert-state-map           [escape] 'evil-normal-state)
            (bind-keys :map evil-normal-state-map ([next]   . evil-scroll-down)
                                                  ([prior]  . evil-scroll-up)
                                                  ([escape] . keyboard-quit)
                                                  ("j"      . evil-next-visual-line)
                                                  ("k"      . evil-previous-visual-line))
            (bind-keys :map evil-motion-state-map
                       ("C-w m" . maximize-window)
                       ("C-w u" . winner-undo))
            (my-add-hooks '(help-mode-hook
                            prog-mode-hook
                            text-mode-hook) #'evil-local-mode)
            (defun my-evil-off ()
              "Turn 'evil-mode' off and change cursor type to bar."
              (interactive)
              (turn-off-evil-mode)
              (setq cursor-type 'bar))

            ;; Disable evil-mode in some major modes
            (my-add-hooks '(shell-mode-hook  term-mode-hook
                            magit-mode-hook  erc-mode-hook
                            eshell-mode-hook comint-mode-hook
                            proced-mode-hook nrepl-connected-hook)
                          #'my-evil-off)))

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init (my-add-hooks '(emacs-lisp-mode-hook
                        lisp-interaction-mode-hook
                        ielm-mode-hook)
                      #'turn-on-eldoc-mode))

(use-package helm
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (require 'helm-misc)
          (require 'helm-locate)
          (use-package flx)
          (use-package helm-flx :config (helm-flx-mode +1))
          (use-package helm-swoop :defer t)

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
                helm-scroll-amount                    8
                helm-echo-input-in-header-line        t)

          (autoload 'helm-descbinds      "helm-descbinds" t)
          (autoload 'helm-eshell-history "helm-eshell"    t)
          (autoload 'helm-esh-pcomplete  "helm-eshell"    t)
          (helm-mode t)
          (helm-adaptive-mode t)

          ;; Do not auto-expand '~/', '//', './', '../' in helm-find-files
          (remove-hook 'helm-after-update-hook 'helm-ff-auto-expand-to-home-or-root)

          (bind-keys :map helm-map ("<tab>"  . helm-execute-persistent-action)
                                   ("C-i"    . helm-execute-persistent-action)
                                   ("C-z"    . helm-select-action)
                                   ([escape] . helm-keyboard-quit)))
  :bind (("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("M-s o"   . helm-swoop)
         ("M-s /"   . helm-multi-swoop)
         ("C-c h"   . helm-command-prefix)
         ("C-x b"   . helm-mini)
         ("C-x f"   . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x c!"  . helm-calcul-expression)
         ("C-x c:"  . helm-eval-expression-with-eldoc)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)))

(use-package projectile
  :diminish projectile-mode
  :config (progn
            (use-package helm-projectile :config (helm-projectile-on))
            (defun my-projectile-switch-to-project ()
              "My switch-to-project action for projectile.
If project is a git-project, then run magit-status.
Otherwise run projectile-find-file."
              (interactive)
              (let ((pr (projectile-project-root))
                    (git-projects (cl-remove-if
                                   (lambda (p) (or (file-remote-p p)
                                                   (not (file-directory-p (concat p "/.git/")))))
                                   (mapcar 'expand-file-name projectile-known-projects))))
                (if (member pr git-projects)
                    (magit-status pr)
                  (projectile-find-file))))

            (setq projectile-keymap-prefix (kbd "C-c p")
                  projectile-completion-system 'helm
                  projectile-enable-caching t
                  projectile-switch-project-action 'my-projectile-switch-to-project)

            (when (eq system-type 'windows-nt)
              (setq projectile-indexing-method 'alien
                    projectile-enable-caching  nil))
            (projectile-global-mode))
  :bind (("C-S-p" . projectile-switch-project)))

(use-package company
  :pin gnu
  :diminish company-mode
  :config (progn
            (use-package company-quickhelp
              :config (progn (setq company-quickhelp-delay 0.7)
                             (company-quickhelp-mode 1)))
            (use-package company-flx
              :config (with-eval-after-load 'company
                        (company-flx-mode +1)))
            (setq company-show-numbers t
                  company-minimum-prefix-length 2
                  company-require-match 'never
                  company-dabbrev-downcase nil
                  company-dabbrev-ignore-case t)
            (add-hook 'after-init-hook 'global-company-mode)))

(use-package org
  :defer t
  :config (progn
            (use-package org-bullets
              :config (progn
                        (defun my-org-bullets-on () (org-bullets-mode 1))
                        (add-hook 'org-mode-hook #'my-org-bullets-on)))
            (add-hook 'org-mode-hook #'my-nlinum-off)
            ;; FIXME indentation in SRC blocks
            (setq org-edit-src-content-indentation 0
                  org-src-preserve-indentation nil
                  org-src-tab-acts-natively t
                  org-src-fontify-natively t
                  org-src-preserve-indentation t
                  org-startup-indented t
                  org-confirm-babel-evaluate nil
                  org-log-done 'time
                  org-enforce-todo-dependencies t
                  org-enforce-todo-checkbox-dependencies t
                  org-catch-invisible-edits 'error
                  org-babel-clojure-backend 'cider
                  org-agenda-files (cl-remove-if-not 'file-exists-p '("~/org/todo.org")))
            (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                                     (java       . t)
                                                                     (clojure    . t)
                                                                     (scheme     . t)
                                                                     (sql        . t))))
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb)
         ("<f12>" . org-agenda)))

(use-package magit
  :defer t
  :pin melpa-stable
  :init   (setq magit-last-seen-setup-instructions "1.4.0")
  :config (progn
            (use-package magit-popup)
            (setenv "GIT_ASKPASS" "git-gui--askpass")
            ;; Don't want to view changes every time before commit
            (setq magit-diff-auto-show (delete 'commit magit-diff-auto-show))
            (setq magit-status-buffer-switch-function 'switch-to-buffer
                  magit-diff-options '("-w")
                  magit-diff-refine-hunk t
                  magit-log-arguments '("--decorate" "--graph" "--color" "-n80")
                  magit-log-cutoff-length 80)
            ;; FIX Don't know why some of these become unbound sometimes
            (bind-keys :map magit-mode-map ("<tab>"     . magit-section-cycle)
                                           ("<backtab>" . magit-section-cycle-global)))
  :bind (("C-x g" . magit-status)))

(use-package ediff
  :defer t
  :config (progn
            (setq ediff-window-setup-function 'ediff-setup-windows-plain
                  ediff-split-window-function 'split-window-horizontally
                  ediff-diff-options          "-w")
            ;; Restore window layout
            (defun my-toggle-ediff-wide-display ()
              "Turn off wide-display mode (if was enabled) before quitting ediff."
              (when ediff-wide-display-p
                (ediff-toggle-wide-display)))
            (add-hook 'ediff-cleanup-hook 'my-toggle-ediff-wide-display)
            (add-hook 'ediff-quit-hook    'winner-undo)))

(use-package eshell
  :defer t
  :config (setq eshell-save-history-on-exit t
                eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"))

(use-package web-mode
  :defer t
  :pin melpa-stable
  :config (progn (add-to-list 'auto-mode-alist '("\\.html?\\'"   . web-mode))
                 (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))))

(use-package flycheck
  :defer t
  :config (progn
            (use-package flycheck-pos-tip
              :config (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (progn
            (global-undo-tree-mode)
            (setq undo-tree-visualizer-timestamps t
                  undo-tree-visualizer-diff       t)
            (define-key undo-tree-map (kbd "C-/") nil)))

(use-package eyebrowse
  :pin melpa-stable
  :config (progn
            (eyebrowse-mode t)
            (eyebrowse-setup-evil-keys)
            (require 'evil-commentary)
            (define-key evil-motion-state-map (kbd "gc") 'evil-commentary)
            (setq eyebrowse-new-workspace t
                  eyebrowse-close-window-config-prompt t)))

(use-package shackle
  :config (progn
            (setq shackle-lighter " |#|"
                  shackle-rules '(("\\`\\*magit.*?\\'"   :regexp t :same t)
                                  ("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)
                                  (compilation-mode      :ignore t)
                                  (sauron-mode           :ignore t)
                                  (erc-mode              :same   t)
                                  (proced-mode           :same   t)
                                  (help-mode             :same   t)
                                  (ibuffer-mode          :same   t)))
            (shackle-mode t)))

(use-package sauron
  :defer t
  :config (progn
            (setq sauron-modules '(sauron-erc sauron-org sauron-notifications)
                  sauron-separate-frame nil
                  sauron-max-line-length 180
                  sauron-watch-nicks '("kovrik" "kovrik`" "kovrik``"))
            (sauron-start)))

(use-package erc
  :defer t
  :config (progn
            (use-package erc-hl-nicks :config (add-hook 'erc-mode-hook #'erc-hl-nicks-mode))
            (erc-autojoin-mode t)
            (erc-scrolltobottom-enable)
            (erc-scrolltobottom-mode t)
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
                  erc-server-coding-system '(utf-8 . utf-8)
                  erc-prompt ">"
                  erc-accidental-paste-threshold-seconds 0.5)))

;; Misc
(progn
  (defun my-hsplit-last-buffer (prefix)
    "Split the window horizontally and display the previous buffer.  Args: PREFIX."
    (interactive "p")
    (split-window-vertically)
    (other-window 1 nil)
    (when (= prefix 1) (switch-to-next-buffer)))

  (defun my-vsplit-last-buffer (prefix)
    "Split the window vertically and display the previous buffer.  Args: PREFIX."
    (interactive "p")
    (split-window-horizontally)
    (other-window 1 nil)
    (when (= prefix 1) (switch-to-next-buffer)))

  (bind-keys ("C-x 2"  . my-hsplit-last-buffer) ("C-x 3"  . my-vsplit-last-buffer)
             ("C-x -"  . my-hsplit-last-buffer) ("C-x \\" . my-vsplit-last-buffer)
             ("C-x _"  . my-hsplit-last-buffer) ("C-x |"  . my-vsplit-last-buffer)))

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region.  You need to have `nxml-mode`.
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules.  Args: BEGIN END"
  (interactive "r")
  (save-excursion (nxml-mode)
                  (goto-char begin)
                  (while (search-forward-regexp "\>[ \\t]*\<" nil t)
                    (backward-char)
                    (insert "\n")
                    (setq end (1+ end)))
                  (indent-region begin end)))

(defun my-find-file ()
  "If currently in a project, then use Projectile to fuzzy find a file.
Use Helm otherwise."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-find-files-1 (expand-file-name "./"))))
(bind-key "C-S-n" 'my-find-file)

(defun my-kill-buffers (&rest args)
  "Kill buffers."
  (defun my-kill-buffer-by-p (pred)
    (let ((matched-buffers (cl-remove-if-not pred (buffer-list))))
      (when (y-or-n-p (format "Kill %s buffers?" (length matched-buffers)))
        (mapc 'kill-buffer matched-buffers)
        (message (format "Killed %s buffer(s)." (length matched-buffers))))))
  (let ((regex (plist-get args :regex))
        (mode  (plist-get args :mode)))
    (my-kill-buffer-by-p (cond
                           (regex (lambda (b) (string-match regex (buffer-name b))))
                           (mode  (lambda (b) (eq mode (buffer-local-value 'major-mode b))))))))

(defun my-kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (my-kill-buffers :mode 'dired-mode))

(defun my-kill-all-ediff-buffers ()
  "Kill all ediff buffers."
  (interactive)
  (my-kill-buffers :regex "^*ediff-.*\\*$"))

(defun my-align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression.  Args: START END REGEXP."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1 t))
)
;; Bring back to default value
(setq gc-cons-threshold 800000)
(setq debug-on-error nil)
(provide 'init)
;;; init.el ends here
