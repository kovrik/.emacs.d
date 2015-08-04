;;; init.el --- kovrik's Emacs config
;;; Commentary:
;;; Code:
(setq debug-on-error t)
;; =========================================================================
;; Package management
(require 'package)
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("melpa"        . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)
(add-to-list 'package-pinned-packages '(queue . "gnu") t)

;; Make sure to have downloaded archive description
(or (file-directory-p (expand-file-name (concat package-user-dir "/archives")))
    (package-refresh-contents))
;; =========================================================================

;; =========================================================================
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
;; =========================================================================

;; =========================================================================
(defun my-ensure-packages-installed (packages)
  "Assure every package in PACKAGES is installed, ask for installation if it’s not.  Return a list of installed packages or nil for every skipped package."
  (dolist (p packages)
    (when (and (not (package-installed-p p))
               (y-or-n-p (format "Package %s is missing.  Install it? " p)))
      (package-install p))))

(my-ensure-packages-installed '(queue async autopair browse-kill-ring dash epl f fold-dwim fringe-helper goto-chg highlight highlight-escape-sequences highlight-parentheses idle-highlight-mode markdown-mode pkg-info s))

;; Themes
(my-ensure-packages-installed '(ample-theme flatland-theme darcula-theme leuven-theme material-theme minimal-theme noctilux-theme soft-stone-theme solarized-theme sublime-themes twilight-bright-theme zenburn-theme))
;; =========================================================================

;; =========================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "52706f54fd3e769a0895d1786796450081b994378901d9c3fb032d3094788337" "8f2e60e25bd33a29f45867d99c49afd9d7f3f3ed8a60926d32d5a23c790de240" "118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(linum-format " %5i ")
 '(package-selected-packages
   (quote
    (corral ample-theme auto-compile autopair browse-kill-ring cider clojure-mode-extra-font-locking color-theme-solarized company company-quickhelp darcula-theme dired+ elisp--witness--lisp erc-hl-nicks evil-leader evil-numbers evil-org evil-search-highlight-persist evil-surround expand-region f fixme-mode flycheck flycheck-clojure flycheck-pos-tip fold-dwim helm-projectile helm-swoop highlight-escape-sequences highlight-parentheses idle-highlight-mode ido-ubiquitous ido-vertical-mode leuven-theme magit markdown-mode material-theme neotree noctilux-theme racket-mode rainbow-delimiters smart-mode-line soft-stone-theme solarized-theme sublime-themes use-package zenburn-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal))))
 '(cursor ((t (:background "purple"))))
 '(diff-added ((t (:foreground "#ffffff" :background "#43a047"))))
 '(diff-changed ((t (:foreground "#000000" :background "#ffc107"))))
 '(diff-removed ((t (:foreground "#ffffff" :background "#e53935"))))
 '(erc-direct-msg-face ((t (:foreground "#e53935"))))
 '(erc-input-face ((t (:foreground "dark green"))))
 '(erc-timestamp-face ((t (:foreground "#9966ca"))))
 '(evil-search-highlight-persist-highlight-face ((t (:background "#ffdd00" :foreground "#000000"))))
 '(flycheck-fringe-error ((t (:background "#e53935" :foreground "#e53935"))))
 '(flycheck-fringe-info ((t (:background "#43a047" :foreground "#43a047"))))
 '(flycheck-fringe-warning ((t (:background "#ffc107" :foreground "#ffc107"))))
 '(font-lock-keyword-face ((t (:foreground "#8B008B"))))
 '(font-lock-string-face ((t (:foreground "dark green"))))
 '(helm-selection ((t (:background "#bdeebd" :foreground "#000000"))))
 '(helm-source-header ((t (:background "#607d8b" :foreground "#ffffff" :height 100))))
 '(hl-line ((t (:background "#ccddee"))))
 '(link ((t (:foreground "#bb00f8"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#000000"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#ff0000"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#0000b8"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff8c00"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#009800"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#ff00ff")))))
;; =========================================================================
;; PATH
(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns))
            (exec-path-from-shell-initialize)))

(when (eq 'windows-nt system-type)
  (setq exec-path (append (parse-colon-path (getenv "PATH"))
                          (parse-colon-path (getenv "USERPROFILE")) exec-path)))
;; =========================================================================

;; =========================================================================
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
      show-trailing-whitespace t)

(fringe-mode '(4 . 0))
(autopair-global-mode)
(diminish 'autopair-mode)
(show-paren-mode)
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

(bind-keys ([escape] . keyboard-quit)
           ("RET"    . newline-and-indent)
           ("C-h"    . find-function-at-point)
           ("C-c r"  . revert-buffer)
           ("C-c n"  . narrow-to-region)
           ("C-c w"  . widen))

(defun my-linum-on  () "Turn 'linum-mode' on."  (linum-mode  1))
(defun my-linum-off () "Turn 'linum-mode' off." (linum-mode -1))
;; =========================================================================
;; Fonts
(defun my-font-exists-p (f)
  "Return t if font F (font-spec with :name key) exists."
  (and f (member (font-get f :name) (font-family-list))))

(let ((my-font (cl-find-if 'my-font-exists-p
                           (list (font-spec :name "Meslo LG S"      :size 13)
                                 (and (eq system-type 'windows-nt)
                                      (font-spec :name "Consolas"   :size 14))
                                 (font-spec :name "Monaco"          :size 13)
                                 (font-spec :name "Source Code Pro" :size 13)))))
  (when my-font
    (message (format "Using %s %s font." (font-get my-font :name) (font-get my-font :size)))
    (set-face-attribute 'default nil :font my-font)
    (set-frame-font      my-font  nil t)))
;; =========================================================================

;; =========================================================================
;; Packages without config
(use-package bug-hunter)
;; (use-package eval-sexp-fu)
(use-package command-log-mode)
(use-package restclient)
(use-package iedit)
(use-package rainbow-mode)
;; =========================================================================

;; =========================================================================
(use-package color-theme :config (color-theme-initialize))
;; =========================================================================

;; =========================================================================
(use-package fixme-mode :config (fixme-mode t))
;; =======================================================================

;; =========================================================================
(use-package tramp
  :config (when  (eq window-system 'w32)
            (setq tramp-default-method "scpx")))
;; =========================================================================

;; =========================================================================
;; TODO Configure
(use-package ranger :init (require 'dired))
;; =========================================================================

;; =========================================================================
(use-package smart-mode-line
  :config (progn (setq sml/theme 'light)
                 (sml/setup)))
;; =========================================================================

;; =========================================================================
(use-package linum
  :config (progn
            (setq linum-format (if (display-graphic-p) " %3d" "%4d "))
            (add-hook 'prog-mode-hook #'my-linum-on)
            (add-hook 'text-mode-hook #'my-linum-on)))
;; =========================================================================

;; =========================================================================
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))
;; =========================================================================

;; =========================================================================
(use-package corral
  :config (progn
            (setq corral-preserve-point t)
            (bind-keys ("M-\"" . corral-double-quotes-backward)
                       ("M-9"  . corral-parentheses-backward)
                       ("M-0"  . corral-parentheses-forward)
                       ("M-["  . corral-brackets-backward)
                       ("M-]"  . corral-brackets-forward)
                       ("M-{"  . corral-braces-backward)
                       ("M-}"  . corral-braces-forward))))
;; =========================================================================

;; =========================================================================
(use-package rainbow-delimiters
  :config (progn
            (setq rainbow-delimiters-max-face-count 9
                  rainbow-delimiters-outermost-only-face-count 8)
            (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
            (add-hook 'clojure-mode-hook    #'rainbow-delimiters-mode)
            (add-hook 'lisp-mode-hook       #'rainbow-delimiters-mode)
            (add-hook 'scheme-mode-hook     #'rainbow-delimiters-mode)
            (add-hook 'racket-mode-hook     #'rainbow-delimiters-mode)))
;; =========================================================================

;; =========================================================================
(use-package eval-in-repl
  :config (progn
            ;; Elisp
            (require 'eval-in-repl-ielm)
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
            (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)

            ;; Clojure
            ;; (require 'cider) ; if not done elsewhere
            (require 'eval-in-repl-cider)
            (define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)

            ;; Common Lisp
            ;; (require 'slime) ; if not done elsewhere
            ;; (require 'eval-in-repl-slime)
            ;; (add-hook 'lisp-mode-hook
            ;;           '(lambda ()
            ;;              (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))

            ;; Geiser support (for Racket and Guile Scheme)
            ;; When using this, turn off racket-mode and scheme supports
            ;; (require 'geiser) ; if not done elsewhere
            ;; (require 'eval-in-repl-geiser)
            ;; (add-hook 'geiser-mode-hook
                      ;; '(lambda ()
                         ;; (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser)))
            ;; racket-mode support (for Racket; if not using Geiser)
            (require 'racket-mode) ; if not done elsewhere
            (require 'eval-in-repl-racket)
            (define-key racket-mode-map (kbd "<C-return>") 'eir-eval-in-racket)

            ;; Scheme support (if not using Geiser))
            ;; (require 'scheme)    ; if not done elsewhere
            ;; (require 'cmuscheme) ; if not done elsewhere
            ;; (require 'eval-in-repl-scheme)
            ;; (add-hook 'scheme-mode-hook
            ;;    '(lambda ()
;;       (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme)))
))
;; =========================================================================

;; =========================================================================
(use-package clojure-mode
  :defer  t
  :config (progn
            (use-package cider :pin melpa)
            (use-package clojure-mode-extra-font-locking)
            (use-package flycheck-clojure :pin melpa :config (flycheck-clojure-setup))

            (add-hook 'clojure-mode-hook    'flycheck-mode)
            (add-hook 'clojure-mode-hook    'turn-on-eldoc-mode)
            (add-hook 'cider-mode-hook      'cider-turn-on-eldoc-mode)
            (add-hook 'cider-mode-hook      'company-mode)
            (add-hook 'cider-repl-mode-hook 'company-mode)

            (setq nrepl-log-messages           t
                  nrepl-hide-special-buffers   t
                  cider-prefer-local-resources t
                  cider-repl-popup-stacktraces t
                  cider-popup-stacktraces      nil)

            (defun my-cider-find-var-no-prompt ()
              "cider-find-var at point without prompt"
              (interactive)
              (cider-find-var t nil))
            (bind-keys :map clojure-mode-map ("C-h"   . my-cider-find-var-no-prompt)
                                             ("C-M-x" . cider-eval-defun-at-point))))
;; ========================================================================

;; ========================================================================
;; FIXME
(use-package geiser)
;; ========================================================================

;; ========================================================================
(use-package racket-mode
  :defer t
  :config (add-hook 'racket-mode-hook #'company-quickhelp--disable))
;; ========================================================================

;; ========================================================================
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
            (use-package evil-org)
            (use-package evil-numbers)
            (use-package evil-anzu)
            (use-package evil-surround :config (global-evil-surround-mode 1))
            (use-package evil-search-highlight-persist
              :config (global-evil-search-highlight-persist t))
            (use-package evil-matchit
              :config (progn
                        (add-hook 'nxml-mode-hook 'evil-matchit-mode)
                        (add-hook 'html-mode-hook 'evil-matchit-mode)
                        (add-hook 'web-mode-hook  'evil-matchit-mode)))
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
            (bind-keys :map evil-motion-state-map ("C-w m" . maximize-window)
                       ("C-w u" . winner-undo))

            (add-hook 'help-mode-hook #'evil-local-mode)
            (add-hook 'prog-mode-hook #'evil-local-mode)
            (add-hook 'text-mode-hook #'evil-local-mode)

            (defun my-evil-off ()
              "Turn 'evil-mode' off and change cursor type to bar."
              (interactive)
              (turn-off-evil-mode)
              (setq cursor-type 'bar))

            ;; Disable evil-mode in some major modes
            (dolist (mode-hook '(shell-mode-hook  term-mode-hook
                                 magit-mode-hook  erc-mode-hook
                                 eshell-mode-hook comint-mode-hook
                                 proced-mode-hook nrepl-connected-hook))
              (add-hook mode-hook 'my-evil-off))))
;; ========================================================================

;; ========================================================================
(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)))
;; ========================================================================

;; ========================================================================
(use-package helm
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (require 'helm-misc)
          (require 'helm-locate)
          (use-package helm-swoop)

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
;; ========================================================================

;; ========================================================================
(use-package projectile
  :defer t
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
;; ========================================================================

;; ========================================================================
(use-package company
  :pin gnu
  :diminish company-mode
  :config (progn
            (use-package company-quickhelp
              :config (progn (setq company-quickhelp-delay 0.7)
                             (company-quickhelp-mode 1)))
            (setq company-show-numbers t
                  company-minimum-prefix-length 2
                  company-require-match 'never
                  company-dabbrev-downcase nil
                  company-dabbrev-ignore-case t)
            (define-key company-active-map (kbd "ESC") 'company-abort)
            (add-hook 'after-init-hook 'global-company-mode)))
;; ========================================================================

;; ========================================================================
(use-package org
  :defer t
  :config (progn
            (use-package org-bullets
              :config (progn
                        (defun my-org-bullets-on () (org-bullets-mode 1))
                        (add-hook 'org-mode-hook #'my-org-bullets-on)))

            (add-hook 'org-mode-hook #'my-linum-off)
            ;; FIXME indentation in SRC blocks
            (setq org-edit-src-content-indentation 2
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
;; ========================================================================

;; ========================================================================
(use-package magit
  :defer t
  :pin melpa-stable
  :init   (setq magit-last-seen-setup-instructions "1.4.0")
  :config (progn
            (setq magit-diff-options '("-w")
                  magit-status-buffer-switch-function 'switch-to-buffer
                  magit-diff-refine-hunk t)

            ;; FIX Don't know why these become unbound sometimes
            (bind-keys :map magit-mode-map ;;("s"       . magit-stage-item)
                                           ;;("u"       . magit-unstage-item)
                                           ("<tab>"     . magit-section-cycle)
                                           ("<backtab>" . magit-section-cycle-global)))
  :bind (("C-x g" . magit-status)))
;; ========================================================================

;; ========================================================================
(use-package ediff
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
;; ========================================================================

;; =========================================================================
(use-package eshell
  :config (setq eshell-save-history-on-exit t
                eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"))
;; =========================================================================

;; ========================================================================
(use-package web-mode
  :pin melpa-stable
  :config (progn (add-to-list 'auto-mode-alist '("\\.html?\\'"   . web-mode))
                 (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))))
;; ========================================================================

;; ========================================================================
(use-package flycheck
  :config (progn
            (use-package flycheck-pos-tip
              :config (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
            (add-hook 'after-init-hook #'global-flycheck-mode)))
;; ========================================================================

;; ========================================================================
(use-package undo-tree
  :diminish undo-tree-mode
  :config (progn
            (global-undo-tree-mode)
            (setq undo-tree-visualizer-timestamps t
                  undo-tree-visualizer-diff       t)
            (define-key undo-tree-map (kbd "C-/") nil)))
;; ========================================================================

;; ========================================================================
(use-package sauron
  :config (progn
            (setq sauron-modules '(sauron-erc sauron-org sauron-notifications)
                  sauron-separate-frame nil
                  sauron-max-line-length 180
                  sauron-watch-nicks '("kovrik" "kovrik`" "kovrik``"))
            (sauron-start)))
;; ========================================================================

;; ========================================================================
(use-package eyebrowse
  :pin melpa-stable
  :config (progn
            (eyebrowse-mode t)
            (eyebrowse-setup-evil-keys)
            (define-key evil-motion-state-map (kbd "gc") nil)
            (setq eyebrowse-new-workspace t
                  eyebrowse-close-window-config-prompt t)))
;; ========================================================================

;; ========================================================================
(use-package shackle
  :config (progn
            (setq shackle-lighter " |#|"
                  shackle-rules '(("\\`\\*magit.*?\\*\\'" :regexp t :same t)
                                  ("\\`\\*helm.*?\\*\\'"  :regexp t :align t :ratio 0.4)
                                  (compilation-mode       :ignore t)
                                  (erc-mode               :same   t)
                                  (proced-mode            :same   t)
                                  (help-mode              :same   t)
                                  (ibuffer-mode           :same   t)))
            (shackle-mode t)))
;; ========================================================================

;; ========================================================================
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
;; ========================================================================

;; ========================================================================
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
;; ========================================================================
(setq debug-on-error nil)
(provide 'init)
;;; init.el ends here
