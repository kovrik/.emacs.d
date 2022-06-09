;;; -*- lexical-binding: t; -*-
;;; init.el --- kovrik's Emacs config
;;; Commentary:
;;; Code:
(let ((file-name-handler-alist nil)
      (gc-cons-threshold (* 100 1024 1024))
      (gc-cons-percentage 0.6)
      (debug-on-error t)
      (debug-on-quit t))

  (defadvice load (before debug-log activate)
    (message "Advice: now loading: '%s'" (ad-get-arg 0)))

  ;; Package management

  ;; Straight
  (setq straight-check-for-modifications nil)
  (progn
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage)))
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)

  (require 'bind-key)
  (require 'uniquify)
  (use-package queue)
  (use-package browse-kill-ring)
  (use-package dash)
  (use-package epl)
  (use-package f)
  (use-package fold-dwim)
  (use-package fringe-helper)
  (use-package goto-chg)
  (use-package highlight)
  (use-package highlight-escape-sequences)
  (use-package idle-highlight-mode)
  (use-package markdown-mode)
  (use-package pkg-info)
  (use-package s)

  ;; Globals
  ;; UTF-8 Everywhere
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

  (when window-system
    (tooltip-mode    -1)
    (tool-bar-mode   -1)
    (menu-bar-mode    t)
    (scroll-bar-mode -1)
    (toggle-horizontal-scroll-bar -1))

  (customize-set-variable 'display-buffer-base-action
                          '((display-buffer-reuse-window display-buffer-same-window)
                            (reusable-frames . t)))
  ;; avoid resizing
  (customize-set-variable 'even-window-sizes nil)

  (setq-default indent-tabs-mode nil
                tab-width 2
                find-file-visit-truename t
                mode-require-final-newline nil
                major-mode 'text-mode
                pdf-view-display-size 'fit-page
                fringes-outside-margins nil
                indicate-buffer-boundaries nil
                indicate-empty-lines nil
                overflow-newline-into-fringe t
                bidi-paragraph-direction 'left-to-right
                bidi-inhibit-bpa t)

  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        inhibit-startup-message t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil
        frame-inhibit-implied-resize t
        ;; nicer scrolling
        scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1
        ;; move by logical lines rather than visual lines (better for macros)
        line-move-visual nil
        sentence-end-double-space nil
        ring-bell-function 'ignore
        use-dialog-box nil
        visible-bell nil
        confirm-kill-emacs #'yes-or-no-p
        ;; enable external-bound copy-pasting
        select-enable-clipboard t
        save-interprogram-paste-before-kill t
        ;; potentially speed up cursor operations
        auto-window-vscroll nil
        pdf-view-use-scaling 1
        uniquify-buffer-name-style 'forward
        evil-want-keybinding nil
        show-trailing-whitespace t
        ns-use-srgb-colorspace nil
        ;; remove the warnings from the GnuTLS library when using HTTPS
        gnutls-min-prime-bits 4096
        tab-always-indent 'complete
        search-default-mode #'char-fold-to-regexp
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8
        ;; Always load newest byte code
        load-prefer-newer t
        ;; warn when opening files bigger than 200MB
        large-file-warning-threshold 200000000
        ;; Also auto refresh dired, but be quiet about it
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        ;; Version control
        version-control t
        ;; delete excess backup versions silently
        delete-old-versions t
        ;; don't ask for confirmation when opening symlinked file
        vc-follow-symlinks t
        ;; do not create backup files
        ;; auto-save-default nil
        create-lockfiles nil
        ;; make-backup-files nil
        ;; vc-make-backup-files nil
        ;; disable package
        package-enable-at-startup nil
        ;; show parent parentheses
        show-paren-delay 0
        show-paren-style 'parenthesis
        undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))
        custom-file (concat user-emacs-directory "custom.el"))

  ;; make ibuffer the default buffer lister.
  (defalias 'list-buffers 'ibuffer)
  (defalias 'xml-pretty-print 'sgml-pretty-print)
  (defalias 'first 'car)
  (defalias 'second 'cadr)
  (defalias 'third 'caddr)
  (defalias 'when-not 'unless)
  (defalias 'word-count 'count-words)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;;(fringe-mode '(7 . 0))
  (fringe-mode nil)
  (column-number-mode 1)
  (desktop-save-mode)
  (global-font-lock-mode)
  (global-hl-line-mode)
  (global-so-long-mode 1)
  (winner-mode)
  (blink-cursor-mode -1)
  ;; (pixel-scroll-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'narrow-to-defun  'disabled nil)
  (put 'narrow-to-page   'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (global-unset-key (kbd "<f2>"))
  (global-unset-key (kbd "<f3>"))
  (bind-keys ([escape]   . keyboard-quit)
             ("RET"      . newline-and-indent)
             ("C-M-l"    . indent-region)
             ("<f5>"     . (lambda () (interactive) (find-file user-init-file))))

  (load custom-file :noerror :nomessage)

  ;; Delete trailing whitespace on save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (defun my-add-hooks (hooks function)
    "For each hook in HOOKS list bind FUNCTION."
    (dolist (hook hooks)
      (add-hook hook function)))

  (use-package async
    :demand t
    :init (dired-async-mode 1)
    :config (async-bytecomp-package-mode 1)
    (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil))))

  (use-package all-the-icons
    :if (display-graphic-p)
    :config (use-package all-the-icons-dired
              :defer t
              :config (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))

  ;; (use-package doom-themes
  ;;   :config (progn
  ;;             (load-theme 'doom-opera-light t)
  ;;             (doom-themes-org-config)
  ;;             ;; (doom-themes-neotree-config)
  ;;             ))

  (use-package modus-themes
    ;; load the theme files before enabling a theme
    :init (modus-themes-load-themes)
    :custom (modus-themes-italic-constructs nil)
    (modus-themes-bold-constructs nil)
    (modus-themes-region '(accented bg-only no-extend))
    ;; OR (modus-themes-load-vivendi)
    :config (modus-themes-load-operandi)
    :bind ("S-<f5>" . modus-themes-toggle))

  (use-package solaire-mode
    :config (solaire-global-mode +1))

  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :config (setq doom-modeline-minor-modes nil)
    (add-hook 'pdf-view-mode-hook #'doom-modeline-set-pdf-modeline))

  ;; PATH
  (use-package exec-path-from-shell
    :config (progn
              (when (memq window-system '(mac ns))
                (exec-path-from-shell-initialize))
              (when (eq 'windows-nt system-type)
                (setq exec-path (append (parse-colon-path (getenv "PATH"))
                                        (parse-colon-path (getenv "USERPROFILE")) exec-path)))))

  (use-package shell
    :config (progn
              (when (eq 'windows-nt system-type)
                (setq shell-file-name "bash"))
              (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
              (setenv "SHELL" shell-file-name)
              ;; (setenv "ITERM_SHELL_INTEGRATION_INSTALLED" nil)
              (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

  ;; Fonts
  (let ((my-font (cl-find-if (lambda (f) (and f (member (font-get f :name) (font-family-list))))
                             (list (font-spec :name "JetBrains Mono" :size 11)
                                   (font-spec :name "Monaco"         :size 11)
                                   (font-spec :name "Roboto Mono"    :size 11)
                                   (font-spec :name "Fira Code"      :size 11)
                                   (font-spec :name "Meslo LG S"     :size 11)
                                   (font-spec :name "Consolas"       :size 11)))))
    (when my-font
      (message (format "Using %s %s font." (font-get my-font :name) (font-get my-font :size)))
      (add-to-list 'default-frame-alist `(font . ,(concat (font-get my-font :name) "-" (number-to-string (font-get my-font :size)))))
      (when (eq system-type 'darwin)
        (setq mac-allow-anti-aliasing t))))

  ;; One-line packages
  (use-package bug-hunter :defer t)
  (use-package command-log-mode :defer t)
  (use-package restclient :defer t)
  (use-package iedit)
  (use-package rg :config (rg-enable-default-bindings))
  (use-package wgrep)
  (use-package ranger :defer t)
  (use-package rainbow-mode :defer t :diminish rainbow-mode)
  (use-package focus :defer t)
  (use-package flx)
  (use-package request :defer t)

  (use-package which-key
    :init (which-key-mode)
    :config (which-key-setup-minibuffer))

  (use-package vterm
    :defer t
    :custom (vterm-install t)
    :config (add-hook 'vterm-mode-hook (lambda () (global-hl-line-mode 0)))
    (setq vterm-max-scrollback 10000)
    :bind (:map vterm-mode-map
                ("C-y" . vterm-yank)))

  (use-package auto-dim-other-buffers
    :config (set-face-background 'auto-dim-other-buffers-face "#e0e0e6")
    (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
    (auto-dim-other-buffers-mode t))

  (use-package helpful
    :config (progn
              (global-set-key (kbd "C-h f") #'helpful-callable)
              (global-set-key (kbd "C-h v") #'helpful-variable)
              (global-set-key (kbd "C-h k") #'helpful-key)
              (global-set-key (kbd "C-h C") #'helpful-command)
              (global-set-key (kbd "C-h .") #'helpful-at-point)))

  (use-package pdf-tools
    :defer nil
    :commands (pdf-view-mode pdf-tools-install)
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :config
    (pdf-loader-install :no-query)
    (setq-default pdf-view-display-size 'fit-page)
    (add-hook 'pdf-view-mode-hook #'pdf-misc-size-indication-minor-mode)
    :hook ((pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
           (pdf-view-mode.hook . (lambda () (blink-cursor-mode -1)))
           (pdf-view-mode-hook . pdf-tools-enable-minor-modes)))

  (use-package pdf-view-restore
    :after pdf-tools
    :hook (pdf-view-mode . pdf-view-restore-mode))

  (use-package diff-hl
    :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
           (magit-post-refresh . diff-hl-magit-post-refresh))
    :config (progn
              (defun diff-hl-next-hunk-cycle (&optional backward)
                "Go to the beginning of the next (previous if BACKWARD) hunk in the current buffer."
                (interactive)
                (condition-case err
                    (diff-hl-next-hunk backward)
                  (error
                   (let ((pos (point)))
                     (if backward
                         (end-of-buffer)
                       (beginning-of-buffer))
                     (condition-case err
                         (diff-hl-next-hunk backward)
                       (error
                        (goto-char pos)
                        (user-error "No more hunks found")))))))

              (defun diff-hl-previous-hunk-cycle (&optional backward)
                "Go to the beginning of the previous (next if BACKWARD) hunk in the current buffer."
                (interactive)
                (diff-hl-next-hunk-cycle t))

              (bind-keys )
              (global-diff-hl-mode t)
              (diff-hl-margin-mode t))
    :bind (:map diff-hl-mode-map
                ("C-x v n" . diff-hl-next-hunk-cycle)
                ("C-x v j" . diff-hl-next-hunk-cycle)
                ("C-x v p" . diff-hl-previous-hunk-cycle)
                ("C-x v k" . diff-hl-previous-hunk-cycle)
                ("C-x v r" . diff-hl-revert-hunk)))

  (use-package hl-todo
    :hook (prog-mode . hl-todo-mode)
    :config (setq hl-todo-highlight-punctuation ":"
                  hl-todo-keyword-faces `(("TODO"       warning bold)
                                          ("FIXME"      error bold)
                                          ("HACK"       font-lock-constant-face bold)
                                          ("REVIEW"     font-lock-keyword-face bold)
                                          ("NOTE"       success bold)
                                          ("DEPRECATED" font-lock-doc-face bold))))

  (use-package find-func
    :config (defun my-find-thing-at-point ()
              "Find directly thing (var or func) at point in current window."
              (interactive)
              (cond
               ((function-called-at-point)       (find-function (function-called-at-point)))
               ((not (eq 0 (variable-at-point))) (find-variable (variable-at-point)))
               (t                                (user-error "Unknown thing at point!"))))
    :bind (:map emacs-lisp-mode-map
           ("C-S-h" . my-find-thing-at-point)
           ("C-h f" . find-function)
           ("C-h k" . find-function-on-key)
           ("C-h v" . find-variable)
           ("C-h l" . find-library)))

  ;; (use-package smartparens
  ;;   :config (progn
  ;;             (require 'smartparens-config)
  ;;             (use-package evil-smartparens)
  ;;             (show-smartparens-global-mode t)
  ;;             (smartparens-strict-mode)
  ;;             (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  ;;             (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  ;;   ;; FIXME Make it work with evil-mode
  ;;   :bind (:map smartparens-mode-map
  ;;               ("C-<right>"      . sp-forward-slurp-sexp)
  ;;               ("C-<left>"       . sp-forward-barf-sexp)
  ;;               ("C-M-<right>"    . sp-backward-slurp-sexp)
  ;;               ("C-M-<left>"     . sp-backward-barf-sexp)
  ;;               ("M-<delete>"     . sp-unwrap-sexp)
  ;;               ("M-<backspace>"  . sp-unwrap-sexp)
  ;;               ("C-M-t"          . sp-transpose-sexp)))
  (setq electric-pair-pairs '((?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\")))
  (electric-pair-mode t)

;; (use-package treemacs
;;   :straight (treemacs :type git :host github :repo "Alexander-Miller/treemacs")
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   (treemacs-follow-mode t)
;;   (treemacs-filewatch-mode t)
;;   (treemacs-fringe-indicator-mode 'always)
;;   :config (progn
;;             (use-package treemacs-evil
;;               :after (treemacs evil)
;;               :ensure t)
;;
;;             (use-package treemacs-projectile
;;               :after (treemacs projectile)
;;               :ensure t)
;;
;;             (use-package treemacs-icons-dired
;;               :hook (dired-mode . treemacs-icons-dired-enable-once)
;;               :ensure t)
;;
;;             (use-package treemacs-magit
;;               :after (treemacs magit)
;;               :ensure t)))

  (use-package lsp-mode
    :config
    (use-package lsp-treemacs)
    (add-hook 'clojure-mode-hook 'lsp)
    (add-hook 'clojurescript-mode-hook 'lsp)
    (add-hook 'clojurec-mode-hook 'lsp)
    (setq lsp-lens-enable t
          lsp-signature-auto-activate nil
          ;; treemacs-space-between-root-nodes nil
          ))

  (use-package flycheck-clj-kondo :ensure t)

  (use-package clojure-mode
    :defer  t
    ;; :pin melpa-stable
    :config (progn
              (require 'flycheck-clj-kondo)
              (use-package cider
                ;; :pin melpa-stable
                :defer t)
              (use-package clojure-mode-extra-font-locking)
              (use-package flycheck-clojure
                ;; :pin melpa-stable
                :config (progn
                          (flycheck-clojure-setup)
                          (setq flycheck-checkers (delete 'clojure-cider-typed flycheck-checkers))))
              (add-hook 'clojure-mode-hook #'flycheck-mode)
              (add-hook 'clojure-mode-hook #'turn-on-eldoc-mode)
              (add-hook 'cider-mode-hook   #'turn-on-eldoc-mode)
              (setq nrepl-log-messages           t
                    nrepl-hide-special-buffers   t
                    cider-prefer-local-resources t
                    cider-repl-popup-stacktraces t
                    cider-popup-stacktraces      nil)
              (defun cider-find-var-no-prompt ()
                "cider-find-var at point without prompt"
                (interactive)
                (cider-find-var t nil)))
    :bind (:map clojure-mode-map
                ("C-h"   . cider-doc)
                ("C-S-h" . cider-doc)
                ("C-M-x" . cider-eval-defun-at-point)))

  (use-package geiser
    :defer t
    :config (setq geiser-debug-show-debug-p nil
                  geiser-debug-jump-to-debug-p nil))

  (use-package racket-mode
    :defer t
    :config (add-hook 'racket-mode-hook #'company-quickhelp--disable))

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

  ;; Slime
  ;; (use-package slime
  ;;   :defer t
  ;;   :config (progn
  ;;             (setq byte-compile-warnings '(cl-functions))
  ;;             (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  ;;             (setq inferior-lisp-program "sbcl")
  ;;             (slime-setup '(slime-fancy slime-company slime-asdf slime-indentation slime-sbcl-exts slime-scratch))
  ;;             (setq lisp-indent-function 'common-lisp-indent-function)
  ;;             (setq common-lisp-style-default "modern"))
  ;;   :bind (:map slime-mode-map
  ;;               ("C-h" . slime-describe-symbol)
  ;;               ("C-S-h" . slime-describe-symbol)))

  ;; (use-package slime-company
  ;;   :after (slime company)
  ;;   :ensure t
  ;;   :config (setq slime-company-completion 'fuzzy
  ;;                 slime-company-after-completion 'slime-company-just-one-space)
  ;;   ;; We redefine this function to call SLIME-COMPANY-DOC-MODE in the buffer
  ;;   (defun slime-show-description (string package)
  ;;     (let ((bufname (slime-buffer-name :description)))
  ;;       (slime-with-popup-buffer (bufname :package package
  ;;                                         :connection t
  ;;                                         :select slime-description-autofocus)
  ;;                                (when (string= bufname "*slime-description*")
  ;;                                  (with-current-buffer bufname (slime-company-doc-mode)))
  ;;                                (princ string)
  ;;                                (goto-char (point-min))))))

  ;; SLY
  (use-package sly-quicklisp
                 :after sly)

  (use-package sly
               :config (setq sly-lisp-implementations
                             `((sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)
                               (abcl ("/usr/local/bin/abcl" "--noinform" "--no-linedit") :coding-system utf-8-unix) ))
   (evil-set-initial-state 'sly-mrepl-mode 'emacs)
   :bind (:map sly-mode-map
               ("C-S-h" . sly-describe-symbol)))

  (provide 'init-sly)

  (use-package adjust-parens
    :config (progn
              (my-add-hooks '(emacs-lisp-mode-hook
                              clojure-mode-hook
                              common-lisp-lisp-mode-hook
                              racket-mode-hook) #'adjust-parens-mode))
    :bind (:map adjust-parens-mode-map
                ("TAB" . lisp-indent-adjust-parens)
                ("<backtab>" . lisp-dedent-adjust-parens)))

  (use-package highlight-quoted
    :config (progn
              (my-add-hooks '(emacs-lisp-mode-hook
                              clojure-mode-hook
                              common-lisp-lisp-mode-hook
                              racket-mode-hook) #'highlight-quoted-mode)))

  ;; (use-package evil-leader
  ;;   :defer nil
  ;;   :ensure t
  ;;   :config (progn
  ;;             (global-evil-leader-mode)
  ;;             (evil-leader/set-leader "<SPC>")
  ;;             (evil-leader/set-key
  ;;               "s" 'evil-search-highlight-persist-remove-all
  ;;               ;; toggle
  ;;               "t l" 'linum-mode
  ;;               ;; find
  ;;               "f f" 'counsel-find-file
  ;;               "f s" 'swiper-isearch
  ;;               ;; helpful
  ;;               "h ?" 'help-for-help
  ;;               "h k" 'helpful-key
  ;;               "h f" 'helpful-function
  ;;               "h s" 'helpful-symbol
  ;;               "h v" 'helpful-variable
  ;;               "h ." 'helpful-at-point
  ;;               ;; open
  ;;               "o t" 'vterm)))

  (use-package anzu :config (global-anzu-mode +1))

  (use-package vundo
               :commands (vundo)
               :straight (vundo :type git :host github :repo "casouri/vundo")
               :config
               ;; Take less on-screen space.
               (setq vundo-compact-display t)
               ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
               (define-key vundo-mode-map (kbd "l") #'vundo-forward)
               (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
               (define-key vundo-mode-map (kbd "h") #'vundo-backward)
               (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
               (define-key vundo-mode-map (kbd "j") #'vundo-next)
               (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
               (define-key vundo-mode-map (kbd "k") #'vundo-previous)
               (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
               (define-key vundo-mode-map (kbd "<home>") #'vundo-stem-root)
               (define-key vundo-mode-map (kbd "<end>") #'vundo-stem-end)
               (define-key vundo-mode-map (kbd "q") #'vundo-quit)
               (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
               (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))
  (with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "C-M-u") 'vundo))

  (use-package undo-fu
               :config
               (global-unset-key (kbd "C-z"))
               (global-set-key (kbd "C-z")   'undo-fu-only-undo)
               (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

  (use-package evil
    :init (setq evil-undo-system 'undo-fu)
    :config (progn
              (use-package evil-anzu)
              (use-package evil-org :defer t)
              (use-package evil-numbers)
              (use-package evil-surround   :config (global-evil-surround-mode 1))
              (use-package evil-visualstar :config (global-evil-visualstar-mode))
              (use-package evil-search-highlight-persist
                :config (global-evil-search-highlight-persist t))
              ;; (use-package evil-matchit
              ;;   :config (my-add-hooks '(nxml-mode-hook
              ;;                           html-mode-hook
              ;;                           web-mode-hook) #'evil-matchit-mode))
              (use-package evil-commentary
                :config (progn
                          (defun my-comment-line-and-go-to-next ()
                            "Comment current line and go to next."
                            (interactive)
                            (evil-commentary-line (line-beginning-position)
                                                  (line-end-position)
                                                  'line)
                            (evil-next-line))
                          (define-key evil-motion-state-map (kbd "gc") 'evil-commentary)
                          (bind-key "C-/" 'my-comment-line-and-go-to-next)
                          (bind-key "C-/" 'evil-commentary evil-visual-state-map)
                          (evil-commentary-mode)))
              ;; Emacs keys in INSERT mode
              (setcdr evil-insert-state-map nil)
              (setq evil-move-cursor-back t
                    evil-default-cursor   t
                    evil-want-C-u-scroll  t
                    evil-want-C-w-delete  t)
              ;; treat symbol as a word
              (defalias #'forward-evil-word #'forward-evil-symbol)
              ;; kill buffer, but don't close window
              (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
              ;; (evil-ex-define-cmd "quit"   'evil-quit)
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
              (global-set-key                             [escape]      'evil-exit-emacs-state)
              (define-key evil-visual-state-map           [escape]      'keyboard-quit)
              (define-key minibuffer-local-map            [escape]      'minibuffer-keyboard-quit)
              (define-key minibuffer-local-ns-map         [escape]      'minibuffer-keyboard-quit)
              (define-key minibuffer-local-completion-map [escape]      'minibuffer-keyboard-quit)
              (define-key minibuffer-local-must-match-map [escape]      'minibuffer-keyboard-quit)
              (define-key minibuffer-local-isearch-map    [escape]      'minibuffer-keyboard-quit)
              (define-key evil-insert-state-map           [escape]      'evil-normal-state)
              (define-key evil-insert-state-map           (kbd "C-n")   'company-complete)
              (define-key evil-insert-state-map           (kbd "C-SPC") 'company-complete)
              (bind-keys :map evil-normal-state-map
                         ([next]   . evil-scroll-down)
                         ([prior]  . evil-scroll-up)
                         ([escape] . keyboard-quit)
                         ("j"      . evil-next-visual-line)
                         ("k"      . evil-previous-visual-line)
                         ("C-y"    . evil-paste-after)
                         ("SPC"    . hydra-common-commands/body))
              (bind-keys :map evil-visual-state-map ("SPC" . hydra-common-commands/body))
              (my-add-hooks '(help-mode-hook prog-mode-hook text-mode-hook pdf-view-mode-hook) #'evil-local-mode)
              (defun my-evil-off ()
                "Turn 'evil-mode' off and change cursor type to bar."
                (interactive)
                (turn-off-evil-mode)
                (setq cursor-type 'bar))
              ;; Disable evil-mode in some major modes
              (my-add-hooks '(shell-mode-hook term-mode-hook magit-mode-hook erc-mode-hook
                                              eshell-mode-hook comint-mode-hook proced-mode-hook nrepl-connected-hook)
                            #'my-evil-off)
              (with-eval-after-load 'term (evil-set-initial-state 'term-mode 'emacs))
              (with-eval-after-load 'vterm (evil-set-initial-state 'vterm-mode 'emacs))
              (evil-set-initial-state 'pdf-view-mode 'normal)))

  (use-package evil-collection
    :after evil
    :ensure t
    :config (evil-collection-init))

  (use-package dired
    :straight (:type built-in)
    :bind (:map dired-mode-map
                ("j" . dired-next-line)
                ("k" . dired-previous-line)
                ("n" . evil-search-next)
                ("N" . evil-search-previous)))

  (use-package eldoc
    :diminish eldoc-mode
    :commands turn-on-eldoc-mode
    :init (my-add-hooks '(emacs-lisp-mode-hook
                          lisp-interaction-mode-hook
                          ielm-mode-hook)
                        #'turn-on-eldoc-mode))

  ;; (use-package vertico
  ;;              :init
  ;;              (vertico-mode)

  ;;              ;; Different scroll margin
  ;;              ;; (setq vertico-scroll-margin 0)

  ;;              ;; Show more candidates
  ;;              ;; (setq vertico-count 20)

  ;;              ;; Grow and shrink the Vertico minibuffer
  ;;              ;; (setq vertico-resize t)

  ;;              ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;;              ;; (setq vertico-cycle t)
  ;;              )

  ;; (use-package savehist
  ;;   :init (savehist-mode))

  ;; (use-package consult)
  ;; (use-package marginalia
  ;;              ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  ;;              :bind (("M-A" . marginalia-cycle)
  ;;                     :map minibuffer-local-map
  ;;                     ("M-A" . marginalia-cycle))

  ;;              ;; The :init configuration is always executed (Not lazy!)
  ;;              :init

  ;;              ;; Must be in the :init section of use-package such that the mode gets
  ;;              ;; enabled right away. Note that this forces loading the package.
  ;;              (marginalia-mode))
  ;; (use-package prescient)
  ;; ;; (use-package ivy-prescient)
  ;; ;; (use-package company-prescient)
  ;; (use-package selectrum-prescient)
  ;; (use-package selectrum
  ;; :config (selectrum-mode +1)
  ;; (selectrum-prescient-mode +1))
  ;; TODO embark?

  (use-package prescient
    :config (progn
              (use-package ivy-prescient :config (ivy-prescient-mode))
              (use-package company-prescient :config (company-prescient-mode))))

  (use-package swiper
    :demand t
    :diminish ivy-mode
    :config (progn
              (use-package ivy
                :custom-face (ivy-current-match ((t (:inherit 'hl-line))))
                :config (progn
                          (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                                        (t      . ivy--regex-plus)))
                          (setq ivy-use-virtual-buffers t
                                ivy-display-style 'plain
                                ivy-initial-inputs-alist nil
                                ivy-count-format "")
                          (defun my-ivy-page-up (&optional arg)
                            (interactive "p")
                            (ivy-previous-line ivy-height))
                          (defun my-ivy-page-down (&optional arg)
                            (interactive "p")
                            (ivy-next-line ivy-height))
                          (bind-keys :map ivy-minibuffer-map
                                     ([escape] . minibuffer-keyboard-quit)
                                     ([next]   . my-ivy-page-down)
                                     ("C-f"    . my-ivy-page-down)
                                     ([prior]  . my-ivy-page-up)
                                     ("C-b"    . my-ivy-page-up)
                                     ("C-k"    . ivy-previous-line)
                                     ("C-j"    . ivy-next-line))
                          (ivy-mode 1)))
              (use-package counsel
                :config  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s"
                               counsel-describe-function-function #'helpful-callable
                               counsel-describe-variable-function #'helpful-variable))
              (use-package ivy-rich
                :config (ivy-rich-mode 1)
                (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)))
    :bind (("\C-s"    . swiper-isearch)
           ("M-s o"   . swiper-multi)
           ("C-S-f"   . swiper-all)
           ("C-c C-r" . ivy-resume)
           ("M-x"     . counsel-M-x)
           ("C-x f"   . counsel-find-file)
           ("C-x C-f" . counsel-find-file)
           ("C-x C-b" . ivy-switch-buffer)
           ("<f1> f"  . counsel-describe-function)
           ("<f1> v"  . counsel-describe-variable)
           ("<f1> l"  . counsel-load-library)
           ("C-c g"   . counsel-git)
           ("C-c j"   . counsel-git-grep)
           ("C-c k"   . counsel-ag)
           ("C-x l"   . counsel-locate)))

  ;; (use-package ace-window
  ;;              :ensure t
  ;;              :defer 1
  ;;              :config
  ;;              (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
  ;;              (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
  ;;              (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
  ;;                    aw-dispatch-always t
  ;;                    aw-dispatch-alist '((?x aw-delete-window "Ace - Delete Window")
  ;;                                        (?c aw-swap-window "Ace - Swap Window")
  ;;                                        (?n aw-flip-window)
  ;;                                        (?v aw-split-window-vert "Ace - Split Vert Window")
  ;;                                        (?h aw-split-window-horz "Ace - Split Horz Window")
  ;;                                        (?m delete-other-windows "Ace - Maximize Window")
  ;;                                        (?g delete-other-windows)
  ;;                                        (?b balance-windows)
  ;;                                        (?u (lambda () (progn (winner-undo) (setq this-command 'winner-undo))))
  ;;                                        (?r winner-redo))))

  (use-package projectile
    :diminish projectile-mode
    :config (progn
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
                    projectile-completion-system 'ivy
                    projectile-enable-caching t
                    projectile-switch-project-action 'my-projectile-switch-to-project)
              (when (eq system-type 'windows-nt)
                (setq projectile-indexing-method 'alien
                      projectile-enable-caching  nil))
              (projectile-global-mode)

              (defun my-find-file ()
                "If currently in a project, then use Projectile to fuzzy find a file.
Use Counsel otherwise."
                (interactive)
                (require 'projectile)
                (if (projectile-project-p)
                    (projectile-find-file)
                  (counsel-find-file))))
    :bind (("C-S-p" . projectile-switch-project)
           ("C-S-n" . my-find-file)))

  (use-package company
    :diminish company-mode
    :config (progn
              (use-package company-quickhelp
                :config (progn (setq company-quickhelp-delay 0.7)
                               (company-quickhelp-mode 1)))
              (use-package company-flx
                :config (with-eval-after-load 'company
                          (company-flx-mode +1)))
              (setq company-show-numbers t
                    company-idle-delay 0.7
                    company-minimum-prefix-length 3
                    company-require-match 'never
                    company-dabbrev-downcase nil
                    company-dabbrev-ignore-case t
                    company-selection-wrap-around t
                    company-transformers '(company-sort-by-occurrence))
              (add-hook 'after-init-hook 'global-company-mode)))

  (use-package org
    :defer t
    :config (progn
              (use-package org-bullets
                :config (progn
                          (defun my-org-bullets-on () (org-bullets-mode 1))
                          (add-hook 'org-mode-hook #'my-org-bullets-on)))
              (setq org-edit-src-content-indentation 0
                    org-src-preserve-indentation t
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
              (set-face-attribute 'org-level-1 nil :height 1.0)
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
    ;; :pin melpa
    :config (progn
              (use-package magit-popup)
              (setenv "GIT_ASKPASS" "git-gui--askpass")
              ;; Don't want to view changes every time before commit
              (setq magit-status-buffer-switch-function 'switch-to-buffer
                    magit-commit-show-diff nil
                    magit-diff-options '("--ignore-all-space")
                    magit-diff-refine-hunk t
                    magit-log-arguments '("--decorate" "--graph" "--color" "-n80")
                    magit-log-cutoff-length 80
                    magit-display-buffer-function 'display-buffer
                    git-commit-finish-query-functions '())

              (with-eval-after-load 'transient
                (transient-bind-q-to-quit))

              (defun my-magit-checkout-current-file (arg)
                (let ((f (magit-current-file)))
                  (if f
                      (magit-run-git-async "checkout" arg f)
                    (user-error "No file selected!"))))

              (defun my-magit-checkout-ours ()
                (interactive)
                (my-magit-checkout-current-file "--ours"))

              (defun my-magit-checkout-theirs ()
                (interactive)
                (my-magit-checkout-current-file "--theirs")))
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
    :config (setq eshell-prompt-regexp "^[^αλ\n]*[αλ] "
                  eshell-highlight-prompt nil
                  eshell-save-history-on-exit t
                  eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
    (add-hook 'eshell-mode-hook (lambda () (global-hl-line-mode 0)))
    (defun eshell/clear-scrollback ()
      "Clear the scrollback content of the eshell window."
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (defalias 'open 'find-file-other-window)
    (defalias 'clean 'eshell/clear-scrollback))

  (use-package web-mode
    :defer t
    ;; :pin melpa-stable
    :config (progn (add-to-list 'auto-mode-alist '("\\.html?\\'"   . web-mode))
                   (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))))

  (use-package flycheck
    :defer t
    :config (progn
              (use-package flycheck-pos-tip
                :config (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
              (setq-default flycheck-emacs-lisp-load-path 'inherit)
              (defun my-flycheck-next-error-cycle ()
                "Go to next flycheck error if exists.
Start from the beginning of buffer otherwise."
                (interactive)
                (let ((pos (flycheck-next-error-pos 1 nil)))
                  (if pos
                      (goto-char pos)
                    (flycheck-next-error-function 1 t))))
              (define-key flycheck-mode-map (kbd "<f2>") #'my-flycheck-next-error-cycle)
              (define-key flycheck-mode-map (kbd "<f3>") #'flycheck-list-errors)))

  (use-package neotree
    :defer t
    :config (progn
              (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
              (define-key neotree-mode-map (kbd "<tab>") #'neotree-enter)
              (defun neotree-current-file ()
                "Opens current file in Neotree. If current buffer is a Neotree buffer then closes it."
                (interactive)
                (if (eq major-mode 'neotree-mode)
                    (neotree-toggle)
                  (neotree-find (buffer-file-name)))))
    :bind (("M-<f1>". neotree-current-file)))

  (use-package imenu-list
               :config (setq imenu-list-position 'left
                             imenu-list-size 36
                             imenu-list-focus-after-activation t)
                       (my-add-hooks '(imenu-list-major-mode-hook) #'turn-on-evil-mode)
               :bind (("M-<f2>" . imenu-list-smart-toggle)))

  (use-package eyebrowse
    :diminish eyebrowse-mode
    ;; :pin melpa-stable
    :config (progn
              (eyebrowse-mode t)
              (eyebrowse-setup-evil-keys)
              (setq eyebrowse-new-workspace t
                    eyebrowse-close-window-config-prompt t))
    :bind (("C-1"  . eyebrowse-switch-to-window-config-1)
           ("C-2"  . eyebrowse-switch-to-window-config-2)
           ("C-3"  . eyebrowse-switch-to-window-config-3)
           ("C-4"  . eyebrowse-switch-to-window-config-4)
           ("C-5"  . eyebrowse-switch-to-window-config-5)
           ("C-6"  . eyebrowse-switch-to-window-config-6)
           ("C-7"  . eyebrowse-switch-to-window-config-7)
           ("C-8"  . eyebrowse-switch-to-window-config-8)
           ("C-9"  . eyebrowse-switch-to-window-config-9)
           ("C-0"  . eyebrowse-switch-to-window-config-0)))

  (use-package shackle
    :config (progn
              (setq shackle-lighter ""
                    shackle-rules '(("\\`\\*magit.*?\\*\\'"   :regexp t :same t :inhibit-window-quit t)
                                    (compilation-mode         :same t :inhibit-window-quit t)
                                    (erc-mode                 :same t :inhibit-window-quit t)
                                    (proced-mode              :same t :inhibit-window-quit t)
                                    (help-mode                :same t :inhibit-window-quit t)
                                    (ibuffer-mode             :same t :inhibit-window-quit t)
                                    (slime-mode               :popup  t
                                                              :align 'below
                                                              :ratio 0.33)
                                    (sly-mode                 :popup  t
                                                              :align 'below
                                                              :ratio 0.33)
                                    (inferior-lisp-mode       :popup  t
                                                              :align 'below
                                                              :ratio 0.33)
                                    (flycheck-error-list-mode :popup  t
                                                              :align 'below
                                                              :ratio 0.33
                                                              :select t)
                                    ("\\`\\*diff-hl\\*.*?\\'" :regexp t
                                                              :popup  t
                                                              :align 'below
                                                              :ratio 0.33)))
              (shackle-mode t)))

  ;; Solidity
  (use-package solidity-mode)
  (use-package solidity-flycheck
    :config (progn
              (setq solidity-flycheck-solc-checker-active t
                    solidity-flycheck-solium-checker-active t)))
  (use-package company-solidity
    :config (progn
              (add-hook 'solidity-mode-hook
                        (lambda () (set (make-local-variable 'company-backends)
                                        (append '((company-solidity company-capf company-dabbrev-code))
                                                company-backends))))))

  ;; FIXME Setup grammars
  (use-package tree-sitter
    :config (require 'tree-sitter)
    (require 'tree-sitter-hl)
    (global-tree-sitter-mode))
  (use-package tree-sitter-langs
    :config (require 'tree-sitter-langs))

  ;; TODO Remove Hydra and just use which-key
  ;; Hydras
  (use-package hydra
    :config (progn
              (defhydra hydra-find
                (:color blue :hint nil)
                "
  Find
  -----
  _h_: at point         _f_: file      _F_: function   _s_: swiper   _g_: grep
  _k_: function on key  _v_: variabl   _l_: library    _a_: ag
  "
                ("h"   my-find-thing-at-point)
                ("F"   find-function)
                ("s"   swiper-isearch)
                ("f"   counsel-find-file)
                ("k"   find-function-on-key)
                ("v"   find-variable)
                ("l"   find-library)
                ("a"   counsel-ag)
                ("g"   counsel-grep)

                ("q"   nil "quit" :color blue))

              (defhydra hydra-ediff (:color blue :hint nil)
                "
  ^Buffers              Files               VC                Ediff regions
  ----------------------------------------------------------------------------------
  _b_: buffers           _f_: files (_=_)        _r_: revisions      _l_: linewise
  _B_: Buffers (3-way)   _F_: Files (3-way)                      _w_: wordwise
                       _c_: current file
  "
                ("b" ediff-buffers)
                ("B" ediff-buffers3)
                ("=" ediff-files)
                ("f" ediff-files)
                ("F" ediff-files3)
                ("c" ediff-current-file)
                ("r" ediff-revision)
                ("l" ediff-regions-linewise)
                ("w" ediff-regions-wordwise)
                ("q" nil "quit"))

              (defhydra hydra-describe
                (:exit t :columns 2)
                "
  Describe
  --------- "
                ("v" counsel-describe-variable "variable")
                ("f" counsel-describe-function "function")
                ("F" counsel-describe-face "face")
                ("k" describe-key "key")
                ("q" nil "quit"))

              (defhydra hydra-eval
                (:exit t :columns 2)
                "
  Evaluate
  --------- "
                ("r" eval-region "region")
                ("b" eval-buffer "buffer")
                ("e" eval-expression "S-expression")
                ("l" eval-last-sexp "last s-expression")
                ("L" eval-last-sexp-print-value "last s-expression and print value  ")
                ("d" eval-defun "defun / function")
                ("f" eval-defun "defun / function")
                ("q"   nil "quit" :color blue))

              (defhydra hydra-window
                (:hint nil)
                "
  Split     Delete     Switch Window   Buffers        Winner
  --------------------------------------------------------------
  _\\_: vert   _c_: close   _h_: left         _p_: previous    _u_: undo
  _-_: horz   _o_: other   _j_: down         _n_: next        _r_: redo
                       _k_: up           _b_: select
                       _l_: right        _w_: ace-window
                                       _R_: revert-buffer
  "

                ("-" split-window-below)
                ("\\" split-window-right)

                ("c" delete-window)
                ("o" delete-other-windows)

                ("h" windmove-left)
                ("j" windmove-down)
                ("k" windmove-up)
                ("l" windmove-right)

                ("p" previous-buffer)
                ("n" next-buffer)
                ("b" counsel-ibuffer :color blue)
                ("w" ace-window :color blue)
                ("R" revert-buffer :color blue)

                ("u" winner-undo)
                ("r" winner-redo)

                ("q" nil "quit"))

              (defhydra hydra-project
                (:color blue :hint nil)
                "
  Project
  --------
  _p_: projectile   _c_: compile
  "
                ("p" projectile-switch-project)
                ("c" compile)
                ("q" nil "quit"))

              (defhydra hydra-buffers
                (:color blue :hint nil)
                "
  Buffers
  --------
  _b_: switch buffer  _B_: bury buffer
  _k_: kill buffer    _SPC_: previous buffer
  "
                ("b" ivy-switch-buffer)
                ("k" kill-buffer)
                ("B" bury-buffer)
                ("SPC" previous-buffer)
                ("q" nil "quit"))

              (defhydra hydra-metahelp-menu (:hint nil :exit t :foreign-keys warn)
                "
  Describe                           ^^^^^^                             Goto         ^^ View
  -----------------------------------------------------------------------------------------------------
  _b_: bindings             _k_:   key                   _s_:   symbol               _e_: *Messages*    _a_: apropos
  _c_: key-briefly          _K_:   Key (info)            _S_:   Symbol (info)        _i_: info manual   _l_: lossage
  _C_: coding system        _L_:   Language environment  _C-s_: syntax table         _._: local help
  _d_: documentation        _m_:   mode                  _v_:   variable
  _E_: Emacs...             _p_:   package (by topic)    _V_:   Variable (counsel)
  _f_: function             _P_:   Package (by name)     _w_:   whereis (func->keys)
  _F_: Function (info)      _C-p_: external package
  _I_: key input method                                           ^^^^^^                 _q_: quit
  "
                ("?"   counsel-hydra-heads)
                ("a"   counsel-apropos-thing-at-point)
                ("b"   describe-bindings)
                ("c"   describe-key-briefly)
                ("C"   describe-coding-system)
                ("d"   apropos-documentation)
                ("e"   view-echo-area-messages)
                ("E"   hydra-metahelp-emacs-menu/body)
                ("f"   counsel-describe-function)
                ("F"   Info-goto-emacs-command-node)
                ("i"   info)
                ("I"   describe-input-method)
                ("k"   describe-key)
                ("K"   Info-goto-emacs-key-command-node)
                ("l"   view-lossage)
                ("L"   describe-language-environment)
                ("m"   describe-mode)
                ("p"   finder-by-keyword)
                ("P"   describe-package)
                ("C-p" view-external-packages)
                ("q"   nil nil)
                ("s"   describe-symbol)
                ("S"   info-lookup-symbol)
                ("C-s" describe-syntax)
                ("v"   describe-variable)
                ("V"   counsel-describe-variable)
                ("w"   where-is)
                ("."   display-local-help))

              (defhydra hydra-metahelp-emacs-menu (:hint nil :exit t :foreign-keys warn)
                "
  Emacs
  ----------------------------------------------------------------------------------------
  _a_: about Emacs  _D_: Distribution  _h_: hello file     _n_: news            _T_: Todo          _q_: quit
  _c_: copying      _F_: FAQ           _i_: info manual
  _d_: debuging     _G_: GNU           _t_: tutorial
  "
                ("?" counsel-hydra-heads)
                ("a" about-emacs)
                ("c" describe-copying)
                ("d" view-emacs-debugging)
                ("D" describe-distribution)
                ("F" view-emacs-FAQ)
                ("G" describe-gnu-project)
                ("h" view-hello-file)
                ("i" info-manual)
                ("n" view-emacs-news)
                ("q" nil nil)
                ("t" help-with-tutorial)
                ("m" view-order-anuals)
                ("T" view-emacs-todo))

              (defhydra hydra-common-commands
                (:color blue :hint nil)
                "

  _h_:   help            _f_: find        _x_: execute  _._:   find file
  _P_:   project         _d_: describe    _D_: ediff    _SPC_: no highlight
  _b_:   buffers         _e_: evaluate    _w_: window
  "
                ("h"   hydra-metahelp-menu/body)
                ("f"   hydra-find/body)
                ("e"   hydra-eval/body)
                ("w"   hydra-window/body)
                ("d"   hydra-describe/body)
                ("SPC" evil-search-highlight-persist-remove-all)
                ("u"   vundo)
                ("P"   hydra-project/body)
                ("D"   hydra-ediff/body)
                ("x"   counsel-M-x)
                ("b"   hydra-buffers/body)
                ("."   counsel-find-file)
                ("q"   nil "quit")))
    :bind (("M-SPC" . hydra-common-commands/body)))

  (use-package emacs
    :init
    ;; do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Emacs 28: hide commands in M-x which do not work in the current mode.
    (setq read-extended-command-predicate #'command-completion-default-include-p)

    ;; enable recursive minibuffers
    (setq enable-recursive-minibuffers t)

    ;; completion ignores case
    (setq completion-ignore-case t)
    (setq read-file-name-completion-ignore-case t)

    ;; allow Emacs to resize mini windows
    (setq resize-mini-windows t))

  ;; Misc
  (progn
    (defun my-hsplit-last-buffer (prefix)
      "Split the window horizontally and display the previous buffer.  Args: PREFIX."
      (interactive "p")
      (split-window-vertically)
      (other-window 1 nil)
      (when (= prefix 1) (set-buffer-major-mode (switch-to-buffer (generate-new-buffer "*new*")))))
    (defun my-vsplit-last-buffer (prefix)
      "Split the window vertically and display the previous buffer.  Args: PREFIX."
      (interactive "p")
      (split-window-horizontally)
      (other-window 1 nil)
      (when (= prefix 1) (set-buffer-major-mode (switch-to-buffer (generate-new-buffer "*new*")))))
    (bind-keys ("C-x 2" . my-hsplit-last-buffer) ("C-x 3"  . my-vsplit-last-buffer)
               ("C-x -" . my-hsplit-last-buffer) ("C-x \\" . my-vsplit-last-buffer)
               ("C-x _" . my-hsplit-last-buffer) ("C-x |"  . my-vsplit-last-buffer)))

  (defun my-align-repeat (start end regexp)
    "Repeat alignment with respect to the given regular expression.  Args: START END REGEXP."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1 t))

  ;; (progn
  ;;   (defun my-quit (count)
  ;;     "Quit if in a read-only buffer; otherwise, call self-insert-command."
  ;;     (interactive "p")
  ;;     (let ((m (buffer-local-value 'major-mode (current-buffer))))
  ;;       (cond
  ;;        ((eq 'magit-popup-mode m) (magit-popup-quit))
  ;;        ((eq 'neotree-mode m)     (neotree-toggle))
  ;;        ((eq 'vterm-mode m)       (vterm--self-insert))
  ;;        ((not buffer-read-only)   (self-insert-command count))
  ;;        (t                        (kill-this-buffer)))))
  ;;   (global-set-key (kbd "q") 'my-quit)
  ;;   (require 'help-mode)
  ;;   (require 'proced)
  ;;   (require 'compile)
  ;;   (dolist (mode-map (list help-mode-map proced-mode-map compilation-mode-map))
  ;;     (define-key mode-map (kbd "q") 'my-quit)))

  (defun new-scratch-buffer ()
    "Creates a new scratch buffer."
    (interactive)
    (switch-to-buffer (generate-new-buffer-name "*scratch*")))

  (defun emacs-quit ()
    "Quit Emacs."
    (interactive)
    (kill-emacs))
  )

;; Allow font-lock-mode to do background parsing and restore some settings
(setq jit-lock-stealth-time 1
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.05
      debug-on-error nil
      debug-on-quit nil
      gc-cons-threshold (* 1 1024 1024)
      gc-cons-percentage 0.1
      read-process-output-max (* 1024 1024))
(provide 'init)
;;; init.el ends here
