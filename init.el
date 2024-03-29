;;; -*- lexical-binding: t; no-byte-compile: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;;; init.el --- kovrik's Emacs config
;;; Commentary:
;;; Code:
(setq packages-loaded-at-startup '())
(let ((file-name-handler-alist nil)
      (read-process-output-max 10000000)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      (debug-on-error t)
      (debug-on-quit t))

  ;; Compile warnings
  (setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

  (setq idle-update-delay 1.0)

  ;; Use viper-mode as a fallback in case we fail to init evil
  (setq viper-mode t)
  (require 'viper)

  (defmacro k-time (&rest body)
    "Measure and return the time it takes evaluating BODY."
    `(let ((time (current-time)))
       ,@body
       (float-time (time-since time))))

  ;; When idle for 15sec run the GC no matter what.
  (defvar k-gc-timer
    (run-with-idle-timer 15 t (lambda () (garbage-collect))))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "\n")
              (message (format "Emacs startup time %s with %d garbage collections. Loaded %d packages: %s"
                               (emacs-init-time)
                               gcs-done
                               (length packages-loaded-at-startup)
                               (concat "\n\t- " (mapconcat #'identity packages-loaded-at-startup "\n\t- "))))
              (message "Happy hacking!")))

  (defadvice load (before debug-log activate)
    (let ((package-name (ad-get-arg 0)))
      (push (replace-regexp-in-string "^.*/" "" package-name) packages-loaded-at-startup)
      (message "Loading: '%s'" package-name)))

  ;; Package management
  ;(customize-set-variable 'package-enable-at-startup nil)
  ;(advice-add #'package--ensure-init-file :override #'ignore)
  ;; Do not allow loading from the package cache (same reason).
  ;(customize-set-variable 'package-quickstart nil)
  ;; don't add that `custom-set-variables' block to my init.el!
  ;(setq package--init-file-ensured t)

  ;; bootstrap straight
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  ;; use use-package
  (straight-use-package 'use-package)
  ;; automatically ensure every package exists (like :ensure or :straight)
  (setq straight-use-package-by-default t)

;; (defvar elpaca-installer-version 0.5)
;; (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;; (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
;; (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
;; (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
;;                               :ref nil
;;                               :files (:defaults (:exclude "extensions"))
;;                               :build (:not elpaca--activate-package)))
;; (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
;;        (build (expand-file-name "elpaca/" elpaca-builds-directory))
;;        (order (cdr elpaca-order))
;;        (default-directory repo))
;;   (add-to-list 'load-path (if (file-exists-p build) build repo))
;;   (unless (file-exists-p repo)
;;     (make-directory repo t)
;;     (when (< emacs-major-version 28) (require 'subr-x))
;;     (condition-case-unless-debug err
;;         (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
;;                  ((zerop (call-process "git" nil buffer t "clone"
;;                                        (plist-get order :repo) repo)))
;;                  ((zerop (call-process "git" nil buffer t "checkout"
;;                                        (or (plist-get order :ref) "--"))))
;;                  (emacs (concat invocation-directory invocation-name))
;;                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
;;                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
;;                  ((require 'elpaca))
;;                  ((elpaca-generate-autoloads "elpaca" repo)))
;;             (progn (message "%s" (buffer-string)) (kill-buffer buffer))
;;           (error "%s" (with-current-buffer buffer (buffer-string))))
;;       ((error) (warn "%s" err) (delete-directory repo 'recursive))))
;;   (unless (require 'elpaca-autoloads nil t)
;;     (require 'elpaca)
;;     (elpaca-generate-autoloads "elpaca" repo)
;;     (load "./elpaca-autoloads")))
;; (add-hook 'after-init-hook #'elpaca-process-queues)
;; (elpaca `(,@elpaca-order))
;; (elpaca elpaca-use-package
;;   ;; Enable :elpaca use-package keyword.
;;   (elpaca-use-package-mode)
;;   ;; Assume :elpaca t unless otherwise specified.
;;   (setq elpaca-use-package-by-default t))
;;
;; ;; Block until current queue processed.
;; (elpaca-wait)

  (require 'bind-key)
  (require 'uniquify)

  (use-package emacs
    :config
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

    ;; Fonts
    (let ((my-font (cl-find-if (lambda (f) (and f (member (font-get f :name) (font-family-list))))
                               (list (font-spec :name "JetBrains Mono" :size 11)
                                     (font-spec :name "Monaco"         :size 11)
                                     (font-spec :name "Meslo LG S"     :size 11)
                                     (font-spec :name "Consolas"       :size 11)))))
      (when my-font
        (message (format "Using %s %s font." (font-get my-font :name) (font-get my-font :size)))
        (set-frame-font my-font)))

    (when (eq system-type 'darwin)
      (setq mac-allow-anti-aliasing t
            mac-command-modifier       'meta
            mac-option-modifier         nil
            mac-control-modifier       'control
            mac-right-command-modifier 'super
            mac-right-control-modifier 'hyper))

    (setq-default indent-tabs-mode nil
                  tab-width 2
                  cursor-type 'bar
                  ;; Silence warnings for redefinition
                  ad-redefinition-action 'accept
                  ;; Hide the cursor in inactive windows
                  cursor-in-non-selected-windows nil
                  ;; Prevent tracking for auto-saves
                  auto-save-list-file-prefix nil
                  find-file-visit-truename t
                  mode-require-final-newline nil
                  pdf-view-display-size 'fit-page
                  major-mode 'text-mode
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
          scroll-margin 5
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
          gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
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
          make-backup-files nil
          ;; But in case the user does enable it, some sensible defaults:
          version-control t     ; number each backup file
          backup-by-copying t   ; instead of renaming current file (clobbers links)
          delete-old-versions t ; clean up after itself
          kept-old-versions 5
          kept-new-versions 5
          backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/")))
          ;; vc-make-backup-files nil
          ;; show parent parentheses
          show-paren-delay 0
          show-paren-style 'parenthesis
          ;; enable recursive minibuffers
          enable-recursive-minibuffers t
          ;; completion ignores case
          completion-ignore-case t
          ;; TAB cycle if there are only few candidates
          completion-cycle-threshold 0
          ;; Do not allow the cursor in the minibuffer prompt
          minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
          read-file-name-completion-ignore-case t
          ;; allow Emacs to resize mini windows
          resize-mini-windows t
          ;; Enable indentation+completion using the TAB key.
          ;; `completion-at-point' is often bound to M-TAB.
          what-cursor-show-names t
          dired-dwim-target t
          dired-kill-when-opening-new-dired-buffer t
          tab-always-indent 'complete
          reb-re-syntax 'string
          custom-file (concat user-emacs-directory "custom.el")
          electric-pair-pairs '((?\{ . ?\})
                                (?\( . ?\))
                                (?\[ . ?\])
                                (?\" . ?\")))

    ;; make ibuffer the default buffer lister.
    (defalias 'list-buffers 'ibuffer)
    (defalias 'xml-pretty-print 'sgml-pretty-print)
    (defalias 'first 'car)
    (defalias 'second 'cadr)
    (defalias 'third 'caddr)
    (defalias 'when-not 'unless)
    (defalias 'word-count 'count-words)
    (defalias 'yes-or-no-p 'y-or-n-p)
    (fringe-mode nil)
    (column-number-mode 1)
    (desktop-save-mode)
    (global-font-lock-mode)
    ;; hl-line
    (global-hl-line-mode 1)
    (global-so-long-mode 1)
    (winner-mode)
    (repeat-mode 1)
    (blink-cursor-mode -1)
    (electric-pair-mode t)
    ;; (pixel-scroll-mode)
    (fset 'yes-or-no-p 'y-or-n-p)
    (put 'narrow-to-defun  'disabled nil)
    (put 'narrow-to-page   'disabled nil)
    (put 'narrow-to-region 'disabled nil)
    (when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))
    (global-unset-key (kbd "<f2>"))
    (global-unset-key (kbd "<f3>"))

    (load custom-file :noerror :nomessage)

    ;; Delete trailing whitespace on save
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    ;; do not allow the cursor in the minibuffer prompt
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
    (add-hook 'window-setup-hook 'toggle-frame-maximized t)

    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    (defun my-hsplit-last-buffer (prefix)
      "Split the window horizontally and display the previous buffer.  Args: PREFIX."
      (interactive "p")
      (split-window-vertically)
      (other-window 1 nil)
      (when (= prefix 1)
        (set-buffer-major-mode (switch-to-buffer (generate-new-buffer "*new*"))))
      (text-mode))

    (defun my-vsplit-last-buffer (prefix)
      "Split the window vertically and display the previous buffer.  Args: PREFIX."
      (interactive "p")
      (split-window-horizontally)
      (other-window 1 nil)
      (when (= prefix 1)
        (set-buffer-major-mode (switch-to-buffer (generate-new-buffer "*new*"))))
      (text-mode))

    (defun my-align-repeat (start end regexp)
      "Repeat alignment with respect to the given regular expression.  Args: START END REGEXP."
      (interactive "r\nsAlign regexp: ")
      (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1 t))

    (defun new-scratch-buffer ()
      "Creates a new scratch buffer."
      (interactive)
      (switch-to-buffer (generate-new-buffer-name "*scratch*"))
      (text-mode))

    (defun quit-emacs () "Quit Emacs." (interactive) (kill-emacs))
    (defalias 'exit-emacs 'quit-emacs)

    (bind-keys ("C-x 2"    . my-hsplit-last-buffer)
               ("C-x -"    . my-hsplit-last-buffer)
               ("C-x -"    . my-hsplit-last-buffer)
               ("C-x C-\-" . my-hsplit-last-buffer)
               ("C-x _"    . my-hsplit-last-buffer)
               ("C-x 3"    . my-vsplit-last-buffer)
               ("C-x \\"   . my-vsplit-last-buffer)
               ("C-x C-\\" . my-vsplit-last-buffer)
               ("C-x |"    . my-vsplit-last-buffer)
               ([escape]   . keyboard-quit)
               ("RET"      . newline-and-indent)
               ("C-M-l"    . indent-region)
               ("<f5>"     . (lambda () (interactive) (find-file user-init-file)))
               ("C-x f"    . find-file)))

  ;; One-line packages
  (use-package f)
  (use-package gcmh :config (gcmh-mode 1))
  (use-package s)
  (use-package dash)
  (use-package queue)
  (use-package bug-hunter)
  (use-package command-log-mode)
  (use-package restclient)
  (use-package iedit)
  (use-package rg :config (rg-enable-default-bindings))
  (use-package wgrep)
  (use-package ranger)
  (use-package rainbow-mode :diminish rainbow-mode)
  (use-package focus)
  (use-package flx)
  (use-package request)
  (use-package vlf :config (require 'vlf-setup))
  (use-package logview)
  (use-package simpleclip :config (simpleclip-mode 1))
  (use-package savehist :config (savehist-mode))
  (use-package async
    :demand t
    :config (async-bytecomp-package-mode 1)
    (dired-async-mode 1)
    (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil))))

 (use-package nerd-icons
   :demand t
   :straight (nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el"))

 (use-package all-the-icons
   :demand t
   :if (display-graphic-p)
   :config (use-package all-the-icons-dired :hook (dired-mode . all-the-icons-dired-mode)))

 (use-package all-the-icons-completion
   :demand t
   :after (marginalia all-the-icons)
   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
   :config (all-the-icons-completion-mode))

 (use-package nerd-icons-completion
   :after nerd-icons
   :config
   (nerd-icons-completion-mode))

 ;;  (use-package doom-themes
 ;;    :config (load-theme 'doom-opera-light t)
 ;;    (doom-themes-org-config))

 ;; (use-package modus-themes
 ;;   :defer nil
 ;;   ;; load the theme files before enabling a theme
 ;;   :init (modus-themes-load-themes)
 ;;   :custom (modus-themes-italic-constructs nil)
 ;;   (modus-themes-bold-constructs nil)
 ;;   (modus-themes-region '(accented bg-only no-extend))
 ;;   ;; OR (modus-themes-load-vivendi)
 ;;   :config (modus-themes-load-operandi))

 (use-package ef-themes
   :demand t
   :config (ef-themes-select 'ef-cyprus))

 (use-package solaire-mode
   :demand t
   :config (solaire-global-mode +1))

 (use-package doom-modeline
   :demand t
   :straight (doom-modeline :type git :host github :repo "seagle0128/doom-modeline")
   :init (setq doom-modeline-minor-modes nil
               doom-modeline-height 20
               doom-modeline-enable-word-count nil
               doom-modeline-modal-icon nil)
   :config (add-hook 'pdf-view-mode-hook #'doom-modeline-set-pdf-modeline)
   (doom-modeline-mode 1))

 ;; PATH
 (use-package exec-path-from-shell
   :config (when (memq window-system '(mac ns))
             (exec-path-from-shell-initialize))
   (when (eq 'windows-nt system-type)
     (setq exec-path (append (parse-colon-path (getenv "PATH"))
                             (parse-colon-path (getenv "USERPROFILE")) exec-path))))

 (use-package proced
   :commands proced
   :bind (("C-M-p" . proced))
   :config
   (setq-default proced-auto-update-flag t)
   (setq proced-auto-update-interval 1
         proced-goal-attribute nil
         proced-enable-color-flag t)
   (add-to-list
    'proced-format-alist
    '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm)))
   (setq-default proced-format 'custom))

 (use-package shell
   :config (when (eq 'windows-nt system-type)
             (setq shell-file-name "bash"))
   (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
   (setenv "SHELL" shell-file-name)
   ;; (setenv "ITERM_SHELL_INTEGRATION_INSTALLED" nil)
   (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

 (use-package view
   :config
   (defun View-goto-line-last (&optional line)
     "goto last line"
     (interactive "P")
     (goto-line (line-number-at-pos (point-max))))

   (define-key view-mode-map (kbd "e") 'View-scroll-half-page-forward)
   (define-key view-mode-map (kbd "u") 'View-scroll-half-page-backward)
   ;; less like
   (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
   (define-key view-mode-map (kbd "?") 'View-search-regexp-backward?)
   (define-key view-mode-map (kbd "g") 'View-goto-line)
   (define-key view-mode-map (kbd "G") 'View-goto-line-last)
   ;; vi/w3m like
   (define-key view-mode-map (kbd "h") 'backward-char)
   (define-key view-mode-map (kbd "j") 'next-line)
   (define-key view-mode-map (kbd "k") 'previous-line)
   (define-key view-mode-map (kbd "l") 'forward-char))

 (use-package which-key
   :demand t
   :config (which-key-setup-minibuffer)
   (which-key-mode))

 (use-package vterm
   :custom (vterm-install t)
   :commands multi-vterm
   :hook (vterm-mode . evil-normal-state)
   (vterm-copy-mode . evil-normal-in-vterm-copy-mode)
   :config
   (use-package multi-vterm)
   (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
   (setq vterm-max-scrollback 10000)
   (advice-add #'vterm--redraw :after (lambda (&rest args) (evil-refresh-cursor evil-state)))
   (defun evil-normal-in-vterm-copy-mode ()
     (if (bound-and-true-p vterm-copy-mode)
         (evil-normal-state)
       (evil-emacs-state)))
   ;; current directory tracking
   (defun my/vterm--set-title-pre (title)
     (let ((dir (string-trim-left (concat (nth 1 (split-string title ":")) "/"))))
       (when (file-directory-p dir)
         (cd-absolute dir))))
   (advice-add 'vterm--set-title :before #'my/vterm--set-title-pre)
   :bind (:map vterm-mode-map
               ("C-y" . vterm-yank)
               ("M-v" . vterm-yank)))

 (use-package auto-dim-other-buffers
   :config
   (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
   (auto-dim-other-buffers-mode t))

 (use-package helpful
   :config
   (global-set-key (kbd "C-h f") #'helpful-callable)
   (global-set-key (kbd "C-h v") #'helpful-variable)
   (global-set-key (kbd "C-h k") #'helpful-key)
   (global-set-key (kbd "C-h C") #'helpful-command)
   (global-set-key (kbd "C-h .") #'helpful-at-point))

 (use-package diff-hl
   :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
          (magit-post-refresh . diff-hl-magit-post-refresh))
   ;; :hook (find-file    . diff-hl-mode)
   ;; :hook (vc-dir-mode  . diff-hl-dir-mode)
   ;; :hook (dired-mode   . diff-hl-dired-mode)
   ;; :hook (diff-hl-mode . diff-hl-flydiff-mode)
   :config (defun diff-hl-next-hunk-cycle (&optional backward)
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

   (setq vc-git-diff-switches '("--histogram")
         diff-hl-flydiff-delay 0.5)
   (global-diff-hl-mode t)
   (diff-hl-margin-mode t)
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

 (use-package lsp-mode
   :custom
   (lsp-completion-provider :none)
   ;; what to use when checking on-save. "check" is default, I prefer clippy
   (lsp-rust-analyzer-cargo-watch-command "clippy")
   (lsp-eldoc-render-all t)
   (lsp-idle-delay 0.6)
   ;; enable / disable the hints as you prefer:
   (lsp-rust-analyzer-server-display-inlay-hints t)
   (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
   (lsp-rust-analyzer-display-chaining-hints t)
   (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
   (lsp-rust-analyzer-display-closure-return-type-hints t)
   (lsp-rust-analyzer-display-parameter-hints nil)
   (lsp-rust-analyzer-display-reborrow-hints nil)
   :config
   (use-package lsp-treemacs)
   (add-hook 'clojure-mode-hook 'lsp)
   (add-hook 'clojurescript-mode-hook 'lsp)
   (add-hook 'clojurec-mode-hook 'lsp)
   (setq lsp-lens-enable nil
         lsp-signature-auto-activate nil)
   ;; Use orderless completion style with lsp-capf instead of the default lsp-passthrough.
   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
         '(orderless))
   (defun corfu-lsp-setup ()
     (setq-local completion-styles '(orderless)
                 completion-category-defaults nil))
   (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
   :bind (:map lsp-mode-map
               ("C-S-h" . lsp-find-definition)
               ("C-."   . lsp-find-definition)))

 (use-package lsp-ui
   :commands lsp-ui-mode
   :custom
   (lsp-ui-peek-always-show t)
   (lsp-ui-sideline-show-hover t)
   (lsp-ui-doc-enable nil))

 ;;(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
 (use-package flycheck-clj-kondo)

 (use-package clojure-mode
   :config (require 'flycheck-clj-kondo)
   (use-package cider)
   (use-package clojure-mode-extra-font-locking)
   ;; (use-package flycheck-clojure
   ;;   :config (flycheck-clojure-setup)
   ;;           (setq flycheck-checkers (delete 'clojure-cider-typed flycheck-checkers)))
   (add-hook 'clojure-mode-hook #'flycheck-mode)
   (add-hook 'clojure-mode-hook #'turn-on-eldoc-mode)
   (add-hook 'cider-mode-hook   #'turn-on-eldoc-mode)
   (setq nrepl-log-messages           t
         nrepl-hide-special-buffers   t
         cider-prefer-local-resources t
         cider-repl-popup-stacktraces t
         cider-popup-stacktraces      nil)
   (dolist (mode '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,mode . "clojure")))
   (defun cider-find-var-no-prompt ()
     "cider-find-var at point without prompt"
     (interactive)
     (cider-find-var t nil))
   :bind (:map clojure-mode-map
               ("C-h"   . cider-doc)
               ("C-S-h" . cider-doc)
               ("C-M-x" . cider-eval-defun-at-point)))

 (use-package rustic
   :bind (:map rustic-mode-map
               ("M-j" . lsp-ui-imenu)
               ("M-?" . lsp-find-references)
               ("C-c C-c l" . flycheck-list-errors)
               ("C-c C-c a" . lsp-execute-code-action)
               ("C-c C-c r" . lsp-rename)
               ("C-c C-c q" . lsp-workspace-restart)
               ("C-c C-c Q" . lsp-workspace-shutdown)
               ("C-c C-c s" . lsp-rust-analyzer-status)
               ("C-M-l"     . rustic-format-buffer))
   :config
   ;; uncomment for less flashiness
   ;; (setq lsp-eldoc-hook nil)
   ;; (setq lsp-enable-symbol-highlighting nil)
   ;; (setq lsp-signature-auto-activate nil)
   ;; comment to disable rustfmt on save
   (setq rustic-format-on-save t)
   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

 (defun rk/rustic-mode-hook ()
   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
   ;; save rust buffers that are not file visiting. Once
   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
   ;; no longer be necessary.
   (when buffer-file-name
     (setq-local buffer-save-without-query t)))

 (use-package dap-mode)
 (use-package json-mode)
 (use-package yasnippet)

 (progn
   ;; JavaScript and ReactJS
   (use-package js2-mode
     :straight nil
     :mode (rx ".js" eos)
     :custom
     (js-indent-level 2)
     (js-switch-indent-offset 2)
     (js2-highlight-level 3)
     (js2-idle-timer-delay 0)
     (js2-mode-show-parse-errors nil)
     (js2-mode-show-strict-warnings nil))

   (use-package rjsx-mode
     :mode (rx (or ".jsx" (and "components/" (* anything) ".js")) eos)
     :hook
     (rjsx-mode . (lambda () (setq me/pretty-print-function #'sgml-pretty-print)))
     (rjsx-mode . sgml-electric-tag-pair-mode))

   (use-package typescript-mode
     :init
     (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
     (add-to-list 'auto-mode-alist `(,(rx ".tsx" eos) . typescript-tsx-mode))
     :config
     (add-hook 'typescript-mode-hook #'sgml-electric-tag-pair-mode)
     (add-hook 'typescript-tsx-mode-hook #'sgml-electric-tag-pair-mode)
     (add-hook 'typescript-mode-hook 'lsp)
     (add-hook 'typescript-tsx-mode-hook 'lsp)
     (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
     (require 'dap-chrome)
     (yas-global-mode)
     :custom
     (typescript-indent-level 2))

   ;; if you use typescript-mode
   ;; (use-package tide
   ;;   :ensure t
   ;;   :after (typescript-mode company flycheck)
   ;;   :hook ((typescript-mode . tide-setup)
   ;;          (typescript-mode . tide-hl-identifier-mode)
   ;;          (before-save . tide-format-before-save)))
   ;; if you use treesitter based typescript-ts-mode (emacs 29+)
   ;; (use-package tide
   ;;   :ensure t
   ;;   :after (company flycheck)
   ;;   :hook ((typescript-ts-mode . tide-setup)
   ;;          (tsx-ts-mode . tide-setup)
   ;;          (typescript-ts-mode . tide-hl-identifier-mode)
   ;;          (before-save . tide-format-before-save)))

   (use-package tsi
     :after tree-sitter
     :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
     ;; define autoload definitions which when actually invoked will cause package to be loaded
     :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
     :init
     (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
     (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
     (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
     (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1)))))

 (use-package geiser
   :config
   (use-package geiser-chibi)
   (use-package geiser-chicken)
   (use-package geiser-gambit)
   (use-package geiser-guile)
   (use-package geiser-kawa)
   (use-package geiser-racket)
   (setq geiser-debug-show-debug-p nil
         geiser-debug-jump-to-debug-p nil))

 (use-package racket-mode)

 (use-package rainbow-delimiters
   :config (setq rainbow-delimiters-max-face-count 9
                 rainbow-delimiters-outermost-only-face-count 8)
   :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
          (clojure-mode . rainbow-delimiters-mode)
          (lisp-mode . rainbow-delimiters-mode)
          (scheme-mode . rainbow-delimiters-mode)
          (racket-mode . rainbow-delimiters-mode)))

 ;; SLY
 (use-package sly-quicklisp :after sly)
 (use-package sly
   :config (setq sly-lisp-implementations
                 `(
                   ;; (roswell ("ros" "-L" "sbcl" "-Q" "-l" "~/.sbclrc" "--core" "~/roswell.core-for-sly" "run")  :coding-system utf-8-unix)
                   (sbcl    ("/usr/local/bin/sbcl" "--noinform" "--no-linedit" "--core" "~/sbcl.core-for-sly" "run") :coding-system utf-8-unix)
                   (abcl    ("/usr/local/bin/abcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)))
   (add-hook 'sly-mrepl-mode-hook #'turn-off-evil-mode)
   :bind (:map sly-mode-map
               ("C-S-h" . sly-describe-symbol)))
 (provide 'init-sly)

 (use-package highlight-quoted
   :hook ((emacs-lisp-mode . highlight-quoted-mode)
          (clojure-mode . highlight-quoted-mode)
          (lisp-mode . highlight-quoted-mode)
          (racket-mode . highlight-quoted-mode)))

 (use-package anzu :config (global-anzu-mode +1))

 (use-package vundo
   :commands (vundo)
   :straight (vundo :type git :host github :repo "casouri/vundo")
   :config
   ;; Take less on-screen space.
   (setq vundo-compact-display t)
   ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
   (define-key vundo-mode-map (kbd "l")       #'vundo-forward)
   (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
   (define-key vundo-mode-map (kbd "h")       #'vundo-backward)
   (define-key vundo-mode-map (kbd "<left>")  #'vundo-backward)
   (define-key vundo-mode-map (kbd "j")       #'vundo-next)
   (define-key vundo-mode-map (kbd "<down>")  #'vundo-next)
   (define-key vundo-mode-map (kbd "k")       #'vundo-previous)
   (define-key vundo-mode-map (kbd "<up>")    #'vundo-previous)
   (define-key vundo-mode-map (kbd "<home>")  #'vundo-stem-root)
   (define-key vundo-mode-map (kbd "<end>")   #'vundo-stem-end)
   (define-key vundo-mode-map (kbd "q")       #'vundo-quit)
   (define-key vundo-mode-map (kbd "C-g")     #'vundo-quit)
   (define-key vundo-mode-map (kbd "RET")     #'vundo-confirm))
 (with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "C-M-u") 'vundo))

 (use-package lispy
   :init (setq lispy-key-theme '(special lispy))
   :config
   (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
   (add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
   (defun conditionally-enable-lispy ()
     (when (eq this-command 'eval-expression)
       (lispy-mode 1)))
   (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
   :hook ((emacs-lisp-mode . lispy-mode)
          (lisp-mode . lispy-mode)
          (common-lisp-mode . lispy-mode)))

 (use-package vertico
   :demand t
   :custom-face (vertico-current ((t (:inherit hl-line :weight bold))))
   ;; Correct file path when changed
   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
   :config (setq vertico-cycle t)
   ;; Prefix current candidate with arrow
   (defun prefix-current-candidate-with-arrow (orig cand prefix suffix index _start)
     (setq cand (funcall orig cand prefix suffix index _start))
     (concat
      (if (and (= vertico--index index))
          (propertize "> " 'face 'vertico-current)
        "  ")
      cand))
   (advice-remove #'vertico--format-candidate #'prefix-current-candidate-with-arrow)
   (advice-add #'vertico--format-candidate :around #'prefix-current-candidate-with-arrow)
   (vertico-mode)
   :bind (:map vertico-map
               ("C-j" . vertico-next)
               ("C-k" . vertico-previous)))

 (use-package evil
   :demand t
   :init (viper-go-away)
   (setq evil-undo-system 'undo-redo)
   :config (use-package evil-anzu)
   (use-package evil-org)
   (use-package evil-numbers)
   (use-package evil-surround   :config (global-evil-surround-mode 1))
   (use-package evil-visualstar :config (global-evil-visualstar-mode))
   (use-package evil-search-highlight-persist :config (global-evil-search-highlight-persist t))
   (use-package lispyville :hook (lispy-mode . lispyville-mode))
   (use-package evil-commentary
     :config (defun my-comment-line-and-go-to-next ()
               "Comment current line and go to next."
               (interactive)
               (evil-commentary-line (line-beginning-position)
                                     (line-end-position)
                                     'line)
               (evil-next-line))
     (define-key evil-motion-state-map (kbd "gc") 'evil-commentary)
     (bind-key "C-/" 'my-comment-line-and-go-to-next)
     (bind-key "C-/" 'evil-commentary evil-visual-state-map)
     (evil-commentary-mode))
   ;; Emacs keys in INSERT mode
   (setcdr evil-insert-state-map nil)
   (setq evil-move-cursor-back t
         evil-default-cursor t
         evil-want-C-u-scroll t
         evil-want-C-w-delete t
         evil-want-minibuffer t
         evil-want-fine-undo t
         evil-want-Y-yank-to-eol t
         evil-search-module 'evil-search
         evil-normal-state-cursor '(box "purple")
         evil-insert-state-cursor '(bar "purple")
         evil-visual-state-cursor '(box "green"))
   ;; treat symbol as a word
   (defalias #'forward-evil-word #'forward-evil-symbol)
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
   (bind-keys :map evil-normal-state-map
              ([next]   . evil-scroll-down)
              ([prior]  . evil-scroll-up)
              ([escape] . keyboard-quit)
              ("j"      . evil-next-visual-line)
              ("k"      . evil-previous-visual-line)
              ("C-y"    . evil-paste-after)
              ("SPC"    . hydra-common-commands/body))
   (bind-keys :map evil-visual-state-map
              ("SPC" . hydra-common-commands/body))
   (defun my-evil-off ()
     "Turn 'evil-mode' off and change cursor type to bar."
     (interactive)
     (turn-off-evil-mode)
     (setq cursor-type 'bar))
   (with-eval-after-load 'term (evil-set-initial-state 'term-mode 'insert))
   (with-eval-after-load 'vterm  (evil-set-initial-state 'term-mode 'emacs))
   :bind (("<f12>" . evil-local-mode)
          :map evil-normal-state-map
          ("C-e" . move-end-of-line)
          ("M-v" . simpleclip-paste)
          :map evil-visual-state-map
          ("C-e" . move-end-of-line)
          ("M-c" . simpleclip-copy)
          :map evil-insert-state-map
          ("M-v" . simpleclip-paste)
          ("M-c" . simpleclip-copy))
   :hook ((help-mode . evil-local-mode)
          (prog-mode . evil-local-mode)
          (text-mode . evil-local-mode)
          (pdf-view-mode . evil-local-mode)
          (neotree-mode . evil-local-mode)
          ;; Disable evil-mode in some major modes
          (magit-mode . my-evil-off)
          (erc-mode . my-evil-off)
          (nrepl-connected . my-evil-off)))

 (use-package evil-collection
   :after evil
   :config (evil-collection-init '(cider compile debug diff-hl diff-mode dired
                                         doc-view ediff eldoc elisp-mode elisp-refs eshell
                                         geiser ibugger imenu imenu-list log-view magit
                                         org pdf scheme sly typescript-mode vdiff vterm vundo
                                         which-key)))

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
   :hook ((emacs-lisp-mode . turn-on-eldoc-mode)
          (lisp-interaction-mode . turn-on-eldoc-mode)
          (ielm-mode . turn-on-eldoc-mode)))

 (use-package consult
   :demand t
   :config (use-package consult-lsp
             :bind (("M-<f3>" . consult-lsp-diagnostics)
                    ("M-<f4>" . consult-lsp-symbols)
                    ("M-<f5>" . consult-lsp-file-symbols)))
   (defun consult-line-evil-history (&rest _)
     "Add latest `consult-line' search pattern to the evil search history ring.
This only works with orderless and for the first component of the search."
     (when (and ;; (bound-and-true-p evil-mode)
            (eq evil-search-module 'evil-search))
       (let ((pattern (car (orderless-pattern-compiler (car consult--line-history)))))
         (add-to-history 'regexp-search-ring pattern regexp-search-ring-max)
         (add-to-history 'evil-ex-search-history pattern)
         (setq evil-ex-search-pattern (list pattern t t))
         (setq evil-ex-search-direction 'forward)
         (when evil-ex-search-persistent-highlight
           (evil-ex-search-activate-highlight evil-ex-search-pattern)))))
   (advice-add #'consult-line :after #'consult-line-evil-history)
   :bind (("\C-s"  . consult-line)
          ("M-s o" . consult-line-multi)
          ("C-S-f" . consult-find)))

 (use-package marginalia
   :demand t
   :custom (marginalia-max-relative-age 0)
   ;; The :init configuration is always executed (Not lazy!)
   ;; Must be in the :init section of use-package such that the mode gets
   ;; enabled right away. Note that this forces loading the package.
   :config (marginalia-mode)
   :bind (("M-A" . marginalia-cycle)
          :map minibuffer-local-map
          ("M-A" . marginalia-cycle)))

 (use-package orderless
   :demand t
   :init (setq completion-styles '(orderless flex)
               completion-category-defaults nil
               completion-category-overrides '((file (styles partial-completion)))))

 (use-package corfu
   :demand t
   :straight (corfu :files (:defaults "extensions/*")
                    :includes (corfu-info corfu-popupinfo corfu-history))
   :init
   (setq corfu-cycle t
         corfu-auto t
         corfu-auto-prefix 2
         corfu-auto-delay 0.25
         corfu-min-width 60
         corfu-max-width corfu-min-width      ; Always have the same width
         corfu-count 14
         corfu-scroll-margin 4
         corfu-preview-current nil
         corfu-popupinfo-delay 0.2
         corfu-quit-at-boundary t
         corfu-quit-no-match 'separator
         ;; Do not preview current candidate
         corfu-preview-current 'insert
         corfu-preselect-first nil
         ;; Don't auto expand tempel snippets
         corfu-on-exact-match nil)
   :config
   (global-corfu-mode)
   (corfu-popupinfo-mode)
   ;; Make Evil and Corfu play nice
   (evil-make-overriding-map corfu-map)
   (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
   (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
   (set-face-attribute 'corfu-default nil :height 0.9)
   :bind (:map corfu-map
               ("TAB"     . corfu-next)
               ([tab]     . corfu-next)
               ("C-j"     . corfu-next)
               ("S-TAB"   . corfu-previous)
               ("C-k"     . corfu-previous)
               ([backtab] . corfu-previous)
               ("<escape>" . corfu-quit)
               ("<return>" . corfu-insert)))

 (use-package kind-icon
   :demand t
   :after corfu
   :custom
   (kind-icon-use-icons t)
   (kind-icon-default-face 'corfu-default)
   (kind-icon-blend-background nil)
   (kind-icon-blend-frac 0.08)
   :config
   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

 (use-package projectile
   :diminish projectile-mode
   :config (defun my-projectile-switch-to-project ()
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
         projectile-enable-caching t
         projectile-switch-project-action 'my-projectile-switch-to-project)
   (when (eq system-type 'windows-nt)
     (setq projectile-indexing-method 'alien
           projectile-enable-caching  nil))
   (projectile-global-mode)

   (defun my-find-file ()
     "If currently in a project, then use Projectile to fuzzy find a file.
Use `find-file' otherwise."
     (interactive)
     (require 'projectile)
     (if (projectile-project-p)
         (projectile-find-file)
       (find-file)))
   :bind (("C-S-p" . projectile-switch-project)
          ("C-S-n" . my-find-file)))

 (use-package org
   :config (setq org-edit-src-content-indentation 0
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
                 ;; org-babel-clojure-backend 'cider
                 org-agenda-files (cl-remove-if-not 'file-exists-p '("~/org/todo.org")))
   (set-face-attribute 'org-level-1 nil :height 1.0)
   ;; (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
   ;;                                                          (java       . t)
   ;;                                                          (clojure    . t)
   ;;                                                          (scheme     . t)
   ;;                                                          (sql        . t))
   ;;                              )
   :bind (("\C-cl" . org-store-link)
          ("\C-ca" . org-agenda)
          ("\C-cb" . org-iswitchb)))

 (use-package magit
   :config (use-package magit-popup)
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
     (my-magit-checkout-current-file "--theirs"))
   :bind (("C-x g" . magit-status)))

 (use-package ediff
   :config (setq ediff-window-setup-function 'ediff-setup-windows-plain
                 ediff-split-window-function 'split-window-horizontally
                 ediff-diff-options          "-w")
   ;; Restore window layout
   (defun my-toggle-ediff-wide-display ()
     "Turn off wide-display mode (if was enabled) before quitting ediff."
     (when ediff-wide-display-p
       (ediff-toggle-wide-display)))
   (add-hook 'ediff-cleanup-hook 'my-toggle-ediff-wide-display)
   (add-hook 'ediff-quit-hook    'winner-undo))

 (use-package eshell
   :config (setq eshell-prompt-regexp "^[^αλ\n]*[αλ] "
                 eshell-highlight-prompt nil
                 eshell-save-history-on-exit t
                 eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
   (add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
   (defun eshell/clear-scrollback ()
     "Clear the scrollback content of the eshell window."
     (let ((inhibit-read-only t))
       (erase-buffer)))
   (defalias 'open 'find-file-other-window)
   (defalias 'clean 'eshell/clear-scrollback))

 (use-package web-mode
   :config (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode)))

 (use-package flycheck
   :config (use-package flycheck-pos-tip
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
   (define-key flycheck-mode-map (kbd "<f3>") #'flycheck-list-errors))

 (use-package neotree
   :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
   (define-key neotree-mode-map (kbd "<tab>") #'neotree-enter)
   (defun neotree-current-file ()
     "Opens current file in Neotree. If current buffer is a Neotree buffer then closes it."
     (interactive)
     (if (eq major-mode 'neotree-mode)
         (neotree-toggle)
       (neotree-find (buffer-file-name))))
   :bind (("M-<f1>". neotree-current-file)))

 (use-package imenu-list
   :config (setq imenu-list-position 'left
                 imenu-list-size 36
                 imenu-list-focus-after-activation t)
   :bind (("M-<f2>" . imenu-list-smart-toggle))
   :hook (imenu-list-major-mode . turn-on-evil-mode))

 (use-package eyebrowse
   :diminish eyebrowse-mode
   :config (eyebrowse-mode t)
   (eyebrowse-setup-evil-keys)
   (setq eyebrowse-new-workspace t
         eyebrowse-close-window-config-prompt t)
   :bind (("C-1" . eyebrowse-switch-to-window-config-1)
          ("C-2" . eyebrowse-switch-to-window-config-2)
          ("C-3" . eyebrowse-switch-to-window-config-3)
          ("C-4" . eyebrowse-switch-to-window-config-4)
          ("C-5" . eyebrowse-switch-to-window-config-5)
          ("C-6" . eyebrowse-switch-to-window-config-6)
          ("C-7" . eyebrowse-switch-to-window-config-7)
          ("C-8" . eyebrowse-switch-to-window-config-8)
          ("C-9" . eyebrowse-switch-to-window-config-9)
          ("C-0" . eyebrowse-switch-to-window-config-0)))

 (use-package shackle
   :config (setq shackle-lighter ""
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
   (shackle-mode t))

 ;; Solidity
 (use-package solidity-mode)
 (use-package solidity-flycheck
   :config (setq solidity-flycheck-solc-checker-active t
                 solidity-flycheck-solium-checker-active t))

 (use-package tree-sitter
   :config (require 'tree-sitter)
   (require 'tree-sitter-hl)
   (global-tree-sitter-mode)
   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

 (use-package tree-sitter-langs
   :ensure t
   :after tree-sitter)

 (use-package pdf-tools
   :commands (pdf-view-mode pdf-tools-install)
   :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
   :magic ("%PDF" . pdf-view-mode)
   :config
   (pdf-tools-install :no-query)
   (setq-default pdf-view-display-size 'fit-page)
   (setq pdf-view-continuous nil)
   ;; :hook ((pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
   ;;        (pdf-view-mode-hook . (lambda () (blink-cursor-mode -1)))
   ;;        (pdf-view-mode-hook . pdf-tools-enable-minor-modes)
   ;;        (pdf-view-mode-hook . pdf-misc-size-indication-minor-mode))
   :bind (:map pdf-view-mode-map
               ;; From Spacemacs
               ;; Navigation
               ("j"  . pdf-view-next-line-or-next-page)
               ("k"  . pdf-view-previous-line-or-previous-page)
               ("l"  . image-forward-hscroll)
               ("h"  . image-backward-hscroll)
               ("J"  . pdf-view-next-page)
               ("K"  . pdf-view-previous-page)
               ("u"  . pdf-view-scroll-down-or-previous-page)
               ("d"  . pdf-view-scroll-up-or-next-page)
               ("0"  . image-bol)
               ("$"  . image-eol)
               ;; Scale/Fit
               ("W"  . pdf-view-fit-width-to-window)
               ("H"  . pdf-view-fit-height-to-window)
               ("P"  . pdf-view-fit-page-to-window)
               ("m"  . pdf-view-set-slice-using-mouse)
               ("b"  . pdf-view-set-slice-from-bounding-box)
               ("R"  . pdf-view-reset-slice)
               ("zr" . pdf-view-scale-reset)
               ;; Actions
               ("s"  . pdf-occur)
               ("O"  . pdf-outline)
               ("p"  . pdf-misc-print-document)
               ("o"  . pdf-links-action-perform)
               ("r"  . pdf-view-revert-buffer)
               ("n"  . pdf-view-midnight-minor-mode)))

 (use-package pdf-view-restore
   :after pdf-tools
   :hook (pdf-view-mode . pdf-view-restore-mode))

 ;; TODO Remove Hydra and just use which-key or general?
 ;; Hydras
 (use-package hydra
   :config (defhydra hydra-find
             (:color blue :hint nil)
             "
  Find
  -----
  _h_: at point         _f_: file      _F_: function   _s_: line
  _k_: function on key  _v_: variabl   _l_: library    _g_: ripgrep
  "
             ("h"   my-find-thing-at-point)
             ("F"   find-function)
             ("s"   consult-line)
             ("f"   find-file)
             ("k"   find-function-on-key)
             ("v"   find-variable)
             ("l"   find-library)
             ("g"   consult-ripgrep)
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
     ("v" describe-variable "variable")
     ("f" describe-function "function")
     ("F" describe-face "face")
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
     ("q" nil "quit" :color blue))

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
     ("b" ibuffer :color blue)
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
     ("b" switch-to-buffer)
     ("k" kill-buffer)
     ("B" bury-buffer)
     ("SPC" previous-buffer)
     ("q" nil "quit"))

   (defhydra hydra-metahelp-menu (:hint nil :exit t :foreign-keys warn)
     "
  Describe                           ^^^^^^                             Goto         ^^ View
  -----------------------------------------------------------------------------------------------------
  _b_: bindings             _k_:   key                   _s_:   symbol               _e_: *Messages*
  _c_: key-briefly          _K_:   Key (info)            _S_:   Symbol (info)        _i_: info manual
  _C_: coding system        _L_:   Language environment  _C-s_: syntax table         _._: local help
  _d_: documentation        _m_:   mode                  _v_:   variable             _l_: lossage
  _E_: Emacs...             _p_:   package (by topic)    _w_:   whereis (func->keys)
  _f_: function             _P_:   Package (by name)
  _F_: Function (info)      _C-p_: external package
  _I_: key input method                                           ^^^^^^                 _q_: quit
  "
     ("b"   describe-bindings)
     ("c"   describe-key-briefly)
     ("C"   describe-coding-system)
     ("d"   apropos-documentation)
     ("e"   view-echo-area-messages)
     ("E"   hydra-metahelp-emacs-menu/body)
     ("f"   describe-function)
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
     ("SPC" (lambda ()
              (interactive)
              (evil-ex-nohighlight)
              (evil-search-highlight-persist-remove-all)))
     ("u"   vundo)
     ("P"   hydra-project/body)
     ("D"   hydra-ediff/body)
     ("x"   M-x)
     ("b"   hydra-buffers/body)
     ("."   find-file)
     ("q"   nil "quit"))
   :bind (("M-SPC" . hydra-common-commands/body)))

 ;; Allow font-lock-mode to do background parsing and restore some settings
 (setq jit-lock-stealth-time 1
       jit-lock-chunk-size 1000
       jit-lock-defer-time 0.05
       debug-on-error nil
       debug-on-quit nil
       gc-cons-threshold (* 10 1024 1024)
       gc-cons-percentage 0.1
       read-process-output-max (* 1024 1024)))
(provide 'init)
;;; init.el ends here
