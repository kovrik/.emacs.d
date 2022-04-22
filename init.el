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

;; Package
;; (require 'package)
;; (setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ;; ("org"          . "http://orgmode.org/elpa/")
                         ;; ("melpa"        . "http://melpa.org/packages/")
                         ;; ("melpa-stable" . "http://stable.melpa.org/packages/"))
;;       package-enable-at-startup nil)
;; (package-initialize)
;; (add-to-list 'package-pinned-packages '(queue . "gnu"))
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Make sure to have downloaded archive description
;; (or (file-directory-p (expand-file-name (concat package-user-dir "/archives")))
;;     (package-refresh-contents))

;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; (when (not (package-installed-p 'use-package))
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-when-compile (require 'use-package))
;;(require 'diminish)
(require 'bind-key)
(require 'uniquify)
;; (setq use-package-verbose t
;;       load-prefer-newer t
;;       use-package-always-ensure t)
;; (use-package auto-compile :config (auto-compile-on-load-mode))

;; (defun my-ensure-packages-installed (packages)
;;   "Assure every package in PACKAGES is installed, ask for installation if it s not.  Return a list of installed packages or nil for every skipped package."
;;   (dolist (p packages)
;;     (when (and (not (package-installed-p p))
;;                (y-or-n-p (format "Package %s is missing.  Install it? " p)))
;;       (package-install p))))

;; (my-ensure-packages-installed '(queue async browse-kill-ring dash epl f fold-dwim fringe-helper goto-chg highlight highlight-escape-sequences idle-highlight-mode markdown-mode pkg-info s))

(use-package queue)
(use-package async)
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

(defun my-add-hooks (hooks function)
  "For each hook in HOOKS list bind FUNCTION."
  (dolist (hook hooks)
    (add-hook hook function)))

;; Globals
;; UTF-8 Everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when window-system
  (tooltip-mode    -1)
  (tool-bar-mode   -1)
  (menu-bar-mode   -1)
  (scroll-bar-mode -1)
  (toggle-horizontal-scroll-bar -1))

(setq-default indent-tabs-mode nil
              tab-width 2
              find-file-visit-truename t
              mode-require-final-newline nil
              major-mode 'text-mode
              pdf-view-display-size 'fit-page)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      frame-inhibit-implied-resize t
      scroll-margin 5
      scroll-conservatively 30
      scroll-step 1
      sentence-end-double-space nil
      ring-bell-function 'ignore
      use-dialog-box nil
      visible-bell nil
      pdf-view-use-scaling 1
      uniquify-buffer-name-style 'forward
      show-trailing-whitespace t
      ns-use-srgb-colorspace nil
      gnutls-min-prime-bits 4096             ;; remove the warnings from the GnuTLS library when using HTTPS
      tab-always-indent 'complete
      search-default-mode #'char-fold-to-regexp
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      load-prefer-newer t                    ;; Always load newest byte code
      large-file-warning-threshold 200000000 ;; warn when opening files bigger than 200MB
      global-auto-revert-non-file-buffers t  ;; Also auto refresh dired, but be quiet about it
      auto-revert-verbose nil
      version-control t                      ;; Version control
      vc-follow-symlinks t                   ;; don't ask for confirmation when opening symlinked file
      delete-old-versions t                  ;; delete excess backup versions silently
      vc-make-backup-files t                 ;; make backups file even when in version controlled dir
      create-lockfiles nil
      custom-file "~/.emacs.d/custom.el")

(defalias 'list-buffers 'ibuffer)            ;; make ibuffer the default buffer lister.
(defalias 'xml-pretty-print 'sgml-pretty-print)
;;(fringe-mode '(7 . 0))
(column-number-mode)
(desktop-save-mode)
(global-font-lock-mode)
(global-hl-line-mode)
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
           ("M-o"      . ace-window)
           ("M-SPC"    . hydra-common-commands/body)
           ("C-M-l"    . indent-region)
           ("<f5>"     . (lambda () (interactive) (find-file user-init-file)))
           ("M-<f1>"   . dired-jump))

(load custom-file :noerror :nomessage)
(use-package doom-themes
  :config (progn
            (load-theme 'doom-nord t)
            (doom-themes-org-config)
            (global-hl-line-mode)
            (set-face-background 'hl-line "#404960")
            (set-face-background 'region  "#606980")))
(use-package solaire-mode
             :config (solaire-global-mode +1))

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
            (setenv "ITERM_SHELL_INTEGRATION_INSTALLED" nil)
            (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

;; Fonts
(let ((my-font (cl-find-if (lambda (f) (and f (member (font-get f :name) (font-family-list))))
                           (list
                            (font-spec :name "Monaco"     :size 10)
                            (font-spec :name "Meslo LG S" :size 11)
                            (font-spec :name "Consolas"   :size 11)))))
  (when my-font
    (message (format "Using %s %s font." (font-get my-font :name) (font-get my-font :size)))
    ;; (set-face-attribute 'default nil :font my-font)
    ;; (set-frame-font      my-font  nil t)
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
(use-package rainbow-mode :defer t :diminish rainbow-mode)
(use-package focus :defer t)
(use-package color-theme :defer t :config (color-theme-initialize))
(use-package diff-hl
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

            (bind-keys :map diff-hl-mode-map
                       ("C-x v n" . diff-hl-next-hunk-cycle)
                       ("C-x v j" . diff-hl-next-hunk-cycle)
                       ("C-x v p" . diff-hl-previous-hunk-cycle)
                       ("C-x v k" . diff-hl-previous-hunk-cycle)
                       ("C-x v r" . diff-hl-revert-hunk))
            (global-diff-hl-mode t)))
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
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
  :bind (("C-S-h" . my-find-thing-at-point)
         ("C-h f" . find-function)
         ("C-h k" . find-function-on-key)
         ("C-h v" . find-variable)
         ("C-h l" . find-library)))

(use-package doom-modeline
             :ensure t
             :init (doom-modeline-mode 1)
             :config (setq doom-modeline-minor-modes t))

(use-package smartparens
  :config (progn
              (require 'smartparens-config)
              (use-package evil-smartparens)
              (show-smartparens-global-mode t)
              (smartparens-strict-mode)
            (bind-keys :map smartparens-mode-map
                       ;; FIXME Make it work with evil-mode
                       ((kbd "C-<right>")     . sp-forward-slurp-sexp)
                       ((kbd "C-<left>")      . sp-forward-barf-sexp)
                       ((kbd "C-M-<right>")   . sp-backward-slurp-sexp)
                       ((kbd "C-M-<left>")    . sp-backward-barf-sexp)
                       ((kbd "M-<delete>")    . sp-unwrap-sexp)
                       ((kbd "M-<backspace>") . sp-unwrap-sexp)
                       ((kbd "C-M-t")         . sp-transpose-sexp))
            (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
            (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

(use-package clojure-mode
  :defer  t
  ;; :pin melpa-stable
  :config (progn
            (use-package cider
              ;; :pin melpa-stable
                         :defer t)
            (use-package clojure-mode-extra-font-locking)
            (use-package flycheck-clojure
              ;; :pin melpa-stable
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
            (bind-keys :map clojure-mode-map ((kbd "C-h")     . cider-find-var-no-prompt)
                                             ((kbd "C-S-h")   . cider-find-var-no-prompt)
                                             ((kbd "C-M-x")   . cider-eval-defun-at-point))))

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
(use-package slime
  :ensure t
  :config (progn
            (use-package slime-company :defer t)
            (setq byte-compile-warnings '(cl-functions))
            (load (expand-file-name "~/.quicklisp/slime-helper.el"))
            (setq inferior-lisp-program "sbcl")
            (slime-setup '(slime-fancy slime-company slime-asdf slime-indentation slime-sbcl-exts slime-scratch))
            (setq lisp-indent-function 'common-lisp-indent-function)
            (setq common-lisp-style-default "modern")

            ;; FIXME (add-hook 'slime-mode-hook #'common-lisp-mode)

            (defun window-with-name-prefix-live-p (name)
              (cl-some (lambda (buffer-name)
                         (string-prefix-p name buffer-name))
                       (mapcar #'buffer-name
                               (mapcar #'window-buffer
                                       (window-list)))))

            (defun open-slime ()
              (interactive)
              (let* ((buffer-prefix "*slime-repl"))
                (unless (window-with-name-prefix-live-p buffer-prefix)
                  (let* ((buffer-name-list (mapcar #'buffer-name
                                                   (buffer-list)))
                         (buffer (cl-loop for buffer-name in (mapcar #'buffer-name (buffer-list))
                                          until (string-match-p (regexp-quote buffer-prefix)
                                                                buffer-name)
                                          finally (if (string-match-p (regexp-quote buffer-prefix)
                                                                      buffer-name)
                                                      (cl-return buffer-name)
                                                    nil))))
                    (if (not buffer)
                        (slime)
                      (other-window 1))))))))

(use-package slime-company
  :after (slime company)
  :ensure t
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space)
  ;; We redefine this function to call SLIME-COMPANY-DOC-MODE in the buffer
  (defun slime-show-description (string package)
    (let ((bufname (slime-buffer-name :description)))
      (slime-with-popup-buffer (bufname :package package
					                              :connection t
					                              :select slime-description-autofocus)
	      (when (string= bufname "*slime-description*")
	        (with-current-buffer bufname (slime-company-doc-mode)))
	      (princ string)
	      (goto-char (point-min))))))

;; SLY
;; (use-package sly-quicklisp
;;                :after sly)

;; (use-package sly
;;              :config (setq sly-lisp-implementations
;;                            `((sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)))
;;  (evil-set-initial-state 'sly-mrepl-mode 'emacs))

;; (provide 'init-sly)

(use-package evil
  :config (progn
            (use-package evil-org :defer t)
            (use-package evil-numbers)
            (use-package evil-surround   :config (global-evil-surround-mode 1))
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
            (evil-set-undo-system 'undo-tree)
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
            (global-set-key                             (kbd "C-x x")   'evil-ex)
            (global-set-key                             (kbd "C-x C-x") 'evil-ex)
            (global-set-key                             [escape] 'evil-exit-emacs-state)
            (define-key evil-visual-state-map           [escape] 'keyboard-quit)
            (define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)
            (define-key evil-insert-state-map           [escape] 'evil-normal-state)
            (define-key evil-insert-state-map           (kbd "C-n") 'company-complete)
            (bind-keys :map evil-normal-state-map ([next]   . evil-scroll-down)
                                                  ([prior]  . evil-scroll-up)
                                                  ([escape] . keyboard-quit)
                                                  ("j"      . evil-next-visual-line)
                                                  ("k"      . evil-previous-visual-line)
                                                  ("SPC" . hydra-common-commands/body))
            (bind-keys :map evil-visual-state-map ("SPC" . hydra-common-commands/body))
            (my-add-hooks '(help-mode-hook prog-mode-hook text-mode-hook) #'evil-local-mode)
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
                          #'my-evil-off)
            (with-eval-after-load 'term
              (evil-set-initial-state 'term-mode 'emacs))))

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init (my-add-hooks '(emacs-lisp-mode-hook
                        lisp-interaction-mode-hook
                        ielm-mode-hook)
                      #'turn-on-eldoc-mode))

(use-package flx)
(use-package flx-ido)

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
;;              :config (selectrum-mode +1)
;;                      (selectrum-prescient-mode +1))
;; TODO embark?
;; TODO consult?

(use-package swiper
  :demand t
  :diminish ivy-mode
  :config (progn
            (use-package ivy
              :config (progn
                        (setq ivy-re-builders-alist '((swiper      . ivy--regex-plus)
                                                      (t           . ivy--regex-plus)))
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
            (use-package counsel)
            (use-package ivy-rich
                         :config (ivy-rich-mode 1)
                         (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)))
  :bind (("\C-s"    . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x"     . counsel-M-x)
         ("M-s o"   . swiper-multi)
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

(use-package ace-window
  :ensure t
  :defer 1
  :config
  (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
  (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist '((?x aw-delete-window "Ace - Delete Window")
                            (?c aw-swap-window "Ace - Swap Window")
                            (?n aw-flip-window)
                            (?v aw-split-window-vert "Ace - Split Vert Window")
                            (?h aw-split-window-horz "Ace - Split Horz Window")
                            (?m delete-other-windows "Ace - Maximize Window")
                            (?g delete-other-windows)
                            (?b balance-windows)
                            (?u (lambda () (progn (winner-undo) (setq this-command 'winner-undo))))
                            (?r winner-redo))))

;; TODO add more hydras
(use-package hydra
  :config (progn
            (defhydra hydra-undo-tree
              (:color yellow :hint nil)
"
Undo Tree
-------------------
_k_: undo    _s_: save
_j_: redo    _l_: load

"
              ("k"   undo-tree-undo)
              ("j"   undo-tree-redo)
              ("s"   undo-tree-save-history)
              ("l"   undo-tree-load-history)
              ("v"   undo-tree-visualize "visualize" :color blue)
              ("q"   nil "quit" :color blue))

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
              ("s"   swiper)
              ("f"   counsel-find-file)
              ("k"   find-function-on-key)
              ("v"   find-variable)
              ("l"   find-library)
              ("a"   counsel-ag)
              ("g"   counsel-grep)

              ("q"   nil "quit" :color blue))

            (defhydra hydra-zoom
              (:hint nil)
"
Zoom           Region
--------------------------
_j_: zoom in     _n_: narrow
_k_: zoom out    _w_: widen

"
              ("j" text-scale-increase)
              ("k" text-scale-decrease)
              ("n" narrow-to-region :color blue)
              ("w" widen :color blue)
              ("q"   nil "quit" :color blue))

            (defhydra hydra-packages
              (:color blue :hint nil)
"
Packages
---------
_l_: list  _r_: refresh  "
              ("l" list-packages)
              ("r" package-refresh-contents)
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
_b_: switch buffer
"
              ("b" ivy-switch-buffer)
              ("q" nil "quit"))

            (defhydra hydra-common-commands
              (:color blue :hint nil)
"

_SPC_: no highlight    _f_: find        _i_: indent    _s_: delete trailing whitepsaces
_P_:   project         _d_: describe    _a_: align     _D_: ediff
_p_:   packages        _e_: evaluate    _w_: window    _z_: zoom
_b_:   buffers         _x_: execute
"
              ("SPC" evil-search-highlight-persist-remove-all)
              ("f"   hydra-find/body)
              ("i"   indent-region)
              ("w"   hydra-window/body)
              ("d"   hydra-describe/body)
              ("a"   align-regexp)
              ("s"   delete-trailing-whitespace)
              ("u"   hydra-undo-tree/body)
              ("P"   hydra-project/body)
              ("p"   hydra-packages/body)
              ("z"   hydra-zoom/body)
              ("D"   hydra-ediff/body)
              ("e"   hydra-eval/body)
              ("x"   counsel-M-x)
              ("b"   hydra-buffers/body)
              ("q"   nil "quit"))))

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
  ;; :pin gnu
  :diminish company-mode
  :config (progn
            (use-package company-quickhelp
              :config (progn (setq company-quickhelp-delay 0.7)
                             (company-quickhelp-mode 1)))
            (use-package company-flx
              :config (with-eval-after-load 'company
                        (company-flx-mode +1)))
            (setq company-show-numbers t
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
                  git-commit-finish-query-functions '())

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
  :config (setq eshell-save-history-on-exit t
                eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"))

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

(use-package undo-tree
  :diminish undo-tree-mode
  :config (progn
            (global-undo-tree-mode)
            (setq undo-tree-visualizer-timestamps t
                  undo-tree-visualizer-diff       t)
            (define-key undo-tree-map (kbd "C-/") nil)))

(use-package eyebrowse
  :diminish eyebrowse-mode
  ;; :pin melpa-stable
  :config (progn
            (eyebrowse-mode t)
            (eyebrowse-setup-evil-keys)
            (setq eyebrowse-new-workspace t
                  eyebrowse-close-window-config-prompt t)
            (bind-keys ("C-1"  . eyebrowse-switch-to-window-config-1)
                       ("C-2"  . eyebrowse-switch-to-window-config-2)
                       ("C-3"  . eyebrowse-switch-to-window-config-3)
                       ("C-4"  . eyebrowse-switch-to-window-config-4)
                       ("C-5"  . eyebrowse-switch-to-window-config-5)
                       ("C-6"  . eyebrowse-switch-to-window-config-6)
                       ("C-7"  . eyebrowse-switch-to-window-config-7)
                       ("C-8"  . eyebrowse-switch-to-window-config-8)
                       ("C-9"  . eyebrowse-switch-to-window-config-9)
                       ("C-0"  . eyebrowse-switch-to-window-config-0))))

(use-package shackle
  :config (progn
            (setq shackle-lighter ""
                  shackle-rules '(("\\`\\*magit.*?\\'"      :regexp t :same t)
                                  ("\\`\\*helm.*?\\*\\'"    :regexp t :align t :ratio 0.4)
                                  (compilation-mode         :same   t)
                                  (sauron-mode              :ignore t)
                                  (erc-mode                 :same   t)
                                  (proced-mode              :same   t)
                                  (help-mode                :same   t)
                                  (ibuffer-mode             :same   t)
                                  (flycheck-error-list-mode :popup  t
                                                            :align 'below
                                                            :ratio 0.4
                                                            :select t)
                                  ("\\`\\*diff-hl\\*.*?\\'" :regexp t
                                                            :popup  t
                                                            :align 'below
                                                            :ratio 0.4)))
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
			                                                   company-backends)))) ))

;; PDF Tools
(use-package pdf-tools
             :defer f
             :config (pdf-loader-install))

(use-package vterm
             :defer t
             :config (with-eval-after-load 'vterm
                       (evil-set-initial-state 'vterm-mode 'emacs)))  

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

(progn
  (defun my-quit (count)
    "Quit if in a read-only buffer; otherwise, call self-insert-command."
    (interactive "p")
    (let ((m (buffer-local-value 'major-mode (current-buffer))))
      (cond
       ((eq 'magit-popup-mode m) (magit-popup-quit))
       ((not buffer-read-only)   (self-insert-command count))
       (t                        (kill-this-buffer)))))
  (global-set-key (kbd "q") 'my-quit)
  (require 'help-mode)
  (require 'proced)
  (require 'compile)
  (dolist (mode-map (list help-mode-map proced-mode-map compilation-mode-map))
    (define-key mode-map (kbd "q") 'my-quit)))
)

;; Allow font-lock-mode to do background parsing and restore some settings
(setq jit-lock-stealth-time 1
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.05
      debug-on-error nil
      debug-on-quit nil
      gc-cons-threshold (* 1 1024 1024)
      gc-cons-percentage 0.1)
(provide 'init)
;;; init.el ends here
