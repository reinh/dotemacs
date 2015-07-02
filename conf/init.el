
(setq user-full-name "Rein Henrichs")
(setq user-mail-address "rein.henrichs@gmail.com")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(eval-when-compile
  ;; (defvar use-package-verbose t)
  ;; (defvar use-package-expand-minimally t)
  (require 'cl)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)

(if (fboundp 'toggle-frame-maximized)
    (add-hook 'emacs-startup-hook 'toggle-frame-maximized))

(setq inhibit-startup-message t)

(when (not (display-graphic-p))
  (menu-bar-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t)

(use-package move-border
  :commands (move-border-left
             move-border-right
             move-border-up
             move-border-down)
  :init
  (progn
    (define-key evil-normal-state-map (kbd "<left>") 'move-border-left)
    (define-key evil-normal-state-map (kbd "<right>") 'move-border-right)
    (define-key evil-normal-state-map (kbd "<up>") 'move-border-up)
    (define-key evil-normal-state-map (kbd "<down>") 'move-border-down)))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(setq uniquify-buffer-name-style (quote post-forward-angle-brackets))

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(global-font-lock-mode 1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)

;;; Default mode settings
(setq major-mode 'text-mode)
(setq-default cursor-type 'bar)

;; Write backup files to own directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Tabs are the devil.
(set-default 'indent-tabs-mode nil)

;; Default indentation
(setq-default tab-width 2)

;; Come on... really?
(setq sentence-end-double-space nil)

(setq require-final-newline t)

;; Autofill
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package exec-path-from-shell
  :ensure t
  :if (eq window-system 'ns)
  :config
  (progn
    (mapc
     (lambda (variable)
       (add-to-list 'exec-path-from-shell-variables variable))
     '("ALTERNATE_EDITOR"
       "CDPATH"
       "EDITOR"
       "GOPATH"
       "GPG_AGENT_INFO"
       "HISTFILE"
       "INFOPATH"
       "LANG"
       "LC_ALL"
       "SSH_AUTH_SOCK"))
    (exec-path-from-shell-initialize)))

(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(use-package leuven-theme
  :ensure t
  :config
  (progn (load-theme 'leuven t)))

(use-package base16-theme :ensure t :defer 5)
(use-package helm-themes   :ensure t :defer 5)

(use-package pos-tip            :ensure t :defer 5)
(use-package edit-server        :ensure t :defer 5)
(use-package gmail-message-mode :ensure t :defer 5)
(use-package chess              :ensure t :defer 5)
(use-package narrow-indirect    :ensure t :defer 5)

(use-package css-mode :ensure t :mode ("\\.css\\'" . css-mode))
(use-package lua-mode :ensure t :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package discover
  :ensure t
  :config
  (global-discover-mode 1))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode)

(use-package ido
  :ensure t
  :demand t
  :bind (("C-x b" . ido-switch-buffer))
  :config
  (ido-mode t))

(use-package ido-hacks
  :ensure t
  :disabled t
  :config
  (ido-hacks-mode 1))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package on-screen
  :ensure t
  :defer 5
  :config
  (on-screen-global-mode 1))

(use-package smex
  :ensure t
  :demand
  :bind (("M-x" . smex)))

(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

(use-package twittering-mode
  :ensure t
  :commands twit
  :config
  (setq twittering-use-master-password t)
  (setq twittering-icon-mode t)
  (setq twittering-use-icon-storage t))

(use-package undo-tree
  :ensure t
  :commands undo-tree-mode
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package w3m
  :disabled t
  :commands (w3m-search w3m-find-file)
  :bind (("C-. u"   . w3m-browse-url)
         ("C-. U"   . w3m-browse-url-new-session))
  :init
  (setq w3m-command "w3m")

  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8))

(use-package winner
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode))
  :config
  (setq ace-jump-mode-submode-list
        '(ace-jump-char-mode
          ace-jump-word-mode
          ace-jump-line-mode)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ibuffer
  :ensure t
  :commands ibuffer
  :config
  (progn
    (setq ibuffer-saved-filter-groups
          '(("Config" (or
                       (filename . ".dots/")
                       (filename . ".emacs.d/")))
            ("Shell"  (or
                       (mode . eshell-mode)
                       (mode . shell-mode)))
            ("Dired"  (mode . dired-mode))
            ("Prose"  (or
                       (mode . tex-mode)
                       (mode . plain-tex-mode)
                       (mode . latex-mode)
                       (mode . rst-mode)
                       (mode . markdown-mode)))
            ("Haskell" (mode . haskell-mode))
            ("Org"    (mode . org-mode))
            ("Weechat" (name . ".*freenode.*"))
            ("Gnus"   (or
                       (mode . message-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)))
            ("Emacs"  (name . "^\\*.*\\*$")))
          ibuffer-show-empty-filter-groups nil
          ibuffer-expert t)

    (use-package ibuffer-vc
      :ensure t
      :commands ibuffer-vc-generate-filter-groups-by-vc-root
      :init
      (progn
        (defun nox/ibuffer-apply-filter-groups ()
          "Combine my saved ibuffer filter groups with those generated
     by `ibuffer-vc-generate-filter-groups-by-vc-root'"
          (interactive)
          (setq ibuffer-filter-groups
                (append (ibuffer-vc-generate-filter-groups-by-vc-root)
                        ibuffer-saved-filter-groups))
          (message "ibuffer-vc: groups set")
          (let ((ibuf (get-buffer "*Ibuffer*")))
            (when ibuf
              (with-current-buffer ibuf
                (pop-to-buffer ibuf)
                (ibuffer-update nil t)))))

        (add-hook 'ibuffer-hook 'nox/ibuffer-apply-filter-groups)))))

(use-package ag
  :ensure t
  :commands (ag ag-regexp)
  :init
  (use-package helm-ag
    :ensure t
    :commands helm-ag))

(use-package company
  :ensure t
  :diminish company-mode
  :commands company-mode
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  (use-package company-go
    :ensure t)

  (use-package helm-company
    :ensure t
    :load-path "site-lisp/helm-company"
    :disabled t))

(use-package god-mode
  :ensure t
  :commands god-mode
  :bind ("C-z" . god-local-mode))

(use-package evil
  :ensure t
  :commands evil-mode
  :config
  (progn
    ;; Override the starting state in a few major modes
    (evil-set-initial-state 'magit-mode 'emacs)
    (evil-set-initial-state 'org-agenda-mode 'emacs)
    (evil-set-initial-state 'package-menu-mode 'motion)
    (evil-set-initial-state 'paradox-menu-mode 'motion)

    ;; Reclaim useful keys from evil-motion-state-map
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)

    (global-set-key (kbd "RET") 'newline-and-indent)
    (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

    (define-key evil-normal-state-map "Y" (kbd "y$"))))

(use-package evil-surround
  :ensure t
  :defer 2
  :commands global-evil-surround-mode
  :config (global-evil-surround-mode 1))

(use-package gnus
  :ensure t
  :defer t
  :commands gnus
  :bind (("M-G"   . switch-to-gnus)
         ("C-x m" . compose-mail)))

(use-package lisp-mode
  :defer t
  :init
  (progn
    (use-package eldoc
      :commands turn-on-eldoc-mode
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)))
  :config
  (defun my-lisp-mode-hook ()
    (paredit-mode 1)
    (speed-of-thought-mode 1)
    (local-set-key (kbd "<return>") 'paredit-newline)
    (add-hook 'after-save-hook 'check-parens nil t))
  (add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "elisp"))))

(use-package sotlisp
  :ensure t
  :defer t
  :diminish sotlisp-mode)

(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :bind (("C-x g" . magit-status))
  :init
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setenv "GIT_PAGER" "")
  ;; full screen magit-status
  ;; From http://whattheemacsd.com/setup-magit.el-01.html
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

(use-package yasnippet
  :ensure t
  :defer 2
  :diminish yas-minor-mode
  :config
  (progn
    ;; Suppress excessive log messages
    (setq yas-verbosity 1
          yas-prompt-functions '(yas-ido-prompt)
          yas-snippet-dir (expand-file-name "snippets" user-emacs-directory))
    (yas-global-mode t)))

(use-package flycheck
  :ensure t
  :defer 2
  :diminish flycheck-mode " ✓"
  :commands global-flycheck-mode
  :config
  (progn
    (global-flycheck-mode 1)
    (setq-default flycheck-disabled-checkers
                  '(html-tidy
                    emacs-lisp-checkdoc))))

(use-package flyspell
  :defer 2
  :init
  :config
  (progn
    (setq ispell-program-name "aspell")
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook (lambda () (yas-minor-mode)))
  (add-hook 'before-save-hook #'gofmt-before-save)
  :config
  (use-package go-snippets
    :ensure t)
  (setenv "GOPATH" "/Users/reinh/go")
  (setenv "PATH" (concat (getenv "PATH") ":/Users/reinh/go/bin"))
  (setq exec-path (append exec-path '("/Users/reinh/go/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/opt/go/libexec/bin"))
  (setq exec-path (append exec-path '("/usr/local/opt/go/libexec/bin")))
  (setq gofmt-command "goimports")
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  (load-file "$GOPATH/src/golang.org/x/tools/refactor/rename/rename.el"))

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'"    . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.hcr\\'"   . haskell-core-mode))
  :interpreter ("haskell" . haskell-mode)

  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook (lambda () (yas-minor-mode)))

  :config
  (require 'haskell)
  (require 'haskell-mode)
  (require 'haskell-interactive-mode)
  (require 'autoinsert)

  (define-skeleton haskell-skeleton
    "Default Haskell file initial contents."
    nil
    "-- | " _ "\n\n"
    "module "
    (haskell-guess-module-name)
    " where\n\n")

  (define-auto-insert "\\.hs" 'haskell-skeleton)

  (defun haskell-auto-insert-module-template ()
  "Insert a module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert
     "-- | "
     "\n"
     "\n"
     "module "
     )
    (let ((name (haskell-guess-module-name)))
      (if (string= name "")
          (progn (insert "Main")
                 (shm-evaporate (- (point) 5)
                                (point)))
        (insert name)))
    (insert " where"
            "\n"
            "\n")
    (goto-char (point-min))
    (forward-char 4)))

  (defun haskell-insert-doc ()
    "Insert the documentation syntax."
    (interactive)
    (insert "-- | "))

  (defun haskell-insert-undefined ()
    "Insert undefined."
    (interactive)
    (if (and (boundp 'structured-haskell-mode)
             structured-haskell-mode)
        (shm-insert-string "undefined")
      (insert "undefined")))

  (defun haskell-move-right ()
    (interactive)
    (haskell-move-nested 1))

  (defun haskell-move-left ()
    (interactive)
    (haskell-move-nested -1))

  (defun haskell-who-calls (&optional prompt)
    "Grep the codebase to see who uses the symbol at point."
    (interactive "P")
    (let ((sym (if prompt
                   (read-from-minibuffer "Look for: ")
                 (haskell-ident-at-point))))
      (let ((existing (get-buffer "*who-calls*")))
        (when existing
          (kill-buffer existing)))
      (let ((buffer
             (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                                (haskell-session-current-dir (haskell-session))
                                sym))))
        (with-current-buffer buffer
          (rename-buffer "*who-calls*")
          (switch-to-buffer-other-window buffer)))))

  (define-key interactive-haskell-mode-map (kbd "M-,") 'haskell-who-calls)
  (define-key interactive-haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key interactive-haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
  (define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
  (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

  (define-key haskell-mode-map (kbd "C-c C-m") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)
  (define-key haskell-mode-map (kbd "C-c C-a") 'haskell-insert-doc)
  (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
  (define-key haskell-mode-map (kbd "C-<right>") 'haskell-move-right)
  (define-key haskell-mode-map (kbd "C-<left>") 'haskell-move-left)
  (define-key haskell-mode-map (kbd "<space>") 'haskell-mode-contextual-space)

  (use-package shm
    :ensure t
    :config
    (require 'shm-reformat)
    (require 'shm-case-split)

    (defun shm-contextual-space ()
      "Do contextual space first, and run shm/space if no change in
the cursor position happened."
      (interactive)
      (if (looking-back "import")
          (call-interactively 'haskell-mode-contextual-space)
        (progn
          (let ((ident (haskell-ident-at-point)))
            (when ident
              (and interactive-haskell-mode
                   (haskell-process-do-try-type ident))))
          (call-interactively 'shm/space))))

    (define-key shm-map (kbd "C-c C-p") 'shm/expand-pattern)
    (define-key shm-map (kbd "C-c C-s") 'shm/case-split)
    (define-key shm-map (kbd "SPC") 'shm-contextual-space)
    (define-key shm-map (kbd "C-\\") 'shm/goto-last-point)
    (define-key shm-map (kbd "C-c C-f") 'shm-fold-toggle-decl)
    (define-key shm-map (kbd "C-c i") 'shm-reformat-decl)
    (custom-set-faces
     '(shm-quarantine-face ((t (:underline (:color "#FF0000" :style wave)))))
     '(shm-current-face ((t (:inherit highlight)))))
    (use-package hindent :ensure t)))

(use-package rust-mode
  :ensure t
  :defer t
  :init (require 'rust-mode))

;; Basic settings
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t
          LaTeX-electric-left-right-brace t
          TeX-electric-sub-and-superscript t
          TeX-insert-braces nil)
    (setq-default TeX-master nil))
  :config
  (add-hook 'plain-TeX-mode-hook
            (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                            (cons "$" "$"))))
  (add-hook 'LaTeX-mode-hook
            (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                            (cons "\\(" "\\)")))))

(use-package org
  :ensure t

  :config
  (setq org-src-fontify-natively t))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t))

(use-package sauron :ensure t :defer 5)

(use-package weechat
  :ensure t
  :defer 5
  :init
  (setq notify-method nil)
  :config
  (progn
    (setq weechat-color-list
          '(unspecified "black" "dim gray" "dark red" "red"
                        "dark green" "green" "brown"
                        "orange" "dark blue" "blue"
                        "dark magenta" "magenta" "dark cyan"
                        "royal blue" "dark gray" "gray"))
    (require 'weechat-sauron)
    (require 'weechat-tracking)))

(use-package math-symbol-lists
  :ensure t
  :defer 5
  :config
  (progn
    (quail-define-package "math" "UTF-8" "Ω" t)
    (quail-define-rules ; add whatever extra rules you want to define here...
     ("\\from"    #X2190)
     ("\\to"      #X2192)
     ("\\lhd"     #X22B2)
     ("\\rhd"     #X22B3)
     ("\\unlhd"   #X22B4)
     ("\\unrhd"   #X22B5))
    (mapc (lambda (x)
            (if (cddr x)
                (quail-defrule (cadr x) (car (cddr x)))))
          (append math-symbol-list-basic math-symbol-list-extended))))

(use-package popwin
  :ensure t
  :commands popwin-mode
  :defer 2
  :config
  (progn
    (popwin-mode 1)
    (push '("*Org Agenda*" :width 82 :position right :dedicated t :stick t) popwin:special-display-config)
    (push '("*helm*" :height 20) popwin:special-display-config)
    (push '("^\*helm .+\*$" :regexp t :height 20) popwin:special-display-config)
    (push '("*Compile-Log*" :height 20 :noselect t) popwin:special-display-config)
    (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)))

(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (let ((range (list (line-beginning-position)
                     (goto-char (line-end-position n)))))
    (comment-or-uncomment-region
     (apply #'min range)
     (apply #'max range)))
  (forward-line 1)
  (back-to-indentation))

(global-set-key (kbd "C-;") #'endless/comment-line)

(require 're-builder)
(setq reb-re-syntax 'string)
(put 'downcase-region 'disabled nil)
