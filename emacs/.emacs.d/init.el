(setq inhibit-startup-message t) ;; Disables the startup message

(scroll-bar-mode -1)   ; Disables visible scroll bar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Give some breathing room
(menu-bar-mode -1)     ; Disable the menu bar
(blink-cursor-mode -1) ; Makes cursor not blink


(column-number-mode 1) ;; Adds column numbering to the modeline
;; (setq warning-minimum-level :emergency) ;; hides comp errors

(set-face-attribute 'default nil :font "Source Code Pro" :height 110)
(set-face-attribute 'cursor nil :background "DarkGoldenrod2")

(setq default-directory "/home/james/Documents/")
(setq dired-guess-shell-alist-user '(("\\.pdf\\'" "zathura")))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves" user-emacs-directory) t)))

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(setq straight-fix-flycheck t)
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
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(use-package doom-themes)
(use-package nord-theme)

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package ivy
    :diminish ;; Hides minor mode from mode-line minor mode list
    :bind (("C-s" . swiper)
	   :map ivy-minibuffer-map
	   ("TAB" . ivy-alt-done)
	   ("C-l" . ivy-alt-done)
	   ("C-j" . ivy-next-line)
	   ("C-k" . ivy-previous-line)
	   :map ivy-switch-buffer-map
	   ("C-k" . ivy-previous-line)
	   ("C-l" . ivy-done)
	   ("C-d" . ivy-switch-buffer-kill)
	   :map ivy-reverse-i-search-map
	   ("C-k" . ivy-previous-line)
	   ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))


  (use-package counsel
    :bind (("M-x" . counsel-M-x)
	   ("C-x b" . counsel-ibuffer)
	   ("C-x C-f" . counsel-find-file)
	   :map minibuffer-local-map
	   ("C-f" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.01
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-min-display-lines 6
        which-key-max-description-length 32
        which-key-add-column-padding 1
        which-key-allow-multiple-replacements t)
  )

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
	 (agda2-mode . rainbow-delimiters-mode)
	 )
  )

(use-package evil
  :init
  ;; :hook (evil-mode . jl/evil-hook)
  (setq evil-want-keybinding nil)
  (setq evil-normal-state-cursor '("#DAA520" box))
  (setq evil-insert-state-cursor '("#50FA7B" bar))
  :config
  (evil-mode 1)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)


  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :diminish t
  :after evil
  :config
  (evil-commentary-mode 1)
  )

(defun spacemacs/sudo-edit (&optional arg)
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name
                          ;; Try to retrieve a tramp method suitable for
                          ;; multi-hopping
                          (cond ((tramp-get-method-parameter
                                  parsed 'tramp-login-program))
                                ((tramp-get-method-parameter
                                  parsed 'tramp-copy-program))
                                (t parsed-method))
                          parsed-user
                          parsed-domain
                          parsed-host
                          parsed-port
                          nil
                          parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name
                            "sudo"
                            parsed-user
                            parsed-domain
                            parsed-host
                            parsed-port
                            parsed-localname
                            new-hop)))
           new-fname))))))

(use-package general
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-create-definer jl/SPC-keys
    ;; :keymaps '(normal visual motion)
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    )


  (general-create-definer jl/major-modes
                          ;; :keymaps '(normal visual motion)
                          :states '(normal visual motion)
                          :prefix "SPC m"
                          :global-prefix ","
                          )

  (general-create-definer jl/C-c-keys
                          ;; :keymaps '(normal visual insert emacs operator motion)
                          :states '(normal visual insert emacs operator motion)
                          :prefix "C-c"
                          )
  )

(general-auto-unbind-keys)
(jl/SPC-keys)

(defun spacemacs/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing "~/.emacs.d/ReadMe.org"))

  (jl/SPC-keys
    "a" '(:ignore t :which-key "applications")
    "ae" '(:ignore t :which-key "email")
    "ar" '(:ignore t :which-key "readers")

    "frg" 'elpher
    "at" '(:ignore t :which-key "tools")

    "b" '(:ignore t :which-key "buffers")
    "c" '(:ignore t :which-key "compile/Comments")
    "e" '(:ignore t :which-key "errors")
    "f" '(:which-key "files")
    "fe" '(:which-key "Emacs")


    "g" '(:ignore t :which-key "git")
    "h" '(:ignore t :which-key "help")
    "i" '(:ignore t :which-key "insertion")
    "j" '(:ignore t :which-key "jump/join/split")
    "k" '(:ignore t :which-key "lisp")
    "n" '(:ignore t :which-key "narrow/numbers")
    "p" '(:ignore t :which-key "projects")
    "q" '(:ignore t :which-key "quit")
    "r" '(:ignore t :which-key "registers/rings/resume")
    "s" '(:ignore t :which-key "search/symbol")
    "q" '(:ignore t :which-key "quit")
    "t" '(:ignore t :which-key "toggles")
    "w" '(:ignore t :which-key "windows")
    "z" '(:ignore t :which-key "zoom")
    "C" '(:ignore t :which-key "Capture/Colours")
    "D" '(:ignore t :which-key "Diff/compare")
    "F" '(:ignore t :which-key "Frames")
    "K" '(:ignore t :which-key "Keyboard Macros")
    "N" '(:ignore t :which-key "Navigation")
    "S" '(:ignore t :which-key "Spelling")
    "T" '(:ignore t :which-key "UI toggles/Themes")
    "C-v" '(:ignore t :which-key "Rectangles")
    "m" '(:ignore t :which-key "major mode")
    )

(jl/SPC-keys
  "SPC" '(counsel-M-x :which-key "M-x")

  "fd" 'delete-file
  "ff" 'find-file
  "fed" '(spacemacs/find-dotfile :which-key "Open init file")
  "fE" '(spacemacs/sudo-edit :which-key "Edit with sudo...")
  "fR" 'rename-file
  "fs" '(save-buffer :which-key "save")

  "qq" 'kill-emacs


  "ts" '(hydra-text-scale/body :which-key "scale text")
  "tt" '(counsel-load-theme :which-key "choose theme")

  "w-" 'split-window-below
  "w/" 'split-window-right
  "wd" 'delete-window

  "/" 'counsel-git-grep
  )

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(general-define-key
 :states 'normal
 "p" 'counsel-yank-pop
 "C-r" 'undo-tree-redo
 "u" 'undo-tree-undo
 )

(use-package hydra)
(defhydra hydra-text-scale () ;;(:timeout 4) ;; -- I don't want a timeout
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra jl/pasting-hydra ()
  "Pasting Transient State"
  ("C-j" evil-paste-pop)
  ("C-k" evil-paste-pop-next)
  ("p" evil-paste-after)
  ("P" evil-paste-before)
  )

(general-define-key
 :states 'normal
 "p"  'jl/pasting-hydra/evil-paste-after
  )

(defun jl/persp-keys ()
  (jl/SPC-keys
    "b'" 'persp-switch-by-number
    "ba" 'persp-add-buffer
    "bA" 'persp-set-buffer
    "bb" '(persp-ivy-switch-buffer :which-key "show local buffers")
    "bB" '(counsel-ibuffer :which-key "show all buffers")
    "bD" 'persp-remove-buffer
    "bd" 'kill-this-buffer
    "bi" 'persp-import
    "bk" '(persp-kill :which-key "kill perspective")
    "bn" 'next-buffer
    "bN" 'persp-next
    "bp" 'previous-buffer
    "bP" 'persp-prev
    "bs" '(persp-switch :which-key "switch perspective")
    "bS" 'persp-state-save
    "bL" 'persp-state-load

    "bh" 'buffer-visit-dashboard
    "bR" 'revert-buffer

    )
  )

(use-package perspective
  :bind (("C-x k" . persp-kill-buffer*))
  :init
  (jl/persp-keys)
  (persp-mode)
  )

(defun buffer-visit-dashboard ()
  (interactive)
  (switch-to-buffer "*dashboard*")
  (dashboard-refresh-buffer)
  )

(use-package page-break-lines
  :after dashboard
  )

(use-package dashboard
  :init
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

  :config
  (general-evil-define-key '(normal motion) 'dashboard-mode-map
    "j"  'widget-forward
    "k"  'widget-backward
    )

  (dashboard-setup-startup-hook)
  )

(use-package winum
  :init (winum-mode)
  :diminish winum-mode
  :config

(defhydra window-transient-state ()
  "Window Transient State"
  ;; Select
  ("j" evil-window-down)
  ("<down>" evil-window-down)
  ("k" evil-window-up)
  ("<up>" evil-window-up)
  ("h" evil-window-left)
  ("<left>" evil-window-left)
  ("l" evil-window-right)
  ("<right>" evil-window-right)
  ("0" winum-select-window-0)
  ("1" winum-select-window-1)
  ("2" winum-select-window-2)
  ("3" winum-select-window-3)
  ("4" winum-select-window-4)
  ("5" winum-select-window-5)
  ("6" winum-select-window-6)
  ("7" winum-select-window-7)
  ("8" winum-select-window-8)
  ("9" winum-select-window-9)
  ("a" ace-window)
  ("o" other-frame)
  ("w" other-window)
  ;; Move
  ("J" evil-window-move-very-bottom)
  ("<S-down>" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("<S-up>" evil-window-move-very-top)
  ("H" evil-window-move-far-left)
  ("<S-left>" evil-window-move-far-left)
  ("L" evil-window-move-far-right)
  ("<S-right>" evil-window-move-far-right)
  ("r" rotate-windows-forward)
  ("R" rotate-windows-backward)
  ;; Split
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("-" split-window-below-and-focus)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("/" split-window-right-and-focus)
  ("m" toggle-maximize-buffer)
  ("|" maximize-vertically)
  ("_" maximize-horizontally)
  ;; Resize
  ("[" shrink-window-horizontally)
  ("]" enlarge-window-horizontally)
  ("{" shrink-window)
  ("}" enlarge-window)
  ;; Other
  ("d" delete-window)
  ("D" delete-other-windows)
  ("u" winner-undo)
  ("U" winner-redo)
  ("q" nil :exit t)
  )

 (jl/SPC-keys
  "0" '(winum-select-window-0 :which-key "Select window 0")
  "1" '(winum-select-window-1 :which-key "Select window 1")
  "2" '(winum-select-window-2 :which-key "Select window 2")
  "3" '(winum-select-window-3 :which-key "Select window 3")
  "4" '(winum-select-window-4 :which-key "Select window 4")
  "5" '(winum-select-window-5 :which-key "Select window 5")
  "6" '(winum-select-window-6 :which-key "Select window 6")
  "7" '(winum-select-window-7 :which-key "Select window 7")
  "8" '(winum-select-window-8 :which-key "Select window 8")
  "9" '(winum-select-window-9 :which-key "Select window 9")
  "w." 'window-transient-state/body
  )
)

(defhydra jl/agda-goal-navigation ()
  "Goal Navigation Transient State"
  ("f" agda2-next-goal "next")
  ("b" agda2-previous-goal "previous")
  ("q" nil "quit":exit t))

(defun agda2-next-goal-transient ()
  (interactive)
  (jl/agda-goal-navigation/body)
  (agda2-next-goal)
  )

(defun agda2-previous-goal-transient ()
  (interactive)
  (jl/agda-goal-navigation/body)
  (agda2-previous-goal)
  )

(defun jl/agda-keys ()
    (jl/major-modes
      :states 'normal
      :keymaps 'agda2-mode-map
      "g"   '"Go To"
      "?"   'agda2-show-goals
      "."   'agda2-goal-and-context-and-inferred
      ","   'agda2-goal-and-context
      ";"   'agda2-goal-and-context-and-checked
      "="   'agda2-show-constraints
      "SPC" 'agda2-give
      "RET" 'agda2-elaborate-give
      "a"   'agda2-auto-maybe-all
      "b"   'agda2-previous-goal-transient
      "c"   'agda2-make-case
      "d"   'agda2-infer-type-maybe-toplevel
      "e"   'agda2-show-context
      "f"   'agda2-next-goal-transient
      "gG"  'agda2-go-back
      "h"   'agda2-helper-function-type
      "l"   'agda2-load
      "n"   'agda2-compute-normalised-maybe-toplevel
      "o"   'agda2-module-contents-maybe-toplevel
      "r"   'agda2-refine
      "s"   'agda2-solve-maybe-all
      "t"   'agda2-goal-type
      "un"  'agda2-compute-normalised
      "uN"  'agda2-compute-normalised-toplevel
      "w"   'agda2-why-in-scope-maybe-toplevel
      "xa"  'agda2-abort
      "xc"  'agda2-compile
      "xd"  'agda2-remove-annotations
      "xh"  'agda2-display-implicit-arguments
      "xl"  'agda2-load
      "xq"  'agda2-quit
      "xr"  'agda2-restart
      "xs"  'agda2-set-program-version
      "x;"  'agda2-comment-dwim-rest-of-buffer
      "z"   'agda2-search-about-toplevel
      )
    )

(use-package agda2-mode
  ;; :load-path "/home/james/.cabal/share/x86_64-linux-ghc-8.10.5/Agda-2.6.3/emacs-mode/agda2.el"
  :config
  (jl/agda-keys)
  )

(use-package elfeed
  :config
  (setq rmh-elfeed-org-files (list "~/Documents/Org/Elfeed/feeds.org"))

(jl/major-modes
 :keymaps 'elfeed-search-mode-map
 "c"  'elfeed-db-compact
 "gr" 'elfeed-update
 "gR" 'elfeed-search-update--force
 "gu" 'elfeed-unjam
 "o"  'elfeed-load-opml
 "q"  'quit-window
 "w"  'elfeed-web-start
 "W"  'elfeed-web-stop
 "+"  'elfeed-search-tag-all
 "-"  'elfeed-search-untag-all
 "b"  'elfeed-search-browse-url
 "y"  'elfeed-search-yank)

(jl/major-modes
 :states 'normal
 :keymaps 'elfeed-show-mode-map
 "n" 'elfeed-show-next
 "p" 'elfeed-show-prev)

(jl/SPC-keys
 "are" 'elfeed)

)

(defun jl/elpher-key-bindings ()
  (jl/major-modes
   :keymaps 'elpher-mode-map
   "TAB"       'elpher-next-link
   "<backtab>" 'elpher-prev-link
   "u" 'elpher-back
   "U" 'elpher-back-to-start
   "O" 'elpher-root-dir
   "g" 'elpher-go
   "o" 'elpher-go-current
   "t" '(org-latex-preview :which-key "view latex")
   "r" 'elpher-redraw
   "R" 'elpher-reload
   "T" 'elpher-toggle-tls
   "." 'elpher-view-raw
   "d" 'elpher-download
   "D" 'elpher-download-current
   "m" 'elpher-jump
   "i" 'elpher-info-link
   "I" 'elpher-info-current
   "c" 'elpher-copy-link-url
   "C" 'elpher-copy-current-url
   "a" 'elpher-bookmark-link
   "A" 'elpher-bookmark-current
   "x" 'elpher-unbookmark-link
   "X" 'elpher-unbookmark-current
   "B" 'elpher-bookmarks
   "S" 'elpher-set-gopher-coding-system
   "F" 'elpher-forget-current-certificate)
  )

(defun jl/elpher-global-keys ()
  (jl/SPC-keys
   "arg" 'elpher
   )
  )

(use-package elpher
  :init
  (jl/elpher-global-keys)
  :config
  (jl/elpher-key-bindings)
  (set-face-attribute 'elpher-gemini-heading1 nil :inherit 'org-level-1)
  (set-face-attribute 'elpher-gemini-heading2 nil :inherit 'org-level-2)
  (set-face-attribute 'elpher-gemini-heading3 nil :inherit 'org-level-2)

  (setq elpher-bookmarks-file "~/.spacemacs.d/elpher-bookmarks"
	elpher-start-page "gemini://gemini.circumlunar.space")
  (add-hook 'elpher-mode-hook 'variable-pitch-mode)
  )

(defun jl/erc-keys-global ()
  (jl/SPC-keys
    "ari" 'erc-tls
    )
  )

(defun jl/erc-keys ()
  (general-define-key
   :states '(normal insert visual)
   :keymaps 'erc-mode-map
   "C-j" 'erc-next-command
   "C-k" 'erc-previous-command
   "C-l" 'erc-clear-input-ring
   )
  )

(use-package erc-hl-nicks
  :after erc)

(use-package erc-image
  :after erc)

(use-package erc
  :init
  (jl/erc-keys-global)
  :config
  (add-hook 'erc-mode-hook 'erc-image-mode)
  (jl/erc-keys)
  (setq erc-server "irc.libera.chat"
	erc-nick "jeslie0"
	erc-port "6697"
	erc-password (shell-command-to-string "gpg2 -q --for-your-eyes-only --no-tty -d ~/.password-store/irc.libra.chat/jeslie0.gpg")
	erc-prompt-for-password nil
	erc-user-full-name "James Leslie"
	erc-track-shorten-start 8
	erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
	erc-kill-buffer-on-part t
	erc-auto-query 'bury
	erc-image-inline-rescale 400
	)
  )

(defun jl/eww-global-keys ()
  (jl/SPC-keys
    "ate" 'eww
    )
  )

(defun jl/eww-keys ()
  (jl/major-modes
    :keymaps 'eww-mode-map
    "s" 'helm-google-suggest
    "S" 'browse-web
    "t" '(org-latex-preview :which-key "view latex")
    "r" 'eww-reload
    "p" 'eww-previous-url
    "n" 'eww-next-url
    "h" 'eww-list-histories
    "d" 'eww-download
    "a" 'eww-add-bookmark
    "lb" 'eww-list-buffers
    "lo" 'eww-list-bookmarks
    "vx" 'eww-browse-with-external-browser
    "vf" 'eww-toggle-fonts
    "vr" 'eww-readable
    "vs" 'eww-view-source
    )

  (jl/major-modes
    :keymaps 'eww-mode-map
    "v" '(:ignore t :which-key "view")
    "l" '(:ignore t :which-key "list")
    )
  )

(use-package eww
  :init
  (jl/eww-global-keys)
  :config
  (jl/eww-keys)
  (evil-collection-define-key 'normal 'eww-mode-map
    (kbd "SPC") nil)
  (jl/SPC-keys)
  (add-hook 'eww-mode-hook 'variable-pitch-mode)

  )

(use-package haskell-mode
  :hook (haskell-mode . lsp-mode)
  :config
  (jl/major-modes
   :states 'normal
   :keymaps 'haskell-mode-map
   :major-modes t
   "g" '(:ignore t :which-key "navigation")
   "s" '(:ignore t :which-key "repl")
   "c" '(:ignore t :which-key "cabal")
   "h" '(:ignore t :which-key "documentation")
   "d" '(:ignore t :which-key "debug")
   "r" '(:ignore t :which-key "refactor")
   )


  (jl/major-modes
   :states 'normal
   :keymaps 'haskell-mode-map
   :major-modes t
   "'" 'haskell-interactive-switch
   "S" 'haskell-mode-stylish-buffer

   "sb"  'haskell-process-load-file
   "sc"  'haskell-interactive-mode-clear
   "sS"  'spacemacs/haskell-interactive-bring
   "ss"  'haskell-interactive-switch
   "st"  'haskell-session-change-target
   "'"   'haskell-interactive-switch

   "ca"  'haskell-process-cabal
   "cb"  'haskell-process-cabal-build
   "cc"  'haskell-compile
   "cv"  'haskell-cabal-visit-file

   "hd"  'inferior-haskell-find-haddock
   "hi"  'haskell-process-do-info
   "ht"  'haskell-process-do-type
   "hT"  'spacemacs/haskell-process-do-type-on-prev-line

   "da"  'haskell-debug/abandon
   "db"  'haskell-debug/break-on-function
   "dB"  'haskell-debug/delete
   "dc"  'haskell-debug/continue
   "dd"  'haskell-debug
   "dn"  'haskell-debug/next
   "dN"  'haskell-debug/previous
   "dp"  'haskell-debug/previous
   "dr"  'haskell-debug/refresh
   "ds"  'haskell-debug/step
   "dt"  'haskell-debug/trace

   "ri"  'spacemacs/haskell-format-imports
   )

  (general-define-key
   :states '(normal insert visual)
   :keymaps 'haskell-interactive-mode-map
   "C-j" 'haskell-interactive-mode-history-next
   "C-k" 'haskell-interactive-mode-history-previous
   "C-l" 'haskell-interactive-mode-clear
   )
  )

(defun jl/lsp-keys-descr ()
(interactive)
  (jl/major-modes
    :keymaps 'haskell-mode-map
    :major-modes t
    :states '(normal visual motion)
    "=" '(:which-key "format")
    "a" '(:ignore t :which-key "code action")
    "g" '(:ignore t :which-key "goto")
    "h" '(:ignore t :which-key "help")
    "b" '(:ignore t :which-key "backend")
    "r" '(:ignore t :which-key "refactor")
    "l" '(:ignore t :which-key "lsp")
    "T" '(:ignore t :which-key "toggle")
    "F" '(:ignore t :which-key "folder")
    "x" '(:ignore t :which-key "text/code")
    )
)

(defun jl/lsp-keys ()
  (jl/major-modes
    :keymaps 'haskell-mode-map
    :major-modes t
    :states '(normal visual motion)
    ;; format
    "=b" 'lsp-format-buffer
    "=r" 'lsp-format-region
    "=o" 'lsp-organize-imports
    ;; code actions
    "aa" 'lsp-execute-code-action
    "al" 'lsp-avy-lens
    ;; goto
    ;; N.B. implementation and references covered by xref bindings / lsp provider...
    "gt" #'lsp-find-type-definition
    "gk" #'spacemacs/lsp-avy-goto-word
    "gK" #'spacemacs/lsp-avy-goto-symbol
    "gM" 'lsp-ui-imenu
    ;; help
    "hh" #'lsp-describe-thing-at-point
    ;; jump
    ;; backend
    "bd" #'lsp-describe-session
    "br" #'lsp-workspace-restart
    "bs" #'lsp-workspace-shutdown
    "bv" #'lsp-version
    ;; refactor
    "rr" #'lsp-rename
    ;; toggles
    "Tld" #'lsp-ui-doc-mode
    "Tls" #'lsp-ui-sideline-mode
    "TlF" #'spacemacs/lsp-ui-doc-func
    "TlS" #'spacemacs/lsp-ui-sideline-symb
    "TlI" #'spacemacs/lsp-ui-sideline-ignore-duplicate
    "Tll" #'lsp-lens-mode
    ;; folders
    "Fs" #'lsp-workspace-folders-switch
    "Fr" #'lsp-workspace-folders-remove
    "Fa" #'lsp-workspace-folders-add
    ;; text/code
    "xh" #'lsp-document-highlight
    "xl" #'lsp-lens-show
    "xL" #'lsp-lens-hide
    )
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :hook (haskell-mode . lsp)
  :init
  (jl/lsp-keys)
(jl/lsp-keys-descr)
  :config
  )

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-haskell
  :after haskell-mode
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper")
  (setq lsp-haskell-server-args ())
  ;; Comment/uncomment this line to see interactions between lsp client/server.
  (setq lsp-log-io t)
)

(defun jl/auctex-keys ()
  (jl/major-modes
    :keymaps 'LaTeX-mode-map
    :states 'normal
    "\\"  'TeX-insert-macro                            ;; C-c C-m
    "-"   'TeX-recenter-output-buffer                  ;; C-c C-l
    "%"   'TeX-comment-or-uncomment-paragraph          ;; C-c %
    ";"   'comment-or-uncomment-region                 ;; C-c ; or C-c :
    ;; TeX-command-run-all runs compile and open the viewer
    "k"   'TeX-kill-job                                ;; C-c C-k
    "l"   'TeX-recenter-output-buffer                  ;; C-c C-l
    "m"   'TeX-insert-macro                            ;; C-c C-m
    "n"   'TeX-next-error                              ;; C-c `
    "N"   'TeX-previous-error                          ;; M-g p
    "v"   'TeX-view                                    ;; C-c C-v
    ;; TeX-doc is a very slow function
    "hd"  'TeX-doc
    "xb"  'latex/font-bold
    "xc"  'latex/font-code
    "xe"  'latex/font-emphasis
    "xi"  'latex/font-italic
    "xr"  'latex/font-clear
    "xo"  'latex/font-oblique
    "xfc" 'latex/font-small-caps
    "xff" 'latex/font-sans-serif
    "xfr" 'latex/font-serif

    "a"   'TeX-command-run-all                         ;; C-c C-a
    "b"   'latex/build

    "z=" 'TeX-fold-math
    "zb" 'TeX-fold-buffer
    "zB" 'TeX-fold-clearout-buffer
    "ze" 'TeX-fold-env
    "zI" 'TeX-fold-clearout-item
    "zm" 'TeX-fold-macro
    "zp" 'TeX-fold-paragraph
    "zP" 'TeX-fold-clearout-paragraph
    "zr" 'TeX-fold-region
    "zR" 'TeX-fold-clearout-region
    "zz" 'TeX-fold-dwim

    "*"   'LaTeX-mark-section      ;; C-c *
    "."   'LaTeX-mark-environment  ;; C-c .
    "ii"   'LaTeX-insert-item       ;; C-c C-j
    "s"   'LaTeX-section           ;; C-c C-s
    "fe"  'LaTeX-fill-environment  ;; C-c C-q C-e
    "fp"  'LaTeX-fill-paragraph    ;; C-c C-q C-p
    "fr"  'LaTeX-fill-region       ;; C-c C-q C-r
    "fs"  'LaTeX-fill-section      ;; C-c C-q C-s
    "pb"  'preview-buffer
    "pc"  'preview-clearout
    "pd"  'preview-document
    "pe"  'preview-environment
    "pf"  'preview-cache-preamble
    "pp"  'preview-at-point
    "pr"  'preview-region
    "ps"  'preview-section
    "xB"  'latex/font-medium
    "xr"  'latex/font-clear
    "xfa" 'latex/font-calligraphic
    "xfn" 'latex/font-normal
    "xfu" 'latex/font-upright

    "a"   'TeX-command-run-all
    "iC"   'org-ref-insert-cite-key
    "ic"   'LaTeX-close-environment
    "ie"   'LaTeX-environment

    "rc" 'reftex-citation
    "rg" 'reftex-grep-document
    "ri" 'reftex-index-selection-or-word
    "rI" 'reftex-display-index
    "rl" 'reftex-label
    "rp" 'reftex-index-phrase-selection-or-word
    "rr" 'reftex-reference
    "rs" 'reftex-search-document
    "rt" 'reftex-toc
    "rT" 'reftex-toc-recenter
    "rv" 'reftex-view-crossref
    )

  (jl/major-modes
    :keymaps 'LaTeX-mode-map
    :states 'normal
    "xf" '(:ignore t :which-key "fonts")
    "f" '(:ignore t :which-key "fill")
    "h" '(:ignore t :which-key "help")
    "x" '(:ignore t :which-key "text/fonts")
    "z" '(:ignore t :which-key "fold")
    "i" '(:ignore t :which-key "insert")
    "p" '(:ignore t :which-key "preview")
    "r" '(:ignore t :which-key "reftex")
    )
  )

(defun latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))
;; (setq build-proc (TeX-command latex-build-command 'TeX-master-

(defun latex/auto-fill-mode ()
  "Toggle auto-fill-mode using the custom auto-fill function."
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'latex//autofill))

;; Rebindings for TeX-font
(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-medium () (interactive) (TeX-font nil ?\C-m))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-calligraphic () (interactive) (TeX-font nil ?\C-a))
(defun latex/font-small-caps () (interactive) (TeX-font nil ?\C-c))
(defun latex/font-sans-serif () (interactive) (TeX-font nil ?\C-f))
(defun latex/font-normal () (interactive) (TeX-font nil ?\C-n))
(defun latex/font-serif () (interactive) (TeX-font nil ?\C-r))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))
(defun latex/font-upright () (interactive) (TeX-font nil ?\C-u))

(use-package tex
  :hook
  (latex-mode . outline-minor-mode)
  (latex-mode . visual-line-mode)
  :straight auctex
  :config
  (jl/auctex-keys)


  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)



  ;; Folding environments
  (defun latex-fold-env-all ()
    (interactive)
    (let ((env (read-from-minibuffer "Fold Environment: ")))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward (format "begin{%s}" env) nil t)
	  (TeX-fold-env)))))

  ;;(add-hook 'LaTeX-mode-hook
  (lambda ()
    (LaTeX-add-environments
     '("theorem")
     '("proof")
     '("lemma")
     '("proposition")
     '("corollary")
     '("example")
     '("tcolorbox")
     '("tikzcd")
     '("definition")
     '("align*"))
    )


  (setq font-latex-math-environments
	(quote
	 ("display" "displaymath" "equation" "eqnarray" "gather" "math" "multline" "align" "alignat" "xalignat" "xxalignat" "flalign" "tikzcd")))

  (setq reftex-plug-into-auctex t
	reftex-label-alist
	'(("theorem" ?h "thm:" "~\\ref{%s}" t   ("theorem" "th.") -3)
	  ("proof"   ?g "pf:"  "~\\ref{%s}" t   ("proof" "pf.") -3)
	  ("lemma"   ?l "lem:" "~\\ref{%s}" nil ("lemma"   "le.") -2)
	  ("proposition" ?p "prp:" "~\\ref{%s}" t   ("proposition" "pr.") -3)
	  ("corollary" ?c "cor:" "~\\ref{%s}" t   ("corollary" "co.") -3)
	  ("example" ?a "ex:" "~\\ref{%s}" t   ("example" "ex.") -3)
	  ("tcolorbox" ?b  "tcb:" "~\\ref{%s}" t   ("tcolorbox" "cb.") -3)
	  ("tikzcd" ?j "cd:" "~\\ref{%s}" t  ("tikzcd" "cd.") -3)
	  ("definition" ?d "def:" "~\\ref{%s}" t   ("definition" "de.") -3))
	)
  (setq TeX-view-program-selection '((output-pdf "Zathura"))
	TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

  )

(use-package auctex-latexmk)
(auctex-latexmk-setup)

(use-package ivy-bibtex
  :init
  (jl/major-modes
    :keymaps 'bibtex-mode-map
    :states 'normal
    "m" 'ivy-bibtex
    )
  )

(use-package org-ref
   :commands (org-ref-bibtex-next-entry
              org-ref-bibtex-previous-entry
              org-ref-insert-link
              org-ref-open-in-browser
              org-ref-open-bibtex-notes
              org-ref-open-bibtex-pdf
              org-ref-bibtex-hydra/body
              org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
              org-ref-sort-bibtex-entry
              arxiv-add-bibtex-entry
              arxiv-get-pdf-add-bibtex-entry
              doi-utils-add-bibtex-entry-from-doi
              isbn-to-bibtex
              pubmed-insert-bibtex-from-pmid)
   :init
   (add-hook 'org-mode-hook (lambda () (require 'org-ref)))

   (setq org-ref-completion-library 'org-ref-ivy-cite)

   (evil-define-key 'normal bibtex-mode-map
     (kbd "C-j") 'org-ref-bibtex-next-entry
     (kbd "C-k") 'org-ref-bibtex-previous-entry
     "gj" 'org-ref-bibtex-next-entry
     "gk" 'org-ref-bibtex-previous-entry)

   (jl/major-modes
     :keymaps 'bibtex-mode-map
     :states 'normal
     "l" '(:ignore t :which-key "lookup")
     )
   (jl/major-modes
     :keymaps 'bibtex-mode-map
     :states 'normal
     ;; Navigation
     "j" 'org-ref-bibtex-next-entry
     "k" 'org-ref-bibtex-previous-entry

     ;; Open
     "b" 'org-ref-open-in-browser
     "n" 'org-ref-open-bibtex-notes
     "p" 'org-ref-open-bibtex-pdf

     ;; Misc
     "h" 'org-ref-bibtex-hydra/body
     "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
     "s" 'org-ref-sort-bibtex-entry

     ;; Lookup utilities
     "la" 'arxiv-add-bibtex-entry
     "lA" 'arxiv-get-pdf-add-bibtex-entry
     "ld" 'doi-utils-add-bibtex-entry-from-doi
     "li" 'isbn-to-bibtex
     "lp" 'pubmed-insert-bibtex-from-pmid)


  (jl/major-modes
    :keymaps 'latex-mode-map
    :states 'normal
    "ic" 'org-ref-insert-link)
  )

(defun jl/treemacs-keys ()
  (jl/SPC-keys
    "0"  'treemacs-select-window
    "ft" 'treemacs
    )
  )

(use-package treemacs
  :ensure t
  :defer t
  :init
  (jl/treemacs-keys)
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package yasnippet
  :config
   (jl/SPC-keys
    "is" 'ivy-yasnippet)
  )
(yas-global-mode 1)

(use-package yasnippet-snippets)
(use-package ivy-yasnippet
  :after yasnippet
  )

(use-package company
  :config
  )
(global-company-mode 1)

(defun org-bold ()
  (interactive)
  (org-emphasize ?\*)
  )

(defun org-italic ()
  (interactive)
  (org-emphasize ?\/)
  )

(defun org-underline ()
  (interactive)
  (org-emphasize ?\_)
  )

(defun org-code ()
  (interactive)
  (org-emphasize ?\~)
  )

(defun org-strike-through ()
  (interactive)
  (org-emphasize ?\+)
  )

(defun org-verbatim ()
  (interactive)
  (org-emphasize ?\=)
  )

(defun jl/org-mode-key-bindings ()
  (jl/major-modes
   :states 'normal
   :keymaps 'org-mode-map
   :major-modes '(org-mode t)
   "b" '(:ignore t :which-key "Babel")
   "d" '(:ignore t :which-key "Dates")
   "e" '(:ignore t :which-key "Export")
   "f" '(:ignore t :which-key "Feeds")
   "i" '(:ignore t :which-key "Insert")
   "iD" '(:ignore t :which-key "Download")
   "m" '(:ignore t :which-key "More")
   "s" '(:ignore t :which-key "Trees/Subtrees")
   "T" '(:ignore t :which-key "Toggles")
   "t" '(:ignore t :which-key "Tables")
   "td" '(:ignore t :which-key "Delete")
   "ti" '(:ignore t :which-key "Insert")
   "tt" '(:ignore t :which-key "Toggle")
   "C" '(:ignore t :which-key "Clocks")
   "x" '(:ignore t :which-key "Text")
   "r" '(:ignore t :which-key "Org Roam2")
   "rd" '(:ignore t :which-key "Dailies")
   "rt" '(:ignore t :which-key "Tags")

   "'" 'org-edit-special
   "c" 'org-capture

   ;; Clock
   ;; These keybindings should match those under the "aoC" prefix (below)
   "Cc" 'org-clock-cancel
   "Cd" 'org-clock-display
   "Ce" 'org-evaluate-time-range
   "Cg" 'org-clock-goto
   "Ci" 'org-clock-in
   "CI" 'org-clock-in-last
   ;; "Cj" 'spacemacs/org-clock-jump-to-current-clock
   "Co" 'org-clock-out
   "CR" 'org-clock-report
   "Cr" 'org-resolve-clocks

   "dd" 'org-deadline
   "ds" 'org-schedule
   "dt" 'org-time-stamp
   "dT" 'org-time-stamp-inactive
   "ee" 'org-export-dispatch
   "fi" 'org-feed-goto-inbox
   "fu" 'org-feed-update-all

   "a" 'org-agenda

   "p" 'org-priority

   "Tc" 'org-toggle-checkbox
   "Te" 'org-toggle-pretty-entities
   "Ti" 'org-toggle-inline-images
   "Tn" 'org-num-mode
   "Tl" 'org-toggle-link-display
   "Tt" 'org-show-todo-tree
   "TT" 'org-todo
   "TV" 'space-doc-mode
   "Tx" 'org-latex-preview

   ;; More cycling options (timestamps, headlines, items, properties)
   "L" 'org-shiftright
   "H" 'org-shiftleft
   "J" 'org-shiftdown
   "K" 'org-shiftup

   ;; Change between TODO sets
   "C-S-l" 'org-shiftcontrolright
   "C-S-h" 'org-shiftcontrolleft
   "C-S-j" 'org-shiftcontroldown
   "C-S-k" 'org-shiftcontrolup

   ;; Subtree editing
   "sa" 'org-toggle-archive-tag
   "sA" 'org-archive-subtree-default
   "sb" 'org-tree-to-indirect-buffer
   "sd" 'org-cut-subtree
   "sy" 'org-copy-subtree
   "sh" 'org-promote-subtree
   "sj" 'org-move-subtree-down
   "sk" 'org-move-subtree-up
   "sl" 'org-demote-subtree
   "sn" 'org-narrow-to-subtree
   "sw" 'widen
   "sr" 'org-refile
   "ss" 'org-sparse-tree
   "sS" 'org-sort

   ;; tables
   "ta" 'org-table-align
   "tb" 'org-table-blank-field
   "tc" 'org-table-convert
   "tdc" 'org-table-delete-column
   "tdr" 'org-table-kill-row
   "te" 'org-table-eval-formula
   "tE" 'org-table-export
   "tf" 'org-table-field-info
   "th" 'org-table-previous-field
   "tH" 'org-table-move-column-left
   "tic" 'org-table-insert-column
   "tih" 'org-table-insert-hline
   "tiH" 'org-table-hline-and-move
   "tir" 'org-table-insert-row
   "tI" 'org-table-import
   "tj" 'org-table-next-row
   "tJ" 'org-table-move-row-down
   "tK" 'org-table-move-row-up
   "tl" 'org-table-next-field
   "tL" 'org-table-move-column-right
   "tn" 'org-table-create
   "tN" 'org-table-create-with-table.el
   "tr" 'org-table-recalculate
   "tR" 'org-table-recalculate-buffer-tables
   "ts" 'org-table-sort-lines
   "ttf" 'org-table-toggle-formula-debugger
   "tto" 'org-table-toggle-coordinate-overlays
   "tw" 'org-table-wrap-region

   ;; Source blocks / org-babel
   "bp"     'org-babel-previous-src-block
   "bn"     'org-babel-next-src-block
   "be"     'org-babel-execute-maybe
   "bo"     'org-babel-open-src-block-result
   "bv"     'org-babel-expand-src-block
   "bu"     'org-babel-goto-src-block-head
   "bg"     'org-babel-goto-named-src-block
   "br"     'org-babel-goto-named-result
   "bb"     'org-babel-execute-buffer
   "bs"     'org-babel-execute-subtree
   "bd"     'org-babel-demarcate-block
   "bt"     'org-babel-tangle
   "bf"     'org-babel-tangle-file
   "bc"     'org-babel-check-src-block
   "bj"     'org-babel-insert-header-arg
   "bl"     'org-babel-load-in-session
   "bi"     'org-babel-lob-ingest
   "bI"     'org-babel-view-src-block-info
   "bz"     'org-babel-switch-to-session
   "bZ"     'org-babel-switch-to-session-with-code
   "ba"     'org-babel-sha1-hash
   "bx"     'org-babel-do-key-sequence-in-edit-buffer
   ;; "b."     'spacemacs/org-babel-transient-state/body
   ;; Multi-purpose keys
   "," 'org-ctrl-c-ctrl-c
   "*" 'org-ctrl-c-star
   "-" 'org-ctrl-c-minus
   "#" 'org-update-statistics-cookies
   "RET"   'org-ctrl-c-ret
   "M-RET" 'org-meta-return
   ;; attachments
   "A" 'org-attach
   ;; insertion
   "ib" 'org-insert-structure-template
   "id" 'org-insert-drawer
   "ie" 'org-set-effort
   "if" 'org-footnote-new
   "ih" 'org-insert-heading
   "iH" 'org-insert-heading-after-current
   "ii" 'org-insert-item
   ;; "iK" 'spacemacs/insert-keybinding-org
   "il" 'org-insert-link
   "in" 'org-add-note
   "ip" 'org-set-property
   "is" 'org-insert-subheading
   "it" 'org-set-tags-command
   ;; region manipulation
   "xb" 'org-bold
   "xc" 'org-code
   "xi" 'org-italic
   "xo" 'org-open-at-point
   ;; "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
   "xs" 'org-strike-through
   "xu" 'org-underline
   "xv" 'org-verbatim

   )
)

;; Add global evil-leader mappings. Used to access org-agenda
  ;; functionalities – and a few others commands – from any other mode.
(defun jl/org-mode-global-keys ()
  (jl/SPC-keys
   "ao" '(:ignore t :which-key "org")
   "aor" '(:ignore t :which-key "roam")
   "aoC" '(:ignore t :which-key "clocks")
   "aof" '(:ignore t :which-key "feeds")

   "ao#" 'org-agenda-list-stuck-projects
   "aoa" 'org-agenda-list
   "aoo" 'org-agenda
   "aoc" 'org-capture
   "aoe" 'org-store-agenda-views
   "aofi" 'org-feed-goto-inbox
   "aofu" 'org-feed-update-all

   ;; Clock
   ;; These keybindings should match those under the "mC" prefix (above)
   "aoCc" 'org-clock-cancel
   "aoCg" 'org-clock-goto
   "aoCi" 'org-clock-in
   "aoCI" 'org-clock-in-last
   ;; "aoCj" 'spacemacs/org-clock-jump-to-current-clock
   "aoCo" 'org-clock-out
   "aoCr" 'org-resolve-clocks

   "aol" 'org-store-link
   "aom" 'org-tags-view
   "aos" 'org-search-view
   "aot" 'org-todo-list
   ;; SPC C- capture/colors
   "Cc" 'org-capture
   )
  )

(jl/org-mode-global-keys)

(defun jl/org-font-setup ()
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  )

(use-package evil-org
:ensure t
:after org
:hook (org-mode . (lambda () evil-org-mode))
:config
(require 'evil-org-agenda)
(evil-org-agenda-set-keys))

(defun jl/org-roam-global-keys ()
  (jl/SPC-keys
   "aor" '(:ignore t :which-key "org-roam")
   "aord" '(:ignore t :which-key "dailies")
   "aort" '(:ignore t :which-key "tags")

   "aordy" 'org-roam-dailies-goto-yesterday
   "aordt" 'org-roam-dailies-goto-today
   "aordT" 'org-roam-dailies-goto-tomorrow
   "aordd" 'org-roam-dailies-goto-date
   "aorf" 'org-roam-node-find
   "aorn" 'org-roam-node-find
   "aorg" 'org-roam-graph
   "aori" 'org-roam-node-insert
   "aorl" 'org-roam-buffer-toggle
   "aorta" 'org-roam-tag-add
   "aortr" 'org-roam-tag-remove
   "aora" 'org-roam-alias-add
   "aorI" 'org-id-get-create
   )
  )

(defun jl/org-roam-key-bindings ()
  (jl/major-modes
   :states 'normal
   :keymaps 'org-mode-map
   :major-mode '(org-mode)

   "rdy" 'org-roam-dailies-goto-yesterday
   "rdt" 'org-roam-dailies-goto-today
   "rdT" 'org-roam-dailies-goto-tomorrow
   "rdd" 'org-roam-dailies-goto-date
   "rf" 'org-roam-node-find
   "rn" 'org-roam-node-find
   "rg" 'org-roam-graph
   "ri" 'org-roam-node-insert
   "rl" 'org-roam-buffer-toggle
   "rta" 'org-roam-tag-add
   "rtr" 'org-roam-tag-remove
   "ra" 'org-roam-alias-add
   "rI" 'org-id-get-create
   )
  )

(use-package websocket
  :after org-roam
  )

(use-package simple-httpd
  :after org-roam
  )

(use-package org-roam-ui
  :straight (:host github
		   :repo "org-roam/org-roam-ui"
		   :branch "main"
		   :files ("*.el" "out")
		   )
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  )

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  (jl/org-roam-global-keys)
  :config
  (setq org-roam-graph-viewer #'browse-url-firefox
	org-roam-directory "~/Documents/Org/Roam"
	org-roam-dailies-directory "~/Documents/Org/Roam/Dailies/")
  (require 'org-roam-protocol)
  ;; (setq org-roam-dailies-capture-templates
  ;;       '(("d" "today" plain
  ;;          #'org-roam-capture--get-point
  ;;          ""
  ;;          :file-name "~/Documents/Org/Roam/journals/%<%Y-%m-%d>"
  ;;          :head "#+title: %<%Y-%m-%d>\n#+roam_tags: Daily\n* [/] Daily Todos \n 1. [ ]\n* Daily Tasks\n* Morning Thoughts\n* Evening Reflections\n"
  ;;          :unnarrowed t
  ;;          :immediate-finish t
  ;;          :jump-to-captured t
  ;;          ;; :olp ("")
  ;;          )
  ;;         )
  ;;       )
  )

(use-package org-roam-bibtex
  :after (org-roam org-ref)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  )

(defun jl/org-mode-setup ()
  (visual-line-mode 1)
  (variable-pitch-mode 1)
  (smartparens-mode 1)
  (org-fragtog-mode 1)
  (rainbow-delimiters-mode 1)
  )

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(defun jl/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jl/org-mode-visual-fill))

(use-package smartparens)
(use-package org-fragtog)

;; (require 'ox-bibtex)
(require 'ox-publish)

(use-package org
  :hook (org-mode . jl/org-mode-setup)
  :config
  (jl/org-roam-key-bindings)
  (jl/org-mode-key-bindings)
  (setq org-ellipsis " ▼"
        org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELLED"))
        org-todo-keyword-faces '(("WAITING" . "aquamarine1") ("CANCELLED" . "red"))
        org-startup-indented t
        org-hide-emphasis-markers t
        org-src-tab-acts-natively t
        org-pretty-entities t
        org-startup-folded t
        org-hide-block-startup t
        org-edit-src-content-indentation 0
        org-startup-with-latex-preview t
        org-enable-reveal-js-support t
        org-re-reveal-root "file:///home/james/emacs-packages/reveal.js"
        org-agenda-files '("~/Documents/Calendar/Agenda.org"
                           ;; "~/Documents/Org/GTD/inbox.org"
                           ;; "~/Documents/Org/GTD/gtd.org"
                           ;; "~/Documents/Org/GTD/tickler.org"
                           ;; "~/Documents/Org/GTD/Mobile Inbox.org"
                           )
        org-export-backends '(ascii beamer html icalendar latex md odt)
        )
  ;; Latex in Org
  (setq org-preview-latex-default-process 'dvisvgm
        org-highlight-latex-and-related '(latex script entities)
        org-format-latex-options
        '(:foreground default :background default :scale 0.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                      ("begin" "$1" "$" "$$" "\\(" "\\[")
                      )
        reftex-default-bibliography '("/home/james/Documents/TeX/common/bibliography.bib")
        org-ref-default-bibliography "/home/james/Documents/TeX/common/bibliography.bib"
        )

  ;; Org capture
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "~/Documents/Org/GTD/inbox.org" "Tasks")
           "* TODO %i%?")
          ("T" "Tickler" entry
           (file+headline "~/Documents/Org/GTD/tickler.org" "Tickler")
           "* %i%? \n %U")
          ("n" "Notes" entry
           (file "~/Documents/Org/Notes/notes.org")
           "* %?")
          ("r" "Research Notes" entry
           (file "~/Documents/Org/Research/notes.org")
           "* %u \n %?")
          ("w" "org-protocol" entry (file "~/Documents/Org/GTD/inbox.org")
           "* TODO Review %a\n%U\n%:initial\n" :immediate-finish t)
          )
        )

  ;; GTD in org
  (setq org-refile-targets '(("~/Documents/Org/GTD/gtd.org" :maxlevel . 3)
                             ("~/Documents/Org/GTD/someday.org" :level . 1)
                             ("~/Documents/Org/GTD/tickler.org" :maxlevel . 2)))

  ;; Org publish
  (setq org-publish-project-alist
        '(("jeslie0.github.io Posts" ; Blog name
           :base-directory "~/Documents/jeslie0.github.io/org/"
           :base-extension "org"
                                        ;Path to Jekyll posts
           :publishing-directory "~/Documents/jeslie0.github.io/_posts/"
           ;; :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :body-only t
           ;; :auto-sitemap t
           ;; :sitemap-title "Blog Index"
           ;; :sitemap-filename "blog-index.org"
           ;; :sitemap-style list
           )
          ("jeslie0.github.io main"
           :base-directory "~/Documents/jeslie0.github.io/org/"
           :base-extension "org"
           :publishing-directory "~/Documents/jeslie0.github.io"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           ;; :html-extension "html"
           :auto-preamble nil
           ;; :body-only t
           )
          ("UniAgda HTML"
           :base-directory "~/agdalibs/Univalent-Agda/org/"
           :base-extension "org"
           :publishing-directory "~/jeslie0.github.io/UniAgda/"
           :publishing-function org-html-publish-to-html
           :auto-preamble nil
           ;; :body-only t
           )
          )
        )

  ;; Org babel
  (setq org-babel-load-languages '((shell . t)
                                   (python . t)
                                   (js . t)
                                   (emacs-lisp . t)
                                   (latex . t)
                                   (haskell . t)
                                   (C . t)))
  (jl/org-font-setup)
  )

(require 'org-protocol)
(setq org-protocol-default-template-key "w")

(defun jl/org-caldav-keybindings ()
  (jl/C-c-keys
   :keymaps 'org-agenda-mode-map
   "S" 'org-caldav-sync)
  )

(use-package org-caldav
  :init
  (setq org-caldav-url "https://posteo.de:8443/calendars/jamesleslie"
        org-caldav-calendar-id "default"
        org-caldav-inbox "~/Documents/Calendar/Agenda.org"
        org-caldav-files '("~/Documents/Calendar/Appointments.org")
        org-icalendar-timezone "America/Toronto"
        org-caldav-delete-org-entries 'always
        org-caldav-delete-calendar-entries 'never)
  )

(jl/org-caldav-keybindings)

(with-eval-after-load 'org-capture
  (defun spacemacs//org-capture-start ()
    "Make sure that the keybindings are available for org capture."
    (jl/C-c-keys
      :keymaps 'org-capture-mode-map
      "a" 'org-capture-kill
      "c" 'org-capture-finalize
      "k" 'org-capture-kill
      "r" 'org-capture-refile)
    ;; Evil bindins seem not to be applied until at least one
    ;; Evil state is executed
    (evil-normal-state))
  ;; Must be done everytime we run org-capture otherwise it will
  ;; be ignored until insert mode is entered.
  (add-hook 'org-capture-mode-hook 'spacemacs//org-capture-start))

(with-eval-after-load 'org-src
  (jl/C-c-keys
    :hooks 'org-src-mode-hook
    "c" 'org-edit-src-exit
    "a" 'org-edit-src-abort
    "k" 'org-edit-src-abort)
  )

(defun jl/projectile-keys ()
  (jl/SPC-keys
    ;; Project
    "p!" 'projectile-run-shell-command-in-root
    "p&" 'projectile-run-async-shell-command-in-root
    "p%" 'projectile-replace-regexp
    "pa" 'projectile-toggle-between-implementation-and-test
    "pb" 'projectile-switch-to-buffer
    "pc" 'projectile-compile-project
    "pd" 'projectile-find-dir
    "pD" 'projectile-dired
    "pe" 'projectile-edit-dir-locals
    "pf" 'projectile-find-file
    "pF" 'projectile-find-file-dwim
    "pg" 'projectile-find-tag
    "pG" 'projectile-regenerate-tags
    "pI" 'projectile-invalidate-cache
    "pk" 'projectile-kill-buffers
    "pp" 'projectile-switch-project
    "pr" 'projectile-recentf
    "pR" 'projectile-replace
    "pT" 'projectile-test-project
    "pv" 'projectile-vc))

(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :config (projectile-mode)
  (jl/projectile-keys)
  )

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(defun jl/magit-keys ()
  (jl/SPC-keys
    "gs" 'magit-status
    ))

(use-package magit
  :init
  (jl/magit-keys)
  :config
  (define-key magit-status-mode-map (kbd "SPC") nil)
  )

(use-package flycheck
  :init
  (global-flycheck-mode t)
  )

(defun jl/flyspell ()
  (jl/SPC-keys
    "Sab" 'spacemacs/add-word-to-dict-buffer
    "Sag" 'spacemacs/add-word-to-dict-global
    "Sas" 'spacemacs/add-word-to-dict-session
    "Sb" 'flyspell-buffer
    "Sr" 'flyspell-region
    "Sd" 'spell-checking/change-dictionary
    "Sn" 'flyspell-goto-next-error
    "Ss" 'flyspell-correct-at-point)

  (jl/SPC-keys
    "Sa" '(:ignore t :which-key "add word to dict")
    )
  )

(defun spell-checking/add-flyspell-hook (hook)
  "Add `flyspell-mode' to the given HOOK, if
`spell-checking-enable-by-default' is true."
  (when spell-checking-enable-by-default
    (add-hook hook 'flyspell-mode)))

(defun spell-checking/change-dictionary ()
  "Change the dictionary. Use the ispell version if
auto-dictionary is not used, use the adict version otherwise."
  (interactive)
  (if (fboundp 'adict-change-dictionary)
      (adict-change-dictionary)
    (call-interactively 'ispell-change-dictionary)))

(defun spacemacs/add-word-to-dict-buffer ()
  "Save word at point as correct in current buffer."
  (interactive)
  (spacemacs//add-word-to-dict 'buffer))

(defun spacemacs/add-word-to-dict-global ()
  "Save word at point as a correct word globally."
  (interactive)
  (spacemacs//add-word-to-dict 'save))

(defun spacemacs/add-word-to-dict-session ()
  "Save word at point as correct in current session."
  (interactive)
  (spacemacs//add-word-to-dict 'session))

(defun spacemacs//add-word-to-dict (scope)
  "Save word at point as a correct word.
SCOPE can be:
`save' to save globally,
`session' to save in current session or
`buffer' for buffer local."
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (if (spacemacs//word-in-dict-p (car word))
          (error "%s is already in dictionary" (car word))
        (progn
          (flyspell-do-correct scope nil (car word) current-location
                               (cadr word) (caddr word) current-location)
          (ispell-pdict-save t))))))

(defun spacemacs//word-in-dict-p (word)
  "Check if WORD is defined in any of the active dictionaries."
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let (poss ispell-filter)
    ;; now check spelling of word.
    (ispell-send-string "%\n")	;put in verbose mode
    (ispell-send-string (concat "^" word "\n"))
    ;; wait until ispell has processed word
    (while (progn
             (accept-process-output ispell-process)
             (not (string= "" (car ispell-filter)))))
    ;; Remove leading empty element
    (setq ispell-filter (cdr ispell-filter))
    ;; ispell process should return something after word is sent.
    ;; Tag word as valid (i.e., skip) otherwise
    (or ispell-filter
        (setq ispell-filter '(*)))
    (if (consp ispell-filter)
        (setq poss (ispell-parse-output (car ispell-filter))))
    (or (eq poss t) (stringp poss))))

(use-package flyspell
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  :config
  (jl/flyspell)
  (setq ispell-dictionary "en_GB")
  )

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :commands (flyspell-correct-ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  )

(use-package undo-tree
  :init
   (progn
      (setq undo-tree-visualizer-timestamps t
            undo-tree-visualizer-diff t
            ;; 10X bump of the undo limits to avoid issues with premature
            ;; Emacs GC which truncages the undo history very aggresively
            undo-limit 800000
            undo-strong-limit 12000000
            undo-outer-limit 120000000)
      (global-undo-tree-mode))
   :bind (:map undo-tree-visualizer-mode-map
	       )
   :config
   (jl/SPC-keys
     "au" 'undo-tree-visualize
   )

  (general-evil-define-key '(normal motion) 'undo-tree-visualizer-mode-map
    "j"  'undo-tree-visualize-redo
    "k"  'undo-tree-visualize-undo
    "h"  'undo-tree-visualize-switch-branch-left
    "l"  'undo-tree-visualize-switch-branch-right
    )
  )

(setq epa-pinentry-mode 'loopback)
(setq auth-sources '(password-store))
(auth-source-pass-enable)

(defun jl/mu4e-keys ()
  (jl/major-modes
    :states '(normal visual emacs operator motion)
    :keymaps 'mu4e-compose-mode-map
    :major-modes '(mu4e-compose-mode)
    "," 'message-send-and-exit
    "c" 'message-send-and-exit
    "k" 'message-kill-buffer
    "a" 'message-kill-buffer
    "s" 'message-dont-send         ; saves as draft
    "e" 'mml-secure-message-encrypt-pgpmime
    "S" 'mml-secure-sign-pgpmime
    "f" 'mml-attach-file)

  (jl/major-modes
    :states '(normal visual emacs operator motion)
    :keymaps 'mu4e-headers-mode-map
    :major-modes t
    "c" '(:ignore t :which-key "compose")
    "m" '(:ignore t :which-key "marking")
    "C" 'mu4e-context-switch
    "s" 'mu4e-headers-search
    "O" 'mu4e-headers-change-sorting
    "U" 'mu4e-update-mail-and-index
    "x" 'mu4e-mark-execute-all
    "cr" 'mu4e-compose-reply
    "cf" 'mu4e-compose-forward
    "cc" 'mu4e-compose-new
    "ma" 'mu4e-headers-mark-for-action
    "md" 'mu4e-headers-mark-for-trash
    "m=" 'mu4e-headers-mark-for-untrash
    "mD" 'mu4e-headers-mark-for-delete
    "mR" 'mu4e-headers-mark-for-refile
    "mr" 'mu4e-headers-mark-for-read
    "mu" 'mu4e-headers-mark-for-unread
    "mf" 'mu4e-headers-mark-for-flag
    "mF" 'mu4e-headers-mark-for-unflag
    "mU" 'mu4e-headers-mark-for-unmark
    "m*" 'mu4e-headers-mark-for-something)

  (jl/major-modes
    :states '(normal visual emacs operator motion)
    :keymaps 'mu4e-view-mode-map
    :major-modes t
    "t" '(:ignore t :which-key "toggle")
    "m" '(:ignore t :which-key "marking")
    "g" '(:ignore t :which-key "url")
    "a" '(:ignore t :which-key "attachments")
    )

  (general-evil-define-key '(normal motion) 'mu4e-main-mode-map
    "j" 'mu4e~headers-jump-to-maildir
    )

  )



(defun jl/mu4e-global-keys ()
  (jl/C-c-keys
    "e" 'mu4e
    )
  (jl/SPC-keys
    "ae" 'mu4e
  )
)

(defun jl/mu4e-shortcuts ()
  (setq mu4e-maildir-shortcuts
	'(
	  ;; ("/Gmail/Inbox" . ?i)
          ;; ("/Gmail/[Gmail]/Sent Mail" . ?s)
          ;; ("/Gmail/[Gmail]/All Mail" . ?a)
          ("/Posteo/Inbox" . ?I)
          ("/Posteo/Sent" . ?S)
          ("/Posteo/Archive" . ?A)
          ("/Posteo/University" . ?U)
	  )
	)
  )

(defun jl/mu4e-contexts ()
  (setq mu4e-compose-signature "James Leslie")
  (setq mu4e-contexts
	`(
	  ;; ,(make-mu4e-context
          ;;    :name "personal - jamesleslie314@gmail.com"
          ;;    :match-func (lambda (msg)
          ;;                  (when msg
          ;;                    (mu4e-message-contact-field-matches msg
	  ;; 							 :to "jamesleslie314@gmail.com")))
          ;;    :vars '(
          ;;            (mu4e-sent-messages-behavior . sent)
          ;;            (mu4e-sent-folder . "/Gmail/[Gmail]/Sent Mail")
          ;;            (mu4e-drafts-folder . "/Gmail/[Gmail]/Drafts")
          ;;            (mu4e-trash-folder . "/Gmail/[Gmail]/Trash")
          ;;            (mu4e-refile-folder . "/Gmail/[Gmail]/All Mail")
          ;;            (user-mail-address . "jamesleslie314@gmail.com")
          ;;            (user-full-name . "James Leslie")

          ;;            ;; SMTP configuration
          ;;            (smtpmail-smtp-user . "jamesleslie314@gmail.com")
          ;;            (smtpmail-default-smtp-server . "smtp.gmail.com")
          ;;            (smtpmail-smtp-server . "smtp.gmail.com")
          ;;            (smtpmail-smtp-service . 587)
          ;;            )
          ;;    )
           ,(make-mu4e-context
             :name "Posteo - jamesleslie@posteo.net"
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "jamesleslie@posteo.net")))
             :vars '(
                     (mu4e-sent-messages-behavior . sent)
                     (mu4e-sent-folder . "/Posteo/Sent")
                     (mu4e-drafts-folder . "/Posteo/Drafts")
                     (mu4e-trash-folder . "/Posteo/Trash")
                     (mu4e-refile-folder . "/Posteo/Archive")
                     (user-full-name . "James Leslie")
                     (user-mail-address . "jamesleslie@posteo.net")

                     ;; SMTP configuration
                     (smtpmail-smtp-user . "jamesleslie@posteo.net")
                     (smtpmail-default-smtp-server . "posteo.de")
                     (smtpmail-smtp-server . "posteo.de")
                     (smtpmail-stream-type . starttls)
                     (smtpmail-smtp-service . 587)
                     )
             )
           )
	)
  (setq mu4e-context-policy 'pick-first)

  ;; Parse each context and gather a list of their `user-mail-address'es
  (setq mu4e-user-mail-address-list
	(mapcar (lambda (context)
                  (let ((vars (mu4e-context-vars context)))
                    (cdr (assq 'user-mail-address vars))))
		mu4e-contexts))
)

(defun jl/mu4e-rich ()
  (setq ;; mu4e-html2text-command 'mu4e-shr2text
   mu4e-html2text-command "w3m -dump -T text/html -o display_link_number=true"
   mu4e-view-show-images t
   mu4e-image-max-width 800
   mu4e-view-prefer-html t
   mu4e-use-fancy-chars t)
)

(defun jl/mu4e-attachement-warning ()
  (defun mbork/message-attachment-present-p ()
    "Return t if an attachment is found in the current message."
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(when (search-forward "<#part" nil t) t))))

  (defcustom mbork/message-attachment-intent-re
    (regexp-opt '("I attach"
		  "I have attached"
		  "I've attached"
		  "I have included"
		  "I've included"
		  "see the attached"
		  "see the attachment"
		  "attached file"
		  "Attached"))
    "A regex which - if found in the message, and if there is no
attachment - should launch the no-attachment warning.")

  (defcustom mbork/message-attachment-reminder
    "Are you sure you want to send this message without any attachment? "
    "The default question asked when trying to send a message
containing `mbork/message-attachment-intent-re' without an
actual attachment.")

  (defun mbork/message-warn-if-no-attachments ()
    "Ask the user if s?he wants to send the message even though
there are no attachments."
    (when (and (save-excursion
		 (save-restriction
		   (widen)
		   (goto-char (point-min))
		   (re-search-forward mbork/message-attachment-intent-re nil t)))
	       (not (mbork/message-attachment-present-p)))
      (unless (y-or-n-p mbork/message-attachment-reminder)
	(keyboard-quit))))

  (add-hook 'message-send-hook #'mbork/message-warn-if-no-attachments)
  )

(defun jl/mu4e-reply-quote ()
  ;; customize the reply-quote-string
  (setq message-citation-line-format "On %a, %d %b, %Y at %R %f wrote:\n")
  ;; choose to use the formatted string
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  )

(straight-use-package '(mu4e-thread-folding :host github
					    :repo "rougier/mu4e-thread-folding"
					    :branch "master"))



(use-package mu4e-thread-folding

  :hook (mu4e-headers-mode . mu4e-thread-folding-mode)
  :config
  (setq mu4e-headers-found-hook '(mu4e-headers-mark-threads mu4e-headers-fold-all))
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
				 :shortname ""
				 :function (lambda (msg) "  "))))
  (setq mu4e-headers-fields '((:empty         .    2)
                              (:human-date    .   12)
                              (:flags         .    6)
                              (:mailing-list  .   10)
                              (:from          .   22)
                              (:subject       .   nil)))
  (define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
  (define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
  (define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
  (define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
  (define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)
  )

(defun jl/mu4e-pgp ()
  (setq mml-secure-openpgp-sign-with-sender t) ;; Sign all outgoing emails
  (setq mml-secure-openpgp-signers '("7BC253447F901C3EBD46AB5EDDFB27273B2BFBB6")) ;; Sign emails with this key
  (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign)
  )

(use-package smtpmail
  :after mu4e)

(use-package mu4e
  :ensure nil
  :init
  (jl/mu4e-global-keys)
  (add-hook 'mu4e-compose-mode-hook 'visual-fill-column-mode)
  (add-hook 'mu4e-compose-mode-hook 'variable-pitch-mode)
  (add-hook 'mu4e-view-mode-hook 'visual-fill-column-mode)

  :config
  (jl/mu4e-keys)
  (jl/mu4e-shortcuts)
  (jl/mu4e-contexts)
  (jl/mu4e-rich)
  (jl/mu4e-attachement-warning)
  (jl/mu4e-reply-quote)
  (jl/mu4e-pgp)
  (setq mu4e-maildir "~/.email"
	mu4e-get-mail-command "mbsync -c ~/.emacs.d/.mbsyncrc -a"
	mu4e-compose-signature-auto-include t
	mu4e-view-show-addresses t
	mu4e-headers-include-related nil
	mu4e-headers-skip-duplicates t
	mu4e-headers-auto-update t
	mu4e-update-interval 300
	message-send-mail-function 'smtpmail-send-it
	mu4e-attachment-dir  "~/Downloads/"
	mu4e-change-filenames-when-moving t
	mu4e-compose-format-flowed t
	mu4e-compose-dont-reply-to-self t
	mu4e-sent-messages-behavior 'sent
	mu4e-use-fancy-chars t
	mu4e-display-update-status-in-modeline nil
	)
  )

(use-package mu4e-alert
  :hook (after-init . mu4e-alert-enable-mode-line-display)
  )

(use-package pandoc-mode
  :init
  (jl/SPC-keys
    "P" '(pandoc-main-hydra/body :which-key "Pandoc")
    )
  )

(use-package avy
  :init
  (jl/SPC-keys
    "jb" 'avy-pop-mark
    "jj" 'evil-avy-goto-char-timer
    "jl" 'evil-avy-goto-line
    ;; "ju" 'spacemacs/avy-goto-url
    ;; "jU" 'spacemacs/avy-open-url
    "jw" 'evil-avy-goto-word-or-subword-1)
  )

(defun jl/iedit-keys ()
  (jl/SPC-keys
    "se" 'evil-iedit-state/iedit-mode)
  )

(use-package evil-iedit-state
  :config
  (jl/iedit-keys))

(defun jl/ranger-keys ()
  (jl/SPC-keys
    "atr" 'ranger
    )
  )

(use-package ranger
  :init
  (ranger-override-dired-mode t)
  (jl/ranger-keys)
  :config
  (setq ranger-preview-file t
	ranger-dont-show-binary t
	ranger-show-hidden t)
  )
