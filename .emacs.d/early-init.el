;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)
;;; speedup blub
(let ((default-gc-threshold gc-cons-threshold)
      (default-gc-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        default-gc-percentage 0.8)
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-percentage default-gc-percentage
                    gc-cons-threshold default-gc-threshold))))

(setq load-prefer-newer t)


;;; UI Stuff
(setq inhibit-startup-message t) ;; Disables the startup message

(scroll-bar-mode -1)   ; Disables visible scroll bar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Give some breathing room
(menu-bar-mode -1)     ; Disable the menu bar
(blink-cursor-mode -1) ; Makes cursor not blink
(column-number-mode 1) ;; Adds column numbering to the modeline
(electric-indent-mode -1)


(defvar jl/prog-font
  "Source Code Pro")

(defvar jl/text-font
  "Cantarell")


(set-face-attribute 'default nil :font jl/prog-font :height 110)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font jl/prog-font :height 110)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font jl/text-font :height 120 :weight 'regular)

(set-face-attribute 'cursor nil :background "DarkGoldenrod2")
(set-face-attribute 'mode-line-active nil :inherit nil)

(setq frame-inhibit-implied-resize t)
