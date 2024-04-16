(setq consult--find-regexp-type 'basic
      consult--grep-regexp-type 'pcre
      consult--ripgrep-regexp-type 'extended)

(load-theme 'doom-one t)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)

;; (load-theme 'vscode-dark-plus t)

(defun org-roam-file-id (file)
  "Returns the id for the given org-roam FILE."
  (let* ((lines (with-temp-buffer
                 (insert-file-contents file)
                 (split-string (buffer-string) "\n" t)))
         (id (car (cdr lines))))
    (substring id 11)))

(defun previous-day (day month year)
  "Calculates the date of the day before the given one."
  (let ((days-in-month `((1 . 31)
                         (2 . ,(if (date-leap-year-p year)
                                   29
                                 28))
                         (3 . 31)
                         (4 . 30)
                         (5 . 31)
                         (6 . 30)
                         (7 . 31)
                         (8 . 31)
                         (9 . 30)
                         (10 . 31)
                         (11 . 30)
                         (12 . 31))))
    (if (eq 1 day)
        (if (eq 1 month)
            `(31 12 ,(- year 1))
          `(,(alist-get (- month 1) days-in-month) ,(- month 1) ,year))
      `(,(- day 1) ,month ,year ))))


(defun remote-table-func (day month year)
  (if (equal (org-day-of-week day month year) 1)
      "@7$3"
    (let* ((new-date (previous-day day month year))
          (new-day (car new-date))
          (new-month (car (cdr new-date)))
          (new-year (car (cdr (cdr new-date))))
          (prev-org-file (format "%s%s%s-%s-%s.org" org-roam-directory org-roam-dailies-directory new-year new-month new-day ".org")))
      (format "remote(%s, @8$3) + @7$3" (org-roam-file-id  prev-org-file)))))


(setq treesit-font-lock-level 4)
(setq-default tab-width 4)
(setq c-ts-mode-indent-offset 4)
(setq ement-notify-dbus-p nil)
;; (elfeed-tube-add-feeds '(
;;                          ;; Ordinary Things
;;                          "OrdinaryThings"
;;                          ;; Veritasium
;;                          "veritasium"
;;                          ;; Quanta Magazine
;;                          "QuantaScienceChannel"
;;                          ;; Tom Scott
;;                          "TomScottGo"
;;                          ;; Carefree Wandering
;;                          "carefreewandering"
;;                          ;; Kurzgesagt
;;                          "kurzgesagt"
;;                          ;; James Hoffmann
;;                          "jameshoffmann"
;;                          ;; Magnus Midtbo
;;                          "magmidt"
;;                          ;; Tweag
;;                          "tweag"
;;                          ;; System Crafters
;;                          "SystemCrafters"
;;                          ))

(setq doc-view-resolution 200)

;; (setq default-frame-alist
;;       (append (list
;; 	           '(min-height . 1)
;;                '(height     . 45)
;; 	           '(min-width  . 1)
;;                '(width      . 81)
;;                '(vertical-scroll-bars . nil)
;;                '(internal-border-width . 12)
;;                '(left-fringe    . 1)
;;                '(right-fringe   . 1)
;;                '(tool-bar-lines . 0)
;;                '(menu-bar-lines . 0))))
