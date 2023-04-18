(setq consult--find-regexp-type 'basic
      consult--grep-regexp-type 'pcre
      consult--ripgrep-regexp-type 'extended)

(load-theme 'doom-palenight t)
;; (load-theme 'vscode-dark-plus t)

(advice-add
 'eglot--apply-text-edits :override
 (lambda (edits &optional version)
   (atomic-change-group
     (let* ((change-group (prepare-change-group))

            (howmany (length edits))
            (reporter (make-progress-reporter
                       (format "[eglot] applying %s edits to `%s'..."
                               howmany (current-buffer))
                       0 howmany))
            (done 0))
       (mapc (pcase-lambda (`(,newText ,beg . ,end))
               (let ((source (current-buffer)))
                 (with-temp-buffer
                   (insert newText)
                   (let ((temp (current-buffer)))
                     (with-current-buffer source
                       (save-excursion
                         (save-restriction
                           (narrow-to-region beg end)

                           ;; On emacs versions < 26.2,
                           ;; `replace-buffer-contents' is buggy - it calls
                           ;; change functions with invalid arguments - so we
                           ;; manually call the change functions here.
                           ;;
                           ;; See emacs bugs #32237, #32278:
                           ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32237
                           ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278
                           (let ((inhibit-modification-hooks t)
                                 (length (- end beg))
                                 (beg (marker-position beg))
                                 (end (marker-position end)))
                             (run-hook-with-args 'before-change-functions
                                                 beg end)
                             (replace-buffer-contents temp)
                             (run-hook-with-args 'after-change-functions
                                                 beg (+ beg (length newText))
                                                 length))))
                       (progress-reporter-update reporter (cl-incf done)))))))
             (mapcar (eglot--lambda ((TextEdit) range newText)
                       (cons newText (eglot--range-region range 'markers)))
                     (reverse edits)))
       (undo-amalgamate-change-group change-group)
       (progress-reporter-done reporter)))))


;; (advice-add
;;  'flymake-languagetool--start-server :override
;;  (lambda (report-fn)
;;   "Start the LanguageTool server if we didn’t already."
;;   (let* ((source (current-buffer))
;;          (cmd (or flymake-languagetool-server-command
;;                   (list "java" "-cp" flymake-languagetool-server-jar
;;                         "org.languagetool.server.HTTPServer"
;;                         "--port" flymake-languagetool-server-port))))
;;     (make-process
;;      :name "languagetool-server" :noquery t :connection-type 'pipe
;;      :buffer " *LanguageTool server*"
;;      :command flymake-languagetool-server-command
;;      :filter
;;      (lambda (proc string)
;;        (funcall #'internal-default-process-filter proc string)
;;        (when (string-match ".*Server started\n$" string)
;;          (with-current-buffer source
;;            (setq flymake-languagetool--local t)
;;            (flymake-languagetool--checker report-fn))
;;          (set-process-filter proc nil)))
;;      :sentinel
;;      (lambda (proc _event)
;;        (when (memq (process-status proc) '(exit signal))
;;          (setq flymake-languagetool--local nil)
;;          (kill-buffer (process-buffer proc))))))))














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
