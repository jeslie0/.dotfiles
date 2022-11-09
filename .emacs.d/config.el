;; (setq consult--find-regexp-type 'basic
;;       consult--grep-regexp-type 'pcre
;;       consult--ripgrep-regexp-type 'extended)


  ;; :hook (server-after-make-frame . (lambda () (load-theme
  ;;       				       'doom-palenight t)))
  ;; :init
(load-theme 'doom-palenight t)

(advice-add 'eglot--apply-text-edits :override
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
