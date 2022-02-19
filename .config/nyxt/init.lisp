(define-configuration nyxt/web-mode:web-mode
    ((nyxt/web-mode::keymap-scheme
      (nyxt::define-scheme (:name-prefix "web" :import %slot-default%)
	  ;; If you want to have VI bindings overriden, just use
	  ;; `scheme:vi-normal' or `scheme:vi-insert' instead of
	  ;; `scheme:emacs'
	  scheme:vi-normal
	(list
	 "space space" 'execute-command
	 "space /" 'nyxt/web-mode::search-buffers

	 ;; Buffer
	 "space b b" 'switch-buffer
	 "space b p" 'switch-buffer-previous
	 "space b n" 'switch-buffer-next
	 "space b d" 'delete-current-buffer
	 "space b C-d" 'delete-buffer
	 "space b D" 'delete-other-buffers

	 ;; History
	 "space h t" 'nyxt/web-mode::history-tree

	 "space w d" 'delete-current-window
	 "space w n" 'make-window

	 "space q q" 'quit

	 "C-s" 'nyxt/web-mode::search-buffer

	 "v" 'nyxt/visual-mode::visual-mode
	 )))))

(define-configuration prompt-buffer ;; This is the popup buffer!
    ((style (str:concat
             %slot-default%
             (cl-css:css
              '((body :background-color "#292D3E" :color "white")
		("#prompt-area" :background-color "#292D3E")
		;; The area you input text in.
		("#input" :background-color "white")
		(".source-name" :color "white" :background-color "gray")
		(".source-content" :background-color "#292D3E")
		(".source-content th" :border "1px solid lightgray" :background-color "#292D3E")
		;; The currently highlighted option.
		("#selection" :background-color "#c3e88d" :color "black")
		(.marked :background-color "white" :font-weight "bold" :color "white")
		(.selected :background-color "#292D3E" :color "white")))))))

;;; Panel buffers are the same in regards to style.
(define-configuration (internal-buffer panel-buffer)
    ((style
      (str:concat
       %slot-default%
       (cl-css:css
	'((body :background-color "#292D3E" :color "white")
          (hr :color "white")
          (a :color "#c792ea")
          (.button :color "white" :background-color "gray")))))))

(define-configuration window
    ((message-buffer-style
      (str:concat
       %slot-default%
       (cl-css:css
	'((body :background-color "#292D3E" :color "white")))))))


(define-configuration nyxt/history-tree-mode:history-tree-mode
    ((style
      (str:concat
       %slot-default%
       (cl-css:css
	'((body :background-color "#292D3E" :color "white")
	  (a :color "#c792ea")))))))

(define-configuration browser
    (
     (session-restore-prompt :never-restore)))

(define-configuration web-buffer
    ((default-modes (append '(blocker-mode) %slot-default%))))

(define-bookmarklet-command hack-youtube-speed
    "Tweak YouTube videos' speed beyond the UI options and limits."
  "(function() {
    const rate = prompt('Set the new playback rate', 2.5);
    if (rate != null) {
        const video =
            document.getElementsByTagName('video')[0];
        video.playbackRate = parseFloat(rate);
    }})();")

;; Create a function to launch mpv with given url
(defun mpv (url)
  "MPV launches with given url using the fast profile."
  (uiop:run-program (list "mpv" url "&")))

;; Let's create a function to hint videos, convert the url to a sting, and play them in MPV
(define-command-global hint-mpv (&key nyxt/web-mode::annotate-visible-only-p)
  "Show a set of element hints, and copy the URL of the user inputted one."
  (nyxt/web-mode:query-hints "Copy element URL"
                             (lambda (nyxt/web-mode::result)
                               ;; this converts the url to a string to be used in mpv
                               (let*
                                   ((url
                                     (format nil "~a"
                                             (url (first nyxt/web-mode::result)))))
                                 ;; here we take that string and pipe it into mpv
                                 (mpv url)))
                             :annotate-visible-only-p
                             nyxt/web-mode::annotate-visible-only-p))
