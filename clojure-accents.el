(require 'clojure-mode)
(require 'cider)
(require 'cider-interaction)
(require 'cider-repl)
(require 'nrepl-client)
(require 'evil)
(require 'evil-leader)

(defvar buffer-meta '()
     "Metadata about the Clojure^ buffer.")

(defun string/join (sep strings)
  (mapconcat 'identity strings sep))

(defun accent/nrepl-eval-request (input &optional ns session)
  "Send a request to eval INPUT.
If NS is non-nil, include it in the request.
Use SESSION if it is non-nil, otherwise use the current session."
  (let* ((ns (or ns (plist-get buffer-meta 'ns)))
         (buffer (plist-get buffer-meta 'name))
         (path (plist-get buffer-meta 'filename))
         (accent (plist-get buffer-meta 'filetype)))
    (append (if ns (list "ns" ns))
            (if buffer (list "buffer" buffer))
            (if path (list "path" path))
            (if accent (list "accent" accent))
            (list
             "op" "eval"
             "session" (or session (nrepl-current-session))
             "code" input))))

(defun accent/cider-send-load-file (file-contents file-path file-name)
  "Perform the nREPL \"load-file\" op.
FILE-CONTENTS, FILE-PATH and FILE-NAME are details of the file to be
loaded."
  (let* ((accent (plist-get buffer-meta 'filetype))
         (buffer (current-buffer)))
    (nrepl-send-request
     (append (if accent (list "accent" accent))
             (list "op" "load-file"
                   "session" (nrepl-current-session)
                   "file" file-contents
                   "file-path" file-path
                   "file-name" file-name))
     (cider-load-file-handler buffer))))

(defun accent/cider-load-file-buffer (&optional p)
  (interactive "P")
  (accent/cider-send-load-file (buffer-substring-no-properties (point-min)
                                                               (point-max))
                               (plist-get buffer-meta 'filename)
                               (plist-get buffer-meta 'name)))

(defun accent/nrepl-send-string (input callback &optional ns session)
  "Send the request INPUT and register the CALLBACK as the response handler.
See command `nrepl-eval-request' for details on how NS and SESSION are processed."
  (let ((ns (if (string-match "[[:space:]]*\(ns\\([[:space:]]*$\\|[[:space:]]+\\)" input)
                "user"
              ns)))
    (nrepl-send-request (accent/nrepl-eval-request input ns session) callback)))

(eval-after-load "nrepl-client"
  '(defun nrepl-send-string (input callback &optional ns session)
     "Send the request INPUT and register the CALLBACK as the response
handler.  See command `nrepl-eval-request' for details on how NS and
SESSION are processed."
     (accent/nrepl-send-string input callback ns session)))

(eval-after-load "cider-interaction"
  '(defun cider-send-load-file (file-contents file-path file-name)
     "Perform the nREPL \"load-file\" op.
FILE-CONTENTS, FILE-PATH and FILE-NAME are details of the file to be
loaded."
     (accent/cider-send-load-file file-contents file-path file-name)))

(defun accent/set-buffer-meta ()
  (let ((file (buffer-name)))
    (setq buffer-meta (plist-put buffer-meta 'name file))
    (if (string-match "\\*cider.*\\*" file)
      (setq buffer-meta (plist-put buffer-meta 'filetype "repl"))
      (let* ((path (buffer-file-name))
             (coll (split-string (clojure-expected-ns) "\\."))
             (ns (string/join "." (if (and (string-match "^clj" (car coll))
                                           (string-match "src/clj" path))
                                      (cdr coll)
                                    coll))))
        (setq buffer-meta (plist-put buffer-meta 'name file))
        (setq buffer-meta (plist-put buffer-meta 'filename path))
        (setq buffer-meta (plist-put buffer-meta 'ns ns))
        (cond ((string-match "\.cljs$" file)
               (setq buffer-meta (plist-put buffer-meta 'filetype "cljs")))
              ((string-match "\.clj$" file)
               (setq buffer-meta (plist-put buffer-meta 'filetype "clj")))
              ((string-match "\.cljx$" file)
               (setq buffer-meta (plist-put buffer-meta 'filetype "cljx"))))))))

(defun accent/evil-leader-keys ()
  (evil-leader/set-key "ns" 'cider-set-ns
    "ef" 'accent/cider-load-file-buffer
    "ee" 'cider-eval-expression-at-point
    "gd" 'cider-jump))

(defun accent/set-repl-accent (accent)
  (setq buffer-meta (plist-put buffer-meta 'filetype accent)))

(defun accent/set-repl-accent-cljs ()
  (interactive)
  (accent/set-repl-accent "cljs"))

(defun accent/set-repl-accent-clj ()
  (interactive)
  (accent/set-repl-accent "clj"))

(defun accent/load-cljs-env ()
  (interactive)
  (insert "(def repl-env (reset! cemerick.austin.repls/browser-repl-env
                      (cemerick.austin/repl-env)))")
  (cider-return))

(defun accent/load-cljs-repl ()
  (interactive)
  (accent/load-cljs-env)
  (insert "(cemerick.austin.repls/cljs-repl repl-env)")
  (cider-return))

(define-minor-mode clojure-accents-mode
  "Clj/Cljs/Cljx interaction and co-development."
  :lighter " clj^"
  (make-local-variable 'buffer-meta)
  (accent/set-buffer-meta)
  (accent/evil-leader-keys))

(defun evil-pparedit-mode ()
  (paredit-mode)
  (evil-paredit-mode))

(defun cider-mode-setup ()
  (ac-nrepl-setup)
  (evil-pparedit-mode))

(add-hook 'clojure-mode-hook 'clojure-accents-mode)
(add-hook 'cider-repl-mode-hook 'clojure-accents-mode)

(provide 'clojure-accents)
