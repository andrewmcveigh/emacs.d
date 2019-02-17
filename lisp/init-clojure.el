(require-packages 'clojure-mode 'cider 'paren-face 'clj-refactor)

(require 'clojure-mode)
(require 'cider)
; (require 'cider-interaction)
; (require 'cider-repl)
(require 'clj-refactor)

; (require 'nrepl-client)

;;; Settings
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-hide-special-buffers nil)
(setq cider-show-error-buffer nil)
(setq cider-auto-jump-to-error nil)
(setq cider-auto-select-error-buffer nil)
(setq cider-stacktrace-fill-column 80)
(setq cider-words-of-inspiration '("Fuck cider"))
(setq cider-repl-display-help-banner nil)
(setq cljr-favor-prefix-notation nil)
(setq cljr-favor-private-functions nil)
(setq cljr-warn-on-eval nil)

; (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
; (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

(define-clojure-indent (s/fdef (quote defun)))
(define-clojure-indent (mlet 1))
(define-clojure-indent (pcase 1))
(define-clojure-indent (instance '(2 1)))
(define-clojure-indent (defui '(1 1)))
(define-clojure-indent (class 1))
(define-clojure-indent (match '(:defn)))

;;; Helper Functions
(defun cljr-setup ()
   (clj-refactor-mode 1)
  ;; insert keybinding setup here
  )

(defun jump-to-var ()
  (interactive)
  (cider--find-var (cider-symbol-at-point)))

(defun doc-for-var ()
  (interactive)
  (cider-doc-lookup (cider-symbol-at-point)))

(defun cider-eval-pprint-handler (&optional buffer)
  "Make a handler for evaluating and printing result in BUFFER."
  (nrepl-make-response-handler (or buffer (current-buffer))
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (let ((s (replace-regexp-in-string
                                              "\\\\\"" "\"" (replace-regexp-in-string "\\\\n" "\n" value))))
                                     (insert (substring (substring s 1) 0 -1)))))
                               (lambda (_buffer out)
                                 (cider-emit-interactive-eval-output out))
                               (lambda (_buffer err)
                                 (cider-emit-interactive-eval-err-output err))
                               '()))

(defun cider-eval-last-sexp-and-pprint ()
  "Evaluate the expression preceding point and replace it with its pprinted result."
  (interactive)
  (let ((last-sexp (format "(with-out-str (clojure.pprint/pprint %s))" (cider-last-sexp))))
    ;; we have to be sure the evaluation won't result in an error
    (cider-nrepl-sync-request:eval last-sexp)
    ;; seems like the sexp is valid, so we can safely kill it
    (backward-kill-sexp)
    (cider-interactive-eval last-sexp (cider-eval-pprint-handler))))

(defun reset-namespace ()
  "Clear all mappings and aliases from the current (buffer's) namespace"
  (interactive)
  (let ((sexp "(do
                 (.importClass *ns* java.lang.Class)
                 (let [imports (.getDeclaredField clojure.lang.RT \"DEFAULT_IMPORTS\")
                       _ (.setAccessible imports true)
                       default-imports (vals (.get imports clojure.lang.RT))]
                   (doseq [sym (keys (ns-map *ns*))]
                     (ns-unmap *ns* sym)
                     (clojure.core/require '[clojure.core :refer :all]))
                   (doseq [sym (keys (ns-aliases *ns*))]
                     (ns-unalias *ns* sym))
                   (.importClass *ns* java.lang.Class)
                   (doseq [cls default-imports]
                     (.importClass *ns* cls))))"))
    (cider-interactive-eval sexp)))

(defun user/reset ()
  "(do (load-file \"dev/user.clj\") (user/reset))"
  (interactive)
  (let ((sexp "(do (load-file \"dev/user.clj\") (user/reset))"))
    (cider-interactive-eval sexp)))

(defun user/run-spec-tests-in-current-ns ()
  "(do (load-file \"dev/user.clj\") (user/reset))"
  (interactive)
  (let ((sexp "(do (load-file \"dev/user.clj\") (user/run-spec-tests-in-current-ns))"))
    (cider-interactive-eval sexp)))

(defun type-check-cider-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (expr (cider-last-sexp))
         (wrap "(if (try (require 'tools) true (catch Throwable _))
                  ((resolve 'tools/check-eval) '%s)
                  %s)"))
    (cider-interactive-eval (format wrap expr expr) nil bounds)))

(defun prep-current-expr ()
  (interactive)
  (let* ((bounds     (cider-sexp-at-point 'bounds))
         (expr       (cider-sexp-at-point))
         (top-bounds (cider-defun-at-point 'bounds))
         (top-expr   (cider-defun-at-point))
         (file       (buffer-file-name))
         (exprm      (format "{:start %s :end %s :expr %s}"
                             (car bounds)
                             (cadr bounds)
                             (prin1-to-string expr)))
         (top-exprm  (format "{:start %s :end %s :expr %s}"
                             (car top-bounds)
                             (cadr top-bounds)
                             (prin1-to-string top-expr)))
         (arg        (format "{:file \"%s\" :expr %s :top-level %s}"
                             file exprm top-exprm)))
    [bounds arg]))

(defun type-of-current-expr ()
  (interactive)
  (let* ((bounds     (cider-sexp-at-point 'bounds))
         (expr       (cider-sexp-at-point))
         (top-bounds (cider-defun-at-point 'bounds))
         (top-expr   (cider-defun-at-point))
         (file       (buffer-file-name))
         (exprm      (format "{:start %s :end %s :expr %s}"
                             (car bounds)
                             (cadr bounds)
                             (prin1-to-string expr)))
         (top-exprm  (format "{:start %s :end %s :expr %s}"
                             (car top-bounds)
                             (cadr top-bounds)
                             (prin1-to-string top-expr)))
         (arg        (format "{:file \"%s\" :expr %s :top-level %s}"
                             file exprm top-exprm))
         (wrap   "(lift.middleware/t '%s)"))
    (cider-interactive-eval (format wrap arg) nil bounds)))

(defun type-search-current-expr ()
  (interactive)
  (let* ((bounds     (cider-sexp-at-point 'bounds))
         (expr       (cider-sexp-at-point))
         (top-bounds (cider-defun-at-point 'bounds))
         (top-expr   (cider-defun-at-point))
         (file       (buffer-file-name))
         (exprm      (format "{:start %s :end %s :expr %s}"
                             (car bounds)
                             (cadr bounds)
                             (prin1-to-string expr)))
         (top-exprm  (format "{:start %s :end %s :expr %s}"
                             (car top-bounds)
                             (cadr top-bounds)
                             (prin1-to-string top-expr)))
         (arg        (format "{:file \"%s\" :expr %s :top-level %s}"
                             file exprm top-exprm))
         (wrap   "(do (require 'type) (tools/type-search (tools/t '%s)))"))
    (cider-interactive-eval (format wrap arg) nil bounds)))

(defun type-search-fill-current-expr ()
  (interactive)
  (let* ((bounds     (cider-sexp-at-point 'bounds))
         (expr       (cider-sexp-at-point))
         (top-bounds (cider-defun-at-point 'bounds))
         (top-expr   (cider-defun-at-point))
         (file       (buffer-file-name))
         (exprm      (format "{:start %s :end %s :expr %s}"
                             (car bounds)
                             (cadr bounds)
                             (prin1-to-string expr)))
         (top-exprm  (format "{:start %s :end %s :expr %s}"
                             (car top-bounds)
                             (cadr top-bounds)
                             (prin1-to-string top-expr)))
         (arg        (format "{:file \"%s\" :expr %s :top-level %s}"
                             file exprm top-exprm))
         (wrap   "(do (require 'type) (let [x (first (tools/type-search (tools/t '%s)))] (with-out-str (pr x))))"))
    (kill-sexp)
    (cider-interactive-eval (format wrap arg) (cider-eval-pprint-handler) bounds)))

(defun nrepl--raw-request (op input &optional ns line column)
  (nconc (and ns `("ns" ,ns))
         `("op" op "code" ,(substring-no-properties input))
         (when cider-enlighten-mode
           '("enlighten" "true"))
         (let ((file (or (buffer-file-name) (buffer-name))))
           (when (and line column file)
             `("file" ,file
               "line" ,line
               "column" ,column)))))

(defun nrepl-request:raw (op input callback connection &optional ns line column additional-params tooling)
  (nrepl-send-request
   (append (nrepl--raw-request op input ns line column) additional-params)
   callback
   connection
   tooling))

(defun lift-bounds-pos (bounds)
  (let* ((start (car-safe bounds))
         (end   (car-safe (cdr-safe bounds)))
         (line  (when start (line-number-at-pos start)))
         (col   (when start (cider-column-number-at-pos start))))
    (list line col start end)))

(defun positional-op (op)
  (let* ((top-pos  (lift-bounds-pos (cider-defun-at-point 'bounds)))
         (expr-pos (lift-bounds-pos (cider-sexp-at-point 'bounds)))
         (top      (cider-defun-at-point))
         (expr     (cider-sexp-at-point))
         (file     (buffer-file-name)))
    (format "{:lift.middleware/op %s
              :file \"%s\"
              :top  {:pos %s :code %s}
              :expr {:pos %s :code %s}}"
            op
            file
            top-pos
            (prin1-to-string top)
            expr-pos
            expr)))

(defun toggle-type-checking ()
  (interactive)
  (cider-interactive-eval
   (positional-op "toggle-type-checker")
   nil
   (cider-defun-at-point 'bounds)))

(defun type-of-expr-at-point ()
  (interactive)
  (cider-interactive-eval
   (positional-op "type-of-expr-at-point")
   nil
   (cider-defun-at-point 'bounds)))

(defun type-search-expr-at-point ()
  (interactive)
  (cider-interactive-eval
   (positional-op "type-search-expr-at-point")
   nil
   (cider-defun-at-point 'bounds)))

(defun type-replace-expr-at-point ()
  (interactive)
  (let ((pos (positional-op "type-replace-expr-at-point")))
    (kill-sexp)
    (cider-interactive-eval
     pos
     (cider-eval-print-handler)
     (cider-defun-at-point 'bounds))))


(defun lift-request:load-file
    (ns file-contents file-path file-name &optional connection callback)
  (cider-nrepl-send-request `("op" "load-file"
                              "ns" ,ns
                              "file" ,file-contents
                              "file-path" ,file-path
                              "file-name" ,file-name)
                            (or callback
                                (cider-load-file-handler (current-buffer)))
                            connection))

(defun lift-load-buffer ()
  (interactive)
  (check-parens)
  (cider-ensure-connected)
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (unless buffer-file-name
        (user-error "Buffer `%s' is not associated with a file" (current-buffer)))
      (when (and cider-save-file-on-load
                 (buffer-modified-p)
                 (or (eq cider-save-file-on-load t)
                     (y-or-n-p (format "Save file %s? " buffer-file-name))))
        (save-buffer))
      (remove-overlays nil nil 'cider-temporary t)
      (cider--clear-compilation-highlights)
      (cider--quit-error-window)
      (let ((filename (buffer-file-name buffer))
            (ns-form  (cider-ns-form)))
        (cider-map-connections
         (lambda (connection)
           (when ns-form
             (cider-repl--cache-ns-form ns-form connection))
           (lift-request:load-file
            (cider-current-ns)
            (cider-file-string filename)
            (funcall cider-to-nrepl-filename-function
                     (cider--server-filename filename))
            (file-name-nondirectory filename)
            connection))
         :both)
        (message "Loading %s..." filename)))))

(defun cider-eval-last-sexp (&optional output-to-current-buffer)
  "Evaluate the expression preceding point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current buffer."
  (interactive "P")
  (cider-interactive-eval nil
                          (when output-to-current-buffer (cider-eval-print-handler))
                          (cider-last-sexp 'bounds)
                          `("file-name" ,(buffer-file-name)
                            "expr-pos"  ,(lift-bounds-pos (cider-last-sexp 'bounds)))))

(defun cider-eval-defun-at-point (&optional debug-it)
  "Evaluate the current toplevel form, and print result in the minibuffer.
With DEBUG-IT prefix argument, also debug the entire form as with the
command `cider-debug-defun-at-point'."
  (interactive "P")
  (let ((inline-debug (eq 16 (car-safe debug-it))))
    (when debug-it
      (when (derived-mode-p 'clojurescript-mode)
        (when (y-or-n-p (concat "The debugger doesn't support ClojureScript yet, and we need help with that."
                                "  \nWould you like to read the Feature Request?"))
          (browse-url "https://github.com/clojure-emacs/cider/issues/1416"))
        (user-error "The debugger does not support ClojureScript"))
      (when inline-debug
        (cider--prompt-and-insert-inline-dbg)))
    (cider-interactive-eval (when (and debug-it (not inline-debug))
                              (concat "#dbg\n" (cider-defun-at-point)))
                            ;; (when output-to-current-buffer (cider-eval-print-handler))
                            nil
                            (cider-defun-at-point 'bounds)
                            `("file-name" ,(buffer-file-name)
                              "expr-pos"  ,(lift-bounds-pos (cider-defun-at-point 'bounds))))))

;;; Keybindings
(defun set-keys (mode)
  (evil-leader/set-key-for-mode mode
    "ns" 'cider-repl-set-ns
    "ef" 'cider-eval-buffer
    "ed" 'cider-eval-defun-at-point
    ;; "ee" 'type-check-cider-eval-last-sexp
    "ee" 'cider-eval-last-sexp
    "tt" 'toggle-type-checking
    "te" 'type-of-expr-at-point
    "ts" 'type-search-expr-at-point
    "tr" 'type-replace-expr-at-point
    "er" 'cider-eval-last-sexp-and-pprint
    "eme" 'cider-macroexpand-1
    "ema" 'cider-macroexpand-all
    "eur" 'user/reset
    "eus" 'user/run-spec-tests-in-current-ns
    "je" 'cider-jump-to-compilation-error
    "jb" 'cider-visit-error-buffer
    "cj" 'cider-jack-in
    "cq" 'cider-quit
    "cr" 'cider-connect
    "cz" 'cider-switch-to-relevant-repl-buffer
    "ccb" 'cider-repl-clear-buffer
    "fu" 'cljr-find-usages
    "fg" 'rgrep
    "rtf" 'cljr-thread-first-all
    "rtl" 'cljr-thread-last-all
    "ref" 'cljr-extract-function
    "rns" 'reset-namespace
    "===" 'clojure-align
    (kbd "RET") 'cider-repl-return))

(set-keys 'clojure-mode)
(set-keys 'cider-repl-mode)
(set-keys 'org-mode)

(evil-define-key 'normal clojure-mode-map (kbd "gf") 'cider-find-var)
(evil-define-key 'normal clojure-mode-map (kbd "K")  'doc-for-var)
(define-key evil-motion-state-map "gd" 'evil-goto-definition)

(defun math-prettify-symbols ()
  (let ((syms '(("_Gamma"   . ?Γ)
                ("_Delta"   . ?Δ)
                ("_Rho"     . ?Ρ)
                ("_Theta"   . ?Θ)
                ("_Forall"  . ?∀)
                ("forall"   . ?∀)
                ("_epsilon" . ?ε)
                ("_kind"    . ?✳))))
    (dolist (s syms)
      (push s prettify-symbols-alist))))

;;; Hooks
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'evil-mode)
(add-hook 'cider-repl-mode-hook 'evil-paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'cljr-setup)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)
(add-hook 'clojure-mode-hook 'paren-face-mode)
(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'clojure-mode-hook 'cljr-setup)
(add-hook 'clojure-mode-hook 'math-prettify-symbols)
(add-hook 'org-mode-hook
          (progn
            (define-key paredit-mode-map (kbd "RET") 'newline-and-indent)
            (paredit-mode)
            (evil-paredit-mode)
            (cljr-setup)
            (paren-face-mode)))

(provide 'init-clojure)
