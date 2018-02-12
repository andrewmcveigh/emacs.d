(require-packages 'clojure-mode 'cider 'clj-refactor 'paren-face)
(require 'clojure-mode)
(require 'cider)
(require 'cider-interaction)
(require 'cider-repl)
;; (require 'clj-refactor)
(require 'nrepl-client)

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

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

(define-clojure-indent (s/fdef (quote defun)))
(define-clojure-indent (mlet 1))
(define-clojure-indent (pcase 1))
(define-clojure-indent (instance '(2 1)))
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
         (wrap   "(do (require 'type) (tools/t '%s))"))
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

(defun toggle-type-checking ()
  (interactive)
  (cider-interactive-eval "(tools/toggle)"))

;;; Keybindings
(defun set-keys (mode)
  (evil-leader/set-key-for-mode mode
    "ns" 'cider-repl-set-ns
    "ef" 'cider-load-buffer
    "ed" 'cider-eval-defun-at-point
    ;; "ee" 'type-check-cider-eval-last-sexp
    "ee" 'cider-eval-last-sexp
    "tt" 'toggle-type-checking
    "te" 'type-of-current-expr
    "ts" 'type-search-current-expr
    "tf" 'type-search-fill-current-expr
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

(evil-define-key 'normal clojure-mode-map (kbd "gf") 'jump-to-var)
(evil-define-key 'normal clojure-mode-map (kbd "K")  'doc-for-var)
(define-key evil-motion-state-map "gd" 'evil-goto-definition)

(defun math-prettify-symbols ()
  (let ((syms '(("_Gamma"   . ?Γ)
                ("_Delta"   . ?Δ)
                ("_Rho"     . ?Ρ)
                ("_Theta"   . ?Θ)
                ("_Forall"  . ?∀)
                ("_epsilon" . ?ε))))
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
