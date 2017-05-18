(require 'clojure-mode)
(require 'cider)
(require 'cider-interaction)
(require 'cider-repl)
(require 'nrepl-client)
(require 'evil)
(require 'evil-leader)

;;; Settings
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-hide-special-buffers nil)
;; (setq cider-popup-stacktraces nil)
;; (setq cider-repl-popup-stacktraces t)
(setq cider-show-error-buffer nil)
(setq cider-auto-select-error-buffer nil)
(setq cider-stacktrace-fill-column 80)
;; (setq cider-auto-jump-to-error t)

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

(setq cljr-favor-prefix-notation nil)

(put 'defcomponent 'clojure-backtracking-indent '(4 4 (2)))
(put 'task-fn 'clojure-backtracking-indent '((2) 2))

(define-clojure-indent (s/fdef (quote defun)))
(define-clojure-indent (mlet 1))
(define-clojure-indent (pcase 1))
(define-clojure-indent (instance '(2 1)))
(define-clojure-indent (class 1))

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

(defun yas-setup ()
  (yas/minor-mode 1))

(defun cider-auto-connect ()
  (interactive)
  (let ((local-p (cadr (car (cider-locate-running-nrepl-ports nil)))))
    (cider-connect "localhost" local-p)))

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


;;; Keybindings
(defun evil-clojure-leader-keys ()
  (evil-leader/set-key
    "ns" 'cider-repl-set-ns
    "ef" 'cider-load-buffer
    "ed" 'cider-eval-defun-at-point
    "ee" 'cider-eval-last-sexp
    "er" 'cider-eval-last-sexp-and-pprint
    "eme" 'cider-macroexpand-1
    "ema" 'cider-macroexpand-all
    "je" 'cider-jump-to-compilation-error
    "jb" 'cider-visit-error-buffer
    "cc" 'cider-auto-connect
    "cj" 'cider-jack-in
    "cq" 'cider-quit
    "cr" 'cider-connect
    "cz" 'cider-switch-to-relevant-repl-buffer
    "fu" 'cljr-find-usages
    "fg" 'rgrep
    "rtf" 'cljr-thread-first-all
    "rtl" 'cljr-thread-last-all
    "ref" 'cljr-extract-function
    "===" 'clojure-align
    (kbd "RET") 'cider-repl-return
    ))

(defvar load-command
  "(load-file \"%s\")\n")

(defun load-file (file-name)
  "Load a Clojure file FILE-NAME into the inferior Clojure process."
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq inf-clojure-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                            (file-name-nondirectory file-name)))
  (comint-send-string (inf-clojure-proc) (format load-command file-name)))

(defun root-dir ()
  (or (projectile-project-root)
      (let ((backend (vc-deduce-backend)))
        (and backend
             (ignore-errors
               (vc-call-backend backend 'root default-directory))))))

(defun load-current-buffer ()
  (interactive)
  (save-buffer)
  (when (not (inf-clojure-connected-p))
    (let ((b (window-buffer (minibuffer-selected-window))))
      (run-clojure inf-clojure-program)
      (pop-to-buffer-same-window b)))
  (let ((f (buffer-file-name (window-buffer (minibuffer-selected-window))))
        (root (root-dir)))
    (if root (load-file (s-replace (expand-file-name root) "" f))
      (message "Not in VC dir, cannot infer project root"))))


(defun evil-clojure-keymapping ()
  (define-key evil-normal-state-map "gf" 'jump-to-var)
  (define-key evil-normal-state-map (kbd "K") 'doc-for-var))


;;; Modes
(define-minor-mode evil-clojure-mode
  "Evil Clojure*"
  :lighter " cl&"
  (progn
    (clojure-mode)
    (evil-clojure-leader-keys)
    (evil-clojure-keymapping)
    (cljr-setup)
    (yas-setup)))

(define-minor-mode evil-clojurescript-mode
  "Evil Clojure*"
  :lighter " cljs&"
  (progn
    (clojurescript-mode)
    (evil-clojure-leader-keys)
    (evil-clojure-keymapping)
    ;; (cljr-setup)
    ;; (yas-setup)
    ))


(add-to-list 'auto-mode-alist '("\\.clj\\'" . evil-clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . evil-clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . evil-clojure-mode))

;;; Hooks
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'evil-mode)
(add-hook 'cider-repl-mode-hook 'evil-clojure-mode)
(add-hook 'cider-repl-mode-hook 'evil-paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)
(add-hook 'clojure-mode-hook 'paren-face-mode)
(add-hook 'clojure-mode-hook 'hs-minor-mode)

(provide 'init-clojure)
