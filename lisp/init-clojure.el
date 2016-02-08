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
(setq cider-repl-popup-stacktraces t)
(setq cider-hide-special-buffers t)
(setq cider-popup-stacktraces nil)
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

(setq cljr-favor-prefix-notation nil)

(put 'defcomponent 'clojure-backtracking-indent '(4 4 (2)))
(put 'task-fn 'clojure-backtracking-indent '((2) 2))

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

;;; Keybindings
(defun evil-clojure-leader-keys ()
  (evil-leader/set-key
    "ns" 'cider-repl-set-ns
    "ef" 'cider-load-buffer
    "ed" 'cider-eval-defun-at-point
    "ee" 'cider-eval-last-sexp
    "er" 'cider-eval-last-sexp-and-replace
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
    "ref" 'cljr-extract-function))

(defun evil-clojure-keymapping ()
  (define-key evil-normal-state-map "gf" 'jump-to-var)
  (define-key evil-normal-state-map (kbd "K") 'doc-for-var))

;;; Modes
(define-minor-mode evil-clojure-mode
  "Evil Clojure*"
  :lighter " cl&"
  (progn
    (evil-clojure-leader-keys)
    (evil-clojure-keymapping)))

;;; Hooks
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'evil-mode)
(add-hook 'cider-repl-mode-hook 'evil-clojure-mode)
(add-hook 'cider-repl-mode-hook 'evil-paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'evil-clojure-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)
(add-hook 'clojure-mode-hook 'cljr-setup)
(add-hook 'clojure-mode-hook 'yas-setup)
(add-hook 'clojure-mode-hook 'paren-face-mode)
(add-hook 'clojure-mode-hook 'hs-minor-mode)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-repl-mode))

;;; Library Functions
(defun cljs-node-repl ()
  (interactive)
  (run-clojure "lein trampoline run -m clojure.main repl.clj"))

(provide 'init-clojure)
