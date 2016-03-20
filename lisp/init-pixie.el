(require 'clojure-mode)
(require 'inf-clojure)
(require 'evil)
(require 'evil-leader)
(require 'vc)
(require 's)

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defvar pixie-inf-lisp-program "pixie-vm")

(defvar pixie-inf-lisp-load-command "(load-file \"%s\")\n")

(defvar pixie-inf-lisp-doc-command "(pixie.stdlib/doc \"%s\")\n")

(defvar pixie-inf-lisp-ns-vars-command "(pixie.stdlib/doc-ns \"%s\")\n")

(defvar pixie-inf-lisp-set-ns-command "(pixie.stdlib/in-ns \"%s\")\n")

(defvar pixie-inf-lisp-macroexpand-command
  "(pixie.stdlib/macroexpand-1 \"%s\")\n")

(defvar pixie-inf-lisp-project-root-files
  '("project.edn")
  "A list of files that can be considered project markers.")

;;;###autoload
(define-derived-mode pixie-mode clojure-mode "Pixie"
  "Major mode for editing Pixie code.
\\{pixie-mode-map}"
  (setq-local inf-clojure-load-command pixie-inf-lisp-load-command)
  (setq-local inf-clojure-var-doc-command pixie-inf-lisp-doc-command)
  (setq-local inf-clojure-ns-vars-command pixie-inf-lisp-ns-vars-command)
  (setq-local inf-clojure-set-ns-command pixie-inf-lisp-set-ns-command)
  (setq-local inf-clojure-macroexpand-command
              pixie-inf-lisp-macroexpand-command)
  (setq-local inf-clojure-program pixie-inf-lisp-program)
  (setq-local inf-clojure-project-root-files pixie-inf-lisp-project-root-files))

;;; Helper Functions

;;; Keybindings

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

(defun evil-pixie-leader-keys ()
  (evil-leader/set-key
    "ns" 'inf-clojure-set-ns
    "cz" 'inf-clojure-switch-to-repl
    "eb" 'inf-clojure-eval-buffer
    "ef" 'load-current-buffer
    "ee" 'inf-clojure-eval-defun
    "ee" 'inf-clojure-eval-last-sexp
    "en" 'inf-clojure-eval-form-and-next
    "fg" 'rgrep
    "===" 'clojure-align))

(defun evil-pixie-keymapping ()
  (define-key
    evil-normal-state-map
    (kbd "K")
    'inf-clojure-show-var-documentation))

;;; Modes
(define-minor-mode evil-pixie-mode
  "Evil Pixie*"
  :lighter " px&"
  (progn
    (message "Evil Pixie*")
    (pixie-mode)
    (inf-clojure-minor-mode)
    (company-mode 0)
    (evil-pixie-leader-keys)
    (evil-pixie-keymapping)))

(add-to-list 'auto-mode-alist '("\\.pxi\\'" . evil-pixie-mode))

;;; Hooks
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)
(add-hook 'clojure-mode-hook 'paren-face-mode)
(add-hook 'clojure-mode-hook 'hs-minor-mode)

(add-hook 'inf-clojure-mode-hook 'evil-mode)
(add-hook 'inf-clojure-mode-hook (lambda () (company-mode 0)))
(add-hook 'inf-clojure-mode-hook 'evil-paredit-mode)
(add-hook 'inf-clojure-mode-hook 'paredit-mode)

(provide 'init-pixie)
