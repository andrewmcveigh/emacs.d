
;; Basics

;; Emacs 24 or higher!
(when (< emacs-major-version 24)
  (error "This setup requires Emacs v24, or higher. You have: v%d" emacs-major-version))

;; Configure package manager
(require 'package)

;; Add Marmalade repo
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ... and melpa. Melpa packages that exist on marmalade will have
;; precendence.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; And load things!
;; (package-refresh-contents) ;; Uncomment if refresh needed E.G., on first use.
(package-initialize)

;;; Set fullscreen 27"
(setq default-frame-alist '((width . 421) (height . 99)))

(defun left-vertial-frame ()
  (interactive)
  (new-frame)
  (set-frame-size (selected-frame) 148 135)
  (set-frame-position (selected-frame) -1078 -63))

(defun right-vertial-frame ()
  (interactive)
  (new-frame)
  (set-frame-size (selected-frame) 148 135)
  (set-frame-position (selected-frame) 2562 -63))

;;; display line numbers in margin, col nums at bottom.
(global-linum-mode 1)
(column-number-mode 1)

(defvar my-packages
  '(
    paredit
    evil
    evil-leader
    evil-paredit
    solarized-theme
    ;; Clojure
    ;clojure-cheatsheet
    ac-nrepl
    cider
    clojure-mode
    project-explorer
    )
  "My packages to install.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(require 'paredit)
(require 'evil)
(require 'evil-leader)
(require 'evil-paredit)
(require 'clojure-mode)
(require 'project-explorer)
(require 'ac-nrepl)

;; Use auto-complete as completion at point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)


(evil-mode t)
(global-evil-leader-mode)
(load-theme 'solarized-dark t)

(defun evil-pparedit-mode ()
  (paredit-mode)
  (evil-paredit-mode))

(defun cider-mode-setup ()
  (ac-nrepl-setup)
  (evil-pparedit-mode))

;;; paredit init in lisp(s)
;(add-hook 'clojure-mode-hook 'evil-pparedit-mode)
(add-hook 'emacs-lisp-mode-hook 'evil-pparedit-mode)

;;; Configure CIDER
(add-hook 'cider-repl-mode-hook 'cider-mode-setup)
(add-hook 'clojure-mode-hook 'cider-mode-setup)

;(add-hook 'cider-interaction-mode-hook 'cider-mode-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; Paredit in clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

;; eldoc in clojure
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)

(global-auto-complete-mode)


;; Don't annoy me
;(setq cider-hide-special-buffers t)
;(setq cider-popup-stacktraces nil)
;(setq cider-repl-pop-to-buffer-on-connect nil)
;(setq cider-repl-popup-stacktraces t)

;;; paredit customisations
(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;;; paredit key mappings
(define-key paredit-mode-map (kbd "RET") 'electrify-return-if-match)

;;; normal mode
(define-key evil-normal-state-map (kbd "C-\<") 'paredit-backward-slurp-sexp)
(define-key evil-normal-state-map (kbd "C-\>") 'paredit-forward-slurp-sexp)

(define-key evil-normal-state-map (kbd "C-u") (lambda ()
                                                (interactive)
                                                (evil-scroll-up nil)
                                                (evil-scroll-line-to-center
                                                 (line-number-at-pos))))

(define-key evil-normal-state-map (kbd "C-d") (lambda ()
                                                (interactive)
                                                (evil-scroll-down nil)
                                                (evil-scroll-line-to-center
                                                 (line-number-at-pos))))

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(define-key evil-normal-state-map (kbd "<") 'paredit-backward-barf-sexp)
(define-key evil-normal-state-map (kbd ">") 'paredit-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "S") 'paredit-splice-sexp)
(define-key evil-normal-state-map (kbd "W") 'paredit-wrap-round)

;;; Backspace jump % normal
(define-key evil-normal-state-map (kbd "\d") 'evil-jump-item)

(define-key evil-normal-state-map (kbd "K") 'ac-nrepl-popup-doc)

(define-key evil-normal-state-map (kbd "<left>") 'evil-window-left)
(define-key evil-normal-state-map (kbd "<right>") 'evil-window-right)
(define-key evil-normal-state-map (kbd "<up>") 'evil-window-up)
(define-key evil-normal-state-map (kbd "<down>") 'evil-window-down)

;;; motions
;;; Backspace jump % motion
(define-key evil-motion-state-map (kbd "\d") 'evil-jump-item)

;;; visual mode
(define-key evil-visual-state-map (kbd "W") 'paredit-wrap-round)

;;; general mappings
;(define-key cider-repl-mode-map (kbd "<up>") 'cider-backward-input)
;(define-key cider-repl-mode-map (kbd "<down>") 'cider-forward-input)
;(define-key cider-repl-mode-map (kbd "C-<up>") 'previous-line)
;(define-key cider-repl-mode-map (kbd "C-<down>") 'next-line)

;;; evil leader mappings
(evil-leader/set-leader ",")
(evil-leader/set-key
  "nt" 'project-explorer-open
  "w[" 'paredit-wrap-square
  "w{" 'paredit-wrap-curly
  "w\"" (lambda ()
          (interactive)
          (paredit-doublequote)
          (paredit-forward-slurp-sexp))
  "cp" 'comment-or-uncomment-region
  "cl" (lambda ()
         (interactive)
         (comment-or-uncomment-region (line-beginning-position)
                                      (line-end-position))))

(defvar paren-face 'paren-face)

(defface paren-face
    '((((class color))
       (:foreground "DimGray")))
  "Face for displaying a paren."
  :group 'faces)

(defmacro paren-face-add-support (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
    (let* ((regexp "(\\|)")
           (match (assoc regexp ,keywords)))
      (unless (eq (cdr match) paren-face)
        (setq ,keywords (append (list (cons regexp paren-face)) ,keywords))))))

(defun paren-face-add-keyword ()
  "Adds paren-face support to the mode."
  (font-lock-add-keywords nil '(("(\\|)" . paren-face))))

;; Keep the compiler quiet.
(eval-when-compile
  (defvar clojure-font-lock-keywords nil)
  (defvar jess-font-lock-keywords)
  (defvar lisp-font-lock-keywords-2 nil)
  (defvar scheme-font-lock-keywords-2 nil))

(add-hook 'clojure-mode-hook (paren-face-add-support clojure-font-lock-keywords))
(add-hook 'emacs-lisp-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'ielm-mode-hook 'paren-face-add-keyword)
(add-hook 'inferior-jess-mode-hook 'paren-face-add-keyword)
(add-hook 'jess-mode-hook (paren-face-add-support jess-font-lock-keywords))
(add-hook 'lisp-interaction-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'lisp-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'nrepl-mode-hook 'paren-face-add-keyword)
(add-hook 'scheme-mode-hook (paren-face-add-support scheme-font-lock-keywords-2))
(add-hook 'slime-repl-mode-hook 'paren-face-add-keyword)

;; (eval-after-load 'project-explorer
;;   '(progn
;;      (defvar project-explorer-mode-map)
;;      (evil-make-overriding-map project-explorer-mode-map 'normal t)
;;      (evil-define-key 'normal project-explorer-mode-map
;;        "o" 'pe/return
;;        "v" (lambda ()
;;              (interactive)
;;              (setq w (next-window))
;;              (split-window w nil t)
;;              (pe/return))
;;        "s" (lambda ()
;;              (interactive)
;;              (setq w (next-window))
;;              (split-window w nil)
;;              (pe/return)))))

(add-to-list 'load-path user-emacs-directory)
(require 'nerdtree-project-explorer)
(require 'clojure-accents)
;; (require 'longlines)

(global-whitespace-mode +1)

;;; Set font
(set-face-attribute 'default nil :font "Monaco")

;;; Resize windows
(global-set-key (kbd "s-\<") 'evil-window-decrease-width)
(global-set-key (kbd "s-\>") 'evil-window-increase-width)

;;; Close with CMD-w
(global-set-key (kbd "s-w") 'delete-window)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-bar-mode nil)
(scroll-bar-mode -1)

;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;(setq inferior-lisp-program "ccl64")

;(setq inferior-lisp-program "sbcl")

;(setq slime-default-lisp 'ccl)

;(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

(setq whitespace-empty nil)

