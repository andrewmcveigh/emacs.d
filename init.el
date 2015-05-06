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
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(markdown-mode . "melpa-stable") t)

;; And load things!
 ;; (package-refresh-contents) ;; Uncomment if refresh needed E.G., on first use.
(package-initialize)

(defvar my-packages
  '(
    paredit
    evil
    evil-leader
    evil-paredit
    solarized-theme
    parenface-plus
    helm
    company
    markdown-mode
    linum-relative
    ;; Clojure
    cider
    clojure-mode
    neotree
    rainbow-mode
    projectile
    helm-projectile
    golden-ratio
    powerline
    powerline-evil
    diminish
    clj-refactor
    )
  "My packages to install.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(require 'company)
(require 'cider)
(require 'paredit)
(require 'parenface-plus)
(require 'clojure-mode)
(require 'neotree)
(require 'markdown-mode)
(require 'linum-relative)
(require 'rainbow-mode)
(require 'projectile)
(require 'helm-projectile)
(require 'golden-ratio)
(require 'powerline)
(require 'powerline-evil)
(require 'diminish)
(require 'clj-refactor)

(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
                               ))

;; Use auto-complete as completion at point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(load-theme 'solarized-dark t)

(defun evil-pparedit-mode ()
  (paredit-mode)
  (evil-paredit-mode))

(defun cider-mode-setup ()
  ;; (ac-nrepl-setup)
  (evil-pparedit-mode))

;;; Configure CIDER
(add-hook 'cider-repl-mode-hook 'cider-mode-setup)
(add-hook 'clojure-mode-hook 'cider-mode-setup)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-use-clojure-font-lock t)

;(add-hook 'cider-interaction-mode-hook 'cider-mode-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; Paredit in clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

;; eldoc in clojure
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)

;; Cider... don't annoy me
(setq cider-hide-special-buffers t)
(setq cider-popup-stacktraces nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-popup-stacktraces t)

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

;;; general mappings
;(define-key cider-repl-mode-map (kbd "<up>") 'cider-backward-input)
;(define-key cider-repl-mode-map (kbd "<down>") 'cider-forward-input)
;(define-key cider-repl-mode-map (kbd "C-<up>") 'previous-line)
;(define-key cider-repl-mode-map (kbd "C-<down>") 'next-line)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'init-functions)
(require 'init-evil)
(require 'neotree-evil)
(require 'init-settings)
(require 'clojure-accents)
(require 'vtl)
(require 'julia-mode)
(require 'julia-repl)
