
;; Emacs 24 or higher!
(when (< emacs-major-version 24)
  (error "This setup requires Emacs v24, or higher. You have: v%d"
         emacs-major-version))

;; Configure package manager
(require 'package)

;; Add Marmalade repo
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

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
    material-theme
    paren-face
    helm
    company
    markdown-mode
    linum-relative
    ;; Clojure
    cider
    clojure-mode
    inf-clojure
    neotree
    projectile
    helm-projectile
    golden-ratio
    clj-refactor
    exec-path-from-shell
    )
  "My packages to install.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Load ./lisp sources
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/packages/powerline")

;; Packages...
(require 'company)
(require 'cider)
(require 'inf-clojure)
(require 'paredit)
(require 'evil-paredit)
(require 'paren-face)
(require 'clojure-mode)
(require 'neotree)
(require 'markdown-mode)
(require 'linum-relative)
(require 'projectile)
(require 'helm-projectile)
(require 'golden-ratio)
(require 'clj-refactor)
(require 'powerline)
(require 'exec-path-from-shell)

;; ./lisp/*.el
(require 'powerline)
(require 'electric-return)
(require 'init-autocomplete)
(require 'init-clojure)
(require 'init-evil)
(require 'init-keybindings)
(require 'init-lisp)
(require 'init-neotree)
(require 'init-settings)
(require 'init-theme)
