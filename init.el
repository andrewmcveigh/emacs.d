(require 'package)

(setq inhibit-splash-screen t)
(switch-to-buffer "*scratch*")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f81a9aabc6a70441e4a742dfd6d10b2bae1088830dc7aba9c9922f4b1bd2ba50" default)))
 '(safe-local-variable-values
   (quote
    ((eval define-clojure-indent
           (spec
            (quote defun)))
     (eval define-clojure-indent
           (s/fdef
            (quote defun)))
     (eval define-clojure-indent
           (m/do
            (quote defun)))
     (eval define-clojure-indent
           (domonad
            (quote defun)))
     (eval define-clojure-indent
           (defexpr
             (quote
              (2 nil nil
                 (1)))))
     (eval define-clojure-indent
           (fnode
            (quote defun)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (window-system)
  (set-frame-position (selected-frame) 0 0)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Add Marmalade repo
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(markdown-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(markdown-preview-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(haskell-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(go-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(geiser . "melpa-stable") t)

;; And load things!
;; (package-refresh-contents) ;; Uncomment if refresh needed E.G., on first use.
(package-initialize)

(defvar my-packages
  '(
    paredit
    evil
    evil-leader
    evil-paredit
    paren-face

    company

    markdown-mode
    dockerfile-mode
    ;; Clojure

    clojure-mode
    cider
    clj-refactor

    neotree

    helm
    golden-ratio
    projectile
    helm-projectile

    go-mode

    haskell-mode

    f

    let-alist

    s

    idris-mode

    ;; Racket
    geiser
    
    ;; Magit
    ;; magit
    ;; evil-magit
    

    nix-mode
    smooth-scrolling
    rust-mode

    julia-mode

    inf-ruby
    ;; material-theme
    monokai-theme

    fill-column-indicator

    ox-reveal
    htmlize
    )
  "My packages to install.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Load ./lisp sources
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Packages...
(require 'company)

(require 'cider)
(require 'clojure-mode)
(require 'clj-refactor)

(require 'paredit)
(require 'evil-paredit)
(require 'paren-face)

(require 'neotree)
(require 'markdown-mode)

(require 'projectile)
(require 'helm)
(require 'helm-projectile)
(require 'golden-ratio)

(require 'dockerfile-mode)
(require 'haskell-mode)
(require 'julia-mode)

(require 'inf-ruby)
(require 'go-mode)

(require 'idris-mode)

(require 'geiser)
(require 'nix-mode)
(require 'smooth-scrolling)

(require 'fill-column-indicator)

(require 'ox-reveal)
(require 'htmlize)

;; ;; ./lisp/*.el
(require 'electric-return)
(require 'init-autocomplete)
(require 'init-clojure)
(require 'init-evil)
;; (require 'init-evil-magit)
;; (require 'init-haskell)
;; (require 'init-keybindings)
(require 'init-lisp)
(require 'init-neotree)
;; (require 'init-pixie)
(require 'init-settings)
(require 'init-theme)
(require 'rust-mode)
(require 'speaking-time)
(require 'init-org)

(add-to-list 'load-path "~/.emacs.d/private")
(require 'db-conn)

