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
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "f81a9aabc6a70441e4a742dfd6d10b2bae1088830dc7aba9c9922f4b1bd2ba50" default)))
 '(package-selected-packages
   (quote
    (coffee-mode smooth-scrolling session rust-mode paren-face ox-reveal nix-mode neotree monokai-theme markdown-mode julia-mode inf-ruby idris-mode htmlize helm-projectile haskell-mode go-mode geiser fill-column-indicator f evil-paredit evil-leader dockerfile-mode company clj-refactor)))
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

;; (add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
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
    projectile
    helm-projectile

    nlinum
    go-mode

    haskell-mode

    f

    let-alist

    s

    idris-mode

    ;; Racket
    geiser

    ;; Magit
    magit
    evil-magit

    nix-mode
    smooth-scrolling
    rust-mode

    julia-mode

    inf-ruby
    monokai-theme

    fill-column-indicator

    ox-reveal
    htmlize

    session
    coffee-mode
    )
  "My packages to install.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Load ./lisp sources
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Packages...
(require 'company)
(require 'clojure-mode)
(require 'cider)
(require 'clj-refactor)

(require 'paredit)
(require 'evil-paredit)
(require 'paren-face)

(require 'neotree)
(require 'markdown-mode)

(require 'projectile)
(require 'helm)
(require 'helm-projectile)

(require 'dockerfile-mode)
(require 'haskell-mode)
(require 'julia-mode)

(require 'inf-ruby)
(require 'go-mode)
(require 'nlinum)

(require 'idris-mode)

(require 'geiser)
(require 'nix-mode)
(require 'smooth-scrolling)

(require 'fill-column-indicator)

(require 'ox-reveal)
(require 'htmlize)
(require 'rust-mode)
(require 'session)
(require 'coffee-mode)
(require 'magit)
(require 'evil-magit)

;; ./lisp/*.el
(require 'electric-return)
(require 'init-autocomplete)
(require 'init-clojure)
(require 'init-evil)
(require 'init-haskell)
(require 'init-keybindings)
(require 'init-lisp)
(require 'init-neotree)
;; ;; (require 'init-pixie)
(require 'init-settings)
(require 'init-theme)
(require 'speaking-time)
(require 'init-org)
(require 'init-idris)
