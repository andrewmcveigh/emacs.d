(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq inhibit-splash-screen t)
(switch-to-buffer "*scratch*")

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;;(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file   (concat dotfiles-dir "custom.el"))
(load custom-file)

(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

(defvar archives
  '(("melpa-stable" . "https://melpa.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

(defvar pinned-packages
  '(
    ((evil-mode             . "melpa-stable") t)
    ((evil                  . "melpa-stable") t)

    ((markdown-mode         . "melpa-stable") t)
    ((markdown-preview-mode . "melpa-stable") t)
    ((haskell-mode          . "melpa-stable") t)
    ((purescript-mode       . "melpa-stable") t)
    ((psc-ide               . "melpa-stable") t)
    ((flycheck-haskell      . "melpa-stable") t)
    ((go-mode               . "melpa-stable") t)
    ((geiser                . "melpa-stable") t)

    ((clojure-mode          . "melpa-stable") t)
    ((cider                 . "melpa-stable") t)
    ((clj-refactor          . "melpa-stable") t)

    ))

(require 'package)
(dolist (a archives)        (add-to-list 'package-archives a))
(dolist (p pinned-packages) (apply 'add-to-list 'package-pinned-packages p))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(defun require-package (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defun require-packages (&rest pkgs)
  (dolist (p pkgs) (require-package p)))

(add-to-list 'load-path (concat dotfiles-dir "lisp"))

(require 'init-evil)
(require 'init-autocomplete)
(require 'init-clojure)
(require 'init-flycheck)
(require 'init-keybindings)
(require 'init-lisp)
(require 'init-settings)
(require 'init-theme)
(require 'init-misc)

(defvar gui? (display-graphic-p))

(require 'init-eshell)
(require 'init-haskell)
(require 'init-purescript)
(require 'init-idris)
(require 'init-nix)
(require 'init-org)
(require 'init-ruby)
(require 'init-rust)

(require 'init-magit)
(require 'init-markdown)

(require 'init-emacsclient)

(when (not gui?)
  (require-package 'xclip)
  (xclip-mode 1)
  (require 'init-emacs-nw))
