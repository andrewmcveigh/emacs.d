(setq inhibit-splash-screen t)
(switch-to-buffer "*scratch*")

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file   (concat dotfiles-dir "custom.el"))
(load custom-file)

(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

(defvar archives
  '(("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar pinned-packages
  '(((markdown-mode         . "melpa-stable") t)
    ((markdown-preview-mode . "melpa-stable") t)
    ((haskell-mode          . "melpa-stable") t)
    ((go-mode               . "melpa-stable") t)
    ((geiser                . "melpa-stable") t)))

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
(require 'init-misc)
(require 'init-evil)
(require 'init-autocomplete)
(require 'init-clojure)
(require 'init-dired)
(require 'init-eshell)
(require 'init-haskell)
(require 'init-idris)
(require 'init-keybindings)
(require 'init-lisp)
(require 'init-org)
(require 'init-settings)
(require 'init-theme)
