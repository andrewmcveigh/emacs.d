(require 'haskell-interactive-mode)
(require 'haskell-process)

;;; Keybindings
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

(defun evil-haskell-leader-keys ()
  (evil-leader/set-key
    "ef" 'haskell-process-load-or-reload))

(defun evil-haskell-keymapping ()
  (define-key evil-normal-state-map (kbd "K") 'haskell-process-do-info))

;;; Modes
(define-minor-mode evil-haskell-mode
  "Evil Haskell*"
  :lighter " €hs"
  (progn
    (evil-haskell-leader-keys)
    (evil-haskell-keymapping)))

;;; Hooks
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'evil-haskell-mode)

;; (add-hook 'elm-mode-hook 'elm-indent-mode)
;; (add-hook 'elm-mode-hook 'haskell-indent-mode)
;; (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-hook 'compilation-mode-hook 'evil-mode)

(add-hook 'idris-mode-hook (lambda () (load-theme 'idris-material-theme t)))

(provide 'init-haskell)
