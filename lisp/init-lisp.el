(require 'paredit)
(require 'evil-paredit)

;;; Helper Functions
(defun pop-mark-or-tag ()
  (interactive)
  (let (m (pop-tag-mark))
    (if (not m) (pop-global-mark))))

;;; Keybindings
(defun evil-elisp-leader-keys ()
  (evil-leader/set-key
    "ef" 'eval-buffer
    "ee" 'eval-last-sexp
    "er" 'eval-last-sexp-and-replace
    "t"  'pop-mark-or-tag))

(define-key evil-normal-state-map "gf" 'find-function)

;;; Modes
(define-minor-mode evil-elisp-mode
  "Evil Elisp"
  :lighter " el&"
  (evil-elisp-leader-keys))

;;; Hooks
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'evil-elisp-mode)

(provide 'init-lisp)
