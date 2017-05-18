;;; paredit key mappings
(define-key paredit-mode-map (kbd "RET") 'electrify-return-if-match)

;; (defvar copy-cmd
;;   (cond ((executable-find "pbcopy") "pbcopy")
;;         ((executable-find "xclip") "xclip -sel clip")))

;; (defvar paste-cmd
;;   (cond ((executable-find "pbpaste") "pbpaste")
;;         ((executable-find "xclip") "xclip -o")))

;; (defun pbcopy ()
;;   (interactive)
;;   (shell-command-on-region (point) (mark) copy-cmd)
;;   (setq deactivate-mark t))

;; (defun pbpaste ()
;;   (interactive)
;;   (shell-command-on-region (point)
;;                            (if mark-active (mark) (point))
;;                            paste-cmd))

;; (defun pbcut ()
;;   (interactive)
;;   (pbcopy)
;;   (delete-region (region-beginning) (region-end)))

(defun scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb"))) ;lambda
(global-set-key (kbd "M-f") (lambda () (interactive) (insert "\u0192"))) ;function
(global-set-key (kbd "M--") (lambda () (interactive) (insert "\u2192"))) ;right arrow

(provide 'init-keybindings)
