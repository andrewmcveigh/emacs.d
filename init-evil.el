(require 'evil)
(require 'evil-leader)
(require 'evil-paredit)

(evil-mode t)
(global-evil-leader-mode)

;;; paredit init in lisp(s)
;(add-hook 'clojure-mode-hook 'evil-pparedit-mode)
(add-hook 'emacs-lisp-mode-hook 'evil-pparedit-mode)

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

;;; Resize windows
(global-set-key (kbd "s-\<") (lambda ()
                               (interactive)
                               (evil-window-decrease-width 3)))
(global-set-key (kbd "s-\>") (lambda ()
                               (interactive)
                               (evil-window-increase-width 3)))

(provide 'init-evil)
