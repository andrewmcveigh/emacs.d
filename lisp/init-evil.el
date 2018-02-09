(require-packages 'evil 'evil-leader 'evil-commentary 'evil-paredit)
(require 'evil)
(require 'evil-commentary)
(require 'evil-leader)
(require 'evil-paredit)
(require 'init-eshell)

;;; Settings
(evil-mode t)
(evil-commentary-mode)
(global-evil-leader-mode)

;;; Helper Functions
(defun scroll-up-half-page ()
  (interactive)
  (evil-scroll-up nil)
  (evil-scroll-line-to-center
   (line-number-at-pos)))

(defun scroll-down-half-page ()
  (interactive)
  (evil-scroll-down nil)
  (evil-scroll-line-to-center
   (line-number-at-pos)))

;;; Keybindings
;;; normal mode
(define-key evil-normal-state-map (kbd "C-u") 'scroll-up-half-page)
(define-key evil-normal-state-map (kbd "C-d") 'scroll-down-half-page)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

(define-key evil-normal-state-map (kbd "S") 'paredit-splice-sexp)
(define-key evil-normal-state-map (kbd "W") 'paredit-wrap-round)
(define-key evil-normal-state-map (kbd "<left>") 'windmove-left)
(define-key evil-normal-state-map (kbd "<right>") 'windmove-right)
(define-key evil-normal-state-map (kbd "<up>") 'windmove-up)
(define-key evil-normal-state-map (kbd "<down>") 'windmove-down)
;;; Backspace jump % normal
(define-key evil-normal-state-map (kbd "\d") 'evil-jump-item)
;;; motions
;;; Backspace jump % motion
(define-key evil-motion-state-map (kbd "\d") 'evil-jump-item)
;;; visual mode
(define-key evil-visual-state-map (kbd "W") 'paredit-wrap-round)

(evil-define-operator evil-paredit-yank-eol (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-forward-char
  :move-point nil
  (interactive "<R><x>")
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-yank beg end type register)
    (goto-char beg)))

(defun custom-evil-paredit-mode-hook ()
  ;;; paredit
  (define-key evil-normal-state-map (kbd "<") 'paredit-forward-barf-sexp)
  (define-key evil-normal-state-map (kbd ">") 'paredit-forward-slurp-sexp))

(add-hook 'evil-paredit-mode-hook 'custom-evil-paredit-mode-hook)

(evil-define-key 'normal evil-paredit-mode-map
  (kbd "d") 'evil-paredit-delete
  (kbd "c") 'evil-paredit-change
  (kbd "y") 'evil-paredit-yank
  (kbd "Y") 'evil-paredit-yank-eol
  (kbd "D") 'evil-paredit-delete-line
  (kbd "C") 'evil-paredit-change-line
  (kbd "S") 'paredit-splice-sexp
  (kbd "X") 'paredit-backward-delete
  (kbd "x") 'paredit-forward-delete)

(defun re-replace-in-region (start end match replacement)
  (goto-char start)
  (while (and (< (point) end) (re-search-forward match end t))
    (replace-match replacement)))

(defun evil-remove-too-much-space (start end)
  (interactive "r")
  (save-excursion
    (if (and start end)
        (re-replace-in-region start end " +" " "))
    (indent-region start end)))

(defun evil-remove-too-much-space-in-current-paragraph ()
  (interactive)
  (mark-paragraph)
  (evil-remove-too-much-space (region-beginning) (region-end)))

;;; evil leader mappings
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "c=" 'delete-trailing-whitespace
  "yy" 'pbcopy
  "pp" 'pbpaste
  "dd" 'pbcut
  "de" 'dired
  "e=" 'evil-remove-too-much-space
  "=ip" 'evil-remove-too-much-space-in-current-paragraph
  "w[" 'paredit-wrap-square
  "w{" 'paredit-wrap-curly
  "w\"" (lambda ()
          (interactive)
          (paredit-doublequote)
          (paredit-forward-slurp-sexp))
  "wh" 'previous-buffer
  "wl" 'next-buffer
  "cl" (lambda ()
         (interactive)
         (comment-or-uncomment-region (line-beginning-position)
                                      (line-end-position)))
  "c<SPC>" (lambda ()
             (interactive)
             (comment-or-uncomment-region (line-beginning-position)
                                          (line-end-position)))
  "pf"  'helm-projectile-find-file
  "p,"  'paredit-backward-barf-sexp
  "p."  'paredit-backward-slurp-sexp
  "s"   'save-buffer
  "esh" (lambda ()
          (interactive)
          (if (string-equal "eshell-mode" major-mode)
              (eshell/x)
            (eshell/sh))))

;;; Resize windows
(global-set-key (kbd "s-\<") (lambda ()
                               (interactive)
                               (evil-window-decrease-width 3)))

(global-set-key (kbd "s-\>") (lambda ()
                               (interactive)
                               (evil-window-increase-width 3)))

(global-set-key (kbd "s-<left>")  'evil-prev-buffer)
(global-set-key (kbd "s-<right>") 'evil-next-buffer)
(global-set-key (kbd "C-S-H")   'previous-buffer)
(global-set-key (kbd "C-S-L") 'next-buffer)

(global-set-key (kbd "s-w")       (lambda () (interactive) (window--delete)))

(define-key evil-window-map "=" 'balance-windows)

;; override weird new gd behaviour, default to 1st location in buffer
(evil-define-motion evil-goto-definition ()
  "Go to definition or first occurrence of symbol under point."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t))
         (search (format "\\_<%s\\_>" (regexp-quote string)))
         ientry ipos)
    (if (null string)
        (user-error "No symbol under cursor")
      (setq isearch-forward t)
      (evil-search search t t (point-min)))))

(provide 'init-evil)
