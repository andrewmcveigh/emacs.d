(require 'evil-leader)
(require 'evil-paredit)

;;; Settings
(evil-mode t)
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

(define-key evil-normal-state-map (kbd "Y") 'evil-yank-eol)

(defun custom-evil-paredit-mode-hook ()
  ;;; paredit
  (define-key evil-normal-state-map (kbd "<") 'paredit-forward-barf-sexp)
  (define-key evil-normal-state-map (kbd ">") 'paredit-forward-slurp-sexp))

(add-hook 'evil-paredit-mode-hook 'custom-evil-paredit-mode-hook)

(evil-define-operator evil-yank-eol (beg end type register yank-handler)
  "Saves 'til end of line into the kill-ring."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg)))
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond
     ((eq type 'block)
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (evil-yank beg end 'block register yank-handler)))
     ((eq type 'line)
      (evil-yank beg end type register yank-handler))
     (t
      (evil-yank beg (line-end-position) type register yank-handler)))))

(evil-define-key 'normal evil-paredit-mode-map
  (kbd "d") 'evil-paredit-delete
  (kbd "c") 'evil-paredit-change
  (kbd "y") 'evil-paredit-yank
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
  "e=" 'evil-remove-too-much-space
  "=ip" 'evil-remove-too-much-space-in-current-paragraph
  "nt" 'neotree-toggle
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
                                      (line-end-position)))
  "c<SPC>" (lambda ()
             (interactive)
             (comment-or-uncomment-region (line-beginning-position)
                                          (line-end-position)))
  "pf" 'helm-projectile-find-file
  "p," 'paredit-backward-barf-sexp
  "p." 'paredit-backward-slurp-sexp
  "s" 'save-buffer)

;;; Resize windows
(global-set-key (kbd "s-\<") (lambda ()
                               (interactive)
                               (neo-buffer--unlock-width)
                               (evil-window-decrease-width 3)
                               (neo-buffer--lock-width)))
(global-set-key (kbd "s-\>") (lambda ()
                               (interactive)
                               (neo-buffer--unlock-width)
                               (evil-window-increase-width 3)
                               (neo-buffer--lock-width)))

(global-set-key (kbd "s-<left>") 'evil-prev-buffer)
(global-set-key (kbd "s-<right>") 'evil-prev-buffer)

(define-key evil-window-map "=" 'balance-windows-area)

(provide 'init-evil)
