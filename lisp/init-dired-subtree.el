
(use-package dired-subtree
  :demand
  :bind
  (:map dired-mode-map
        ("<enter>" . mhj/dwim-toggle-or-open)
        ("<return>" . mhj/dwim-toggle-or-open)
        ("<tab>" . mhj/dwim-toggle-or-open)
        ("<down-mouse-1>" . mhj/mouse-dwim-to-toggle-or-open))
  :config
  (progn
    ;; Function to customize the line prefixes (I simply indent the lines a bit)
    (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
    (setq dired-subtree-use-backgrounds nil)))

(defun mhj/dwim-toggle-or-open ()
  "Toggle subtree or open the file."
  (interactive)
  (if (file-directory-p (dired-get-file-for-visit))
      (progn
        (dired-subtree-toggle)
        (revert-buffer))
    (dired-find-file)))

(defun mhj/mouse-dwim-to-toggle-or-open (event)
  "Toggle subtree or the open file on mouse-click in dired."
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
         (buffer (window-buffer window))
         (pos (posn-point (event-end event))))
    (progn
      (with-current-buffer buffer
        (goto-char pos)
        (mhj/dwim-toggle-or-open)))))

(use-package dired
  :ensure nil
  :config
  (progn
    ;; (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")
    (setq dired-listing-switches "-lXGh --group-directories-first")
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

(defun mhj/toggle-project-explorer ()
  "Toggle the project explorer window."
  (interactive)
  (let* ((buffer (dired-noselect (projectile-project-root)))
         (window (get-buffer-window buffer)))
    (if window
        (mhj/hide-project-explorer)
      (mhj/show-project-explorer))))

(defun mhj/show-project-explorer ()
  "Project dired buffer on the side of the frame.
Shows the projectile root folder using dired on the left side of
the frame and makes it a dedicated window for that buffer."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (progn
      ;; (display-buffer-in-side-window buffer '((side . left) (window-width . 0.2)))
      (display-buffer buffer)
;;      (set-window-dedicated-p (get-buffer-window buffer) nil)
      )))

(defun mhj/hide-project-explorer ()
  "Hide the project-explorer window."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (progn
      (delete-window (get-buffer-window buffer))
      (kill-buffer buffer))))

(evil-leader/set-key
  "nt" 'mhj/toggle-project-explorer)

(provide 'init-dired-subtree)
