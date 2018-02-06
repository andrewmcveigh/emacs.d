(require 'eshell)

(defun eshell/sh ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (if (projectile-project-p)
      (let* ((parent (projectile-project-root))
             (name   (car (last (split-string parent "/" t)))))
        (cd parent)
        (eshell "new")
        (rename-buffer (concat "*eshell: " name "*")))
    (let* ((parent default-directory)
           (name   (car (last (split-string parent "/" t)))))
        (eshell "new")
        (rename-buffer (concat "*eshell: " name "*")))))

(defun eshell/x ()
  (interactive)
  (insert "exit")
  (eshell-send-input))

(provide 'init-eshell)
