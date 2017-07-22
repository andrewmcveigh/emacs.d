(require 'eshell)

(defun eshell/sh ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (projectile-project-root))
         (name   (car (last (split-string parent "/" t)))))
    (cd parent)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input))

(provide 'init-eshell)
