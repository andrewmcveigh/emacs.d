(defun left-vertial-frame ()
  (interactive)
  (new-frame)
  (set-frame-size (selected-frame) 148 135)
  (set-frame-position (selected-frame) -1078 -63))

(defun right-vertial-frame ()
  (interactive)
  (new-frame)
  (set-frame-size (selected-frame) 148 135)
  (set-frame-position (selected-frame) 2562 -63))

(provide 'init-functions)
