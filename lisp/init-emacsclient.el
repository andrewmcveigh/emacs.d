(defun set-font-size (sz)
  (let* ((font-name "Iosevka")
         (font-size sz))
    (set-default-font font-name)
    (set-face-attribute 'default nil
                        :family font-name
                        :height font-size
                        :inherit 'fixed-pitch
                        ;; :style 'ligset-haskell
                        :weight 'light
                        )
    (set-face-attribute 'font-lock-keyword-face nil
                        :family font-name
                        :height font-size
                        :inherit 'fixed-pitch
                        :weight 'light)))

;; (set-font-size 140)


(defun font-scale-up ()
  (let ((sz (face-attribute 'default :height)))
    (set-font-size (+ sz 20))))

(defun font-scale-down ()
  (let ((sz (face-attribute 'default :height)))
    (set-font-size (- sz 20))))

;; (font-scale-up)

(set-font-size 170)

(set-background-color "#111120")

(evil-define-key 'normal cider-repl-mode-map (kbd "RET") 'cider-repl-return)
(evil-define-key 'normal cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
(evil-define-key 'normal cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)

(require 'init-ligatures)
(provide 'init-emacsclient)
