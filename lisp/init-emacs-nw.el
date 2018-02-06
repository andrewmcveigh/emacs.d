(let* ((font-name "Iosevka")
       (font-size "13")
       (font-str (concat font-name "-" font-size)))
  (set-default-font font-name)
  (set-face-attribute 'default nil
                      :font font-str
                      :inherit 'fixed-pitch
                      :weight 'semi-light)
  (set-face-attribute 'font-lock-keyword-face nil
                      :font font-str
                      :inherit 'fixed-pitch
                      :weight 'semi-light))

(set-background-color "#111111")
(set-face-background 'whitespace-space "#111111")
(set-face-foreground 'whitespace-space "#333333")
(set-face-foreground 'parenthesis "#444444")
(set-face-background 'linum "#111111")
(set-face-foreground 'linum "#444444")

(require 'init-ligatures)

(provide 'init-emacs-nw)
