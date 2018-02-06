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

(set-background-color "#111120")

(require 'init-ligatures)
