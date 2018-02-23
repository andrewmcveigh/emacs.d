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

(evil-define-key 'normal cider-repl-mode-map (kbd "RET") 'cider-repl-return)
(evil-define-key 'normal cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
(evil-define-key 'normal cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)

(require 'init-ligatures)
