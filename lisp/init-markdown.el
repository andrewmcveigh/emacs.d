(require-packages 'markdown-mode)
 ;;'polymode 'poly-markdown
(require 'markdown-mode)
;; (require 'polymode)
;; (require 'poly-markdown)
;; (require 'polymode-compat)

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq markdown-fontify-code-blocks-natively t)

(defun md-reset-face (face)
  (let* ((font-name "Iosevka")
         (font-size "17")
         (font-str (concat font-name "-" font-size)))
    (set-face-attribute face nil
                        :font font-str
                        :inherit 'fixed-pitch
                        :weight 'light)))

(md-reset-face 'markdown-code-face)
(md-reset-face 'markdown-language-keyword-face)
(md-reset-face 'markdown-blockquote-face)
(md-reset-face 'markdown-bold-face)
(md-reset-face 'markdown-comment-face)
(md-reset-face 'markdown-gfm-checkbox-face)
(md-reset-face 'markdown-inline-code-face)
(md-reset-face 'markdown-header-delimiter-face)
(md-reset-face 'markdown-html-entity-face)
;; (md-reset-face 'markdown-footnote-face)
(md-reset-face 'markdown-footnote-marker-face)
(md-reset-face 'markdown-footnote-text-face)
(md-reset-face 'markdown-header-face)
(md-reset-face 'markdown-header-face-2)
(md-reset-face 'markdown-italic-face)
(md-reset-face 'markdown-language-info-face)
(md-reset-face 'markdown-highlight-face)
(md-reset-face 'markdown-pre-face)


(provide 'init-markdown)
