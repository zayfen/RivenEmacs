(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-live-preview-engine 'eww) ; 或者 'markdown-preview-mode
  :config
  ;; 确保在 markdown-mode 加载完毕后才修改其 faces
  (with-eval-after-load 'markdown-mode
    ;; --- 开始设置 Markdown 特定的 faces ---

    ;; 标题 Faces
    (set-face-attribute 'markdown-header-face-1 nil
                        :inherit 'font-lock-function-name-face
                        :scale 1.8
                        :weight 'bold)
    (set-face-attribute 'markdown-header-face-2 nil
                        :inherit 'font-lock-function-name-face
                        :scale 1.6
                        :weight 'bold)
    (set-face-attribute 'markdown-header-face-3 nil
                        :inherit 'font-lock-function-name-face
                        :scale 1.4
                        :weight 'semi-bold)
    (set-face-attribute 'markdown-header-face-4 nil
                        :inherit 'font-lock-function-name-face
                        :scale 1.2
                        :weight 'semi-bold)
    (set-face-attribute 'markdown-header-face-5 nil
                        :inherit 'font-lock-function-name-face
                        :scale 1.1)
    (set-face-attribute 'markdown-header-face-6 nil
                        :inherit 'font-lock-function-name-face
                        :scale 1.0)

    ;; 代码块 Faces
    (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch :background "#333333") ; 您可以根据主题调整背景色
    (set-face-attribute 'markdown-inline-code-face nil
                        :inherit 'fixed-pitch
                        :foreground "orange" ; 您可以根据主题调整前景色
                        :background "#3A3A3A") ; 您可以根据主题调整背景色

    ;; 强调 Faces (粗体和斜体)
    (set-face-attribute 'markdown-bold-face nil :weight 'bold)
    (set-face-attribute 'markdown-italic-face nil :slant 'italic)

    ;; --- 结束设置 Markdown 特定的 faces ---
    ) ; 结束 with-eval-after-load

  ;; 为 Markdown 模式启用 variable-pitch (变宽字体)
  (add-hook 'markdown-mode-hook #'variable-pitch-mode)

  ;; 确保代码元素 (如代码块和行内代码) 使用 fixed-pitch (等宽字体) 的字体家族和高度
  (add-hook 'markdown-mode-hook
            (lambda ()
              (face-remap-add-relative 'markdown-code-face
                                       :family (face-attribute 'fixed-pitch :family)
                                       :height (face-attribute 'fixed-pitch :height))
              (face-remap-add-relative 'markdown-pre-face
                                       :family (face-attribute 'fixed-pitch :family)
                                       :height (face-attribute 'fixed-pitch :height))
              (face-remap-add-relative 'markdown-inline-code-face
                                       :family (face-attribute 'fixed-pitch :family)
                                       :height (face-attribute 'fixed-pitch :height))))
  )

;; For nicer rendering of horizontal rules and other elements
(add-hook 'markdown-mode-hook #'visual-line-mode) ; Better line wrapping
(add-hook 'markdown-mode-hook (lambda () (setq display-line-numbers-type nil))) ; Optional: Hide line numbers in Markdown

;; Enable prettify symbols for a more 'WYSIWYG' feel in some contexts
;; (add-hook 'markdown-mode-hook 'prettify-symbols-mode)
;; You can define markdown-specific prettifications if desired:
;; (setq markdown-prettify-symbols-alist '((">=" . ?≥) ("<=" . ?≤) ("->" . ?→)))

(provide 'init-markdown)
