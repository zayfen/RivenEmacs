(use-package leetcode
  :straight t
  :commands (leetcode)
  :init
  (use-package graphql
    :straight t
    :defer t)
  (use-package aio
    :defer t
    :straight t)
  :custom
  (url-debug t)
  (leetcode-prefer-language "python3"))

(provide 're-leetcode)
