# RivenEmacs 启动性能基准

## 测量方法

- **无配置基线**: `emacs -Q --batch --eval "(message (emacs-init-time))"`
- **RivenEmacs 配置**: `emacs -Q --batch -l benchmark-startup.el`
- 测量脚本 `benchmark-startup.el` 使用 `float-time` 测量 early-init + init 的加载耗时

## 优化前基线 (2025-03-07)

| 场景 | 耗时 |
|------|------|
| 无配置 (emacs -Q) | ~0.0004 秒 |
| RivenEmacs 完整配置 | **~0.56 秒** |

## 优化后对比 (perf/emacs-startup-audit 分支)

| 场景 | 耗时 | 变化 |
|------|------|------|
| RivenEmacs 主路径加载 | **~0.52 秒** | **约 -7%** |

延迟加载的模块在 `emacs-startup-hook` 中加载，不阻塞首帧显示。

## 优化改动摘要

1. **延迟加载**：将 init-lsp-bridge、init-vc、init-debugger、init-git-hunk、init-envrc、init-project、init-gpt、init-web、init-rust、init-python、init-java、init-swift、init-docker、init-quickrun、init-feed、init-lookup、init-reader 移至 `emacs-startup-hook`
2. **LSP 键位**：lsp-bridge 的 SPC c 键位改为 `with-eval-after-load 'lsp-bridge` 中设置
3. **agent-shell 依赖**：为 agent-shell 添加 `:after general`，并在 keybindings-config 中确保 agent-shell 已加载
