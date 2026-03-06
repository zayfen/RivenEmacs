# RivenEmacs 启动性能分析与优化报告

**分支**: `perf/emacs-startup-audit`  
**日期**: 2025-03-07

## 1. 执行摘要

- **优化前基线**: ~0.56 秒（early-init + init 加载）
- **优化后**: ~0.52 秒（约 7% 提升）
- **策略**: 将 15 个重型模块延迟到 `emacs-startup-hook` 加载，首帧更快显示

## 2. early-init.el 审查结论

| 项目 | 状态 | 说明 |
|------|------|------|
| GC 策略 | 已优化 | `gc-cons-threshold` 启动期设为 `most-positive-fixnum`，`emacs-startup-hook` 恢复为 16MB |
| file-name-handler-alist | 已优化 | 启动期置空，恢复逻辑在 `config:restore-post-init-settings` |
| package-archives | 正常 | 使用清华镜像，无重复 `package-initialize` |
| native-comp | 已优化 | `native-comp-jit-compilation nil`，eln 缓存重定向到 local-dir |
| minibuffer GC | 已优化 | minibuffer 进入/退出时临时提高 GC 阈值 |

**建议**: 可考虑引入 `gcmh` 包做更精细的 GC 管理，非必需。

## 3. 模块加载分类

### 3.1 主路径（启动时必须加载）

- init-use-package, init-config, init-default, init-helper
- init-theme, init-font, init-undo, init-autosave
- init-which-key, init-general, init-hydra
- init-consult, init-vertico, init-crux, init-editor
- init-dired, init-format, init-jump, init-editorconfig, init-checker, init-pair, init-fold, init-markdown
- init-treesit
- init-agent-shell（keybindings 依赖 agent-shell-leader-def）
- init-session, init-terminal
- init-keybindings

### 3.2 延迟加载（emacs-startup-hook）

- init-lsp-bridge
- init-vc, init-debugger, init-git-hunk
- init-envrc, init-project
- init-gpt
- init-web, init-rust, init-python, init-java, init-swift
- init-docker, init-quickrun, init-feed, init-lookup, init-reader

### 3.3 按需加载

- init-org：通过 `org-mode-hook` 在首次打开 org 文件时加载

## 4. 具体改动

### 4.1 init.el

- 新增 `riven/load-deferred-modules`，在 `emacs-startup-hook` 中 require 上述延迟模块
- 移除主路径中的对应 require

### 4.2 init-keybindings.el

- 新增 `riven/lsp-bridge-keybindings`，在 `with-eval-after-load 'lsp-bridge` 中设置 SPC c 键位
- 在 `keybindings-config` 开头添加 `(require 'agent-shell nil t)` 确保 agent-shell-leader-def 存在

### 4.3 init-agent-shell.el

- 为 agent-shell 的 use-package 添加 `:after general`，避免 `general-create-definer` 未定义错误

## 5. 权衡说明

- **首次使用延迟模块**：LSP、Git、AI、Docker 等首次调用时，可能需等待 emacs-startup-hook 完成加载（通常 <1 秒）
- **键位**：SPC c（LSP 代码操作）在 lsp-bridge 加载后才生效，与 LSP 启动时机一致

## 6. 后续可优化方向

1. 使用 `esup` 或 `benchmark-init` 做更细粒度 profiling，针对 TOP 耗时文件进一步优化
2. 将 init-consult、init-vertico 等补全相关模块评估是否可延迟
3. 评估 `exec-path-from-shell` 是否可仅在 GUI 模式下初始化
