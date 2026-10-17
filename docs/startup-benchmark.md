# RivenEmacs 启动性能基准

> **历史文档，数据可能已过期。** 本页记录 2025-03 的基准数字和当时
> （lsp-bridge / general.el / SPC leader 时代）的优化改动。当前实现已改为
> 声明式 `C-c` 键位、eglot 为唯一 LSP、`init-lsp-bridge`/`init-git-hunk`/
> `init-general` 已删除、延迟模块在 `after-init-hook`（depth -20）加载；
> 不要用下列数字判断当前启动速度，请本地重新运行 `benchmark-startup.el`。

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
| package 初始化与 lazy command 清理后 | **~0.77 秒** | 显式初始化已安装包，消除 batch 伪缺包错误 |

（历史注记）当时延迟加载的模块在 `emacs-startup-hook` 中加载，不阻塞首帧显示；
当前延迟模块注册在 `after-init-hook` depth -20。

## 优化改动摘要

1. **延迟加载（历史）**：将当时的重型模块——包括已删除的 `init-lsp-bridge`、`init-git-hunk`——移至 `emacs-startup-hook`；当前延迟列表以 `init.el` 为准
2. **LSP 键位（历史）**：lsp-bridge 的 SPC c 键位改为 `with-eval-after-load 'lsp-bridge` 中设置；lsp-bridge 已删除，C-c 键位由声明式引擎统一定义
3. **AI Code 入口**：`C-c =` 由声明式 keybinding 引擎注册，直接指向 `ai-code` 的编码 agent 命令
4. **package 初始化**：`early-init.el` 与直接 `-l init.el` 路径都会显式初始化 package.el，确保仓库内 `elpa/` 包在 batch 检查中可见
5. **lazy command 清理（历史）**：移除剩余 `general.el` 启动依赖，并将 `iedit`、`sudo-edit`、`vterm` 改为按命令加载
6. **补全模块拆分**：将原 `init-consult.el` 拆为 `init-minibuffer`、`init-completion-ui`、`init-consult` 三个职责清晰的模块，保留 `init-vertico` 兼容 shim
