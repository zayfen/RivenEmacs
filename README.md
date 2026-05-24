# RivenEmacs

RivenEmacs 是一套面向现代开发工作流的 Emacs 配置，重点放在三件事上：模块边界清晰、启动路径轻量、开发能力完整。它把 UI、补全、编辑、IDE、语言、AI、MCP、会话和工具链拆成独立模块，通过 `init.el` 统一编排加载顺序，避免配置长期演进后变成难以维护的单体文件。

## 核心优势

- **架构清晰**：所有能力按目录分层放在 `lisp/` 下，入口只负责装配，模块只负责自己的领域。
- **启动快**：`early-init.el` 处理 GC、文件名处理器、native-comp 缓存等启动期优化，重型模块延迟到 `emacs-startup-hook`。
- **现代 IDE 体验**：默认使用 `eglot`，配合 `eglot-booster`、Tree-sitter、Flymake、Apheleia、Corfu/Cape，覆盖跳转、诊断、格式化、补全和文档。
- **AI 与 MCP 原生集成**：内置 `gptel`、Agent Shell、`ai-code` 和多种 MCP server 配置，支持文件系统、记忆、浏览器、搜索等工具能力。
- **声明式键位**：键位集中在 `lisp/keymaps/`，由声明式 spec 和引擎生成，便于查重、重载和维护 which-key 分组。
- **可复现依赖管理**：`scripts/riven-deps.sh` 可以检查、安装和修复核心依赖、LSP、AI、MCP、拼写和 Markdown 预览工具。
- **集中配置入口**：常用变量放在 `rivenEmacs` defgroup 下，可通过 `customize` 或环境变量调整。

## 快速开始

```bash
# 1) 检查当前机器状态：依赖、环境变量和 batch init
bash scripts/riven-deps.sh doctor

# 2) 自动修复缺失依赖，并提示补齐环境变量
bash scripts/riven-deps.sh fix

# 3) 再次验证
bash scripts/riven-deps.sh doctor
```

手动验证配置是否能完整加载：

```bash
emacs --batch -l init.el --eval "(message \"Config loaded successfully\")"
```

## 架构总览

```text
RivenEmacs
├── early-init.el              # 启动期性能优化、package 初始化、native-comp 缓存
├── init.el                    # 唯一装配入口：设置 load-path、分层加载模块
├── lisp/
│   ├── core/                  # 全局配置、默认值、helper、use-package
│   ├── ui/                    # 主题、字体、which-key、hydra
│   ├── completion/            # Vertico、Consult、Orderless、Corfu、Cape、Embark
│   ├── editor/                # 编辑体验、格式化、成对符号、折叠、Markdown
│   ├── files/                 # Dired 与文件操作
│   ├── parser/                # Tree-sitter 自动 remap 与 grammar 管理
│   ├── ide/                   # Eglot、Flymake、跳转、调试、项目和 envrc
│   ├── vcs/                   # Magit、Git gutter/hunk 等版本控制能力
│   ├── ai/                    # gptel、MCP、Agent Shell、AI Code Interface
│   ├── lang/                  # Web、Rust、Python、Java、Swift、Org 等语言配置
│   ├── keymaps/               # 声明式键位 spec、命令和应用引擎
│   ├── session/               # desktop.el 会话持久化
│   ├── tools/                 # Docker、Quickrun、Feed、Lookup、Terminal、Reader
│   └── writing/               # 写作与导出相关配置
├── scripts/riven-deps.sh      # 跨平台依赖检查、安装和修复脚本
└── docs/                      # 性能报告和设计文档
```

### 加载路径

`init.el` 将配置拆成显式加载阶段：

1. **Core**：`init-use-package`、`init-config`、`init-default`、`init-helper`
2. **UI 与编辑基础**：主题、字体、撤销、自动保存、which-key、补全、编辑器、Dired、格式化、Tree-sitter
3. **AI 入口**：先加载 Agent Shell 入口，保留命令和 transport 的延迟加载能力
4. **写作与会话**：写作模块、Org 按需加载、`desktop.el` 会话管理
5. **工具与键位**：终端和声明式键位聚合
6. **延迟模块**：Eglot、VCS、调试、项目、GPT/MCP、语言和工具模块在 `emacs-startup-hook` 中加载

这种结构让首帧更快出现，同时保持完整开发环境在启动完成后自动就位。

## 特性

### 补全、搜索与操作

- `vertico + consult + orderless` 提供轻量、可组合的 minibuffer 交互。
- `corfu + cape` 负责代码补全，候选文档通过 `corfu-popupinfo` 显示。
- `embark + embark-consult` 提供上下文动作和候选结果联动。
- `which-key` 展示结构化快捷键分组，降低记忆成本。

### IDE 与语言开发

- 默认 LSP 客户端为 Emacs 内置 `eglot`，在 `rivenEmacs-lsp-modes` 中集中声明启用模式。
- `eglot-booster` 在可用时自动启用，提高 JSON-RPC 传输性能。
- Python LSP 会按 `basedpyright-langserver`、`pyright-langserver`、`ruff server`、`pylsp` 顺序选择可用实现。
- Vue、Kotlin 等语言 server 注册逻辑集中在 `lisp/ide/init-eglot.el`。
- Tree-sitter 由 `treesit-auto` 管理，支持现代 mode remap 和手动 grammar 安装。
- Flymake 负责诊断，ElDoc 保持单行 minibuffer 文档，减少窗口抖动。

### AI 与 MCP

- `gptel` 支持 Zayfen、DeepSeek、OpenAI-compatible 后端，并可配置 Responses API 模型。
- MCP 配置集中在 `lisp/ai/init-gpt.el`，支持 filesystem、memory、Playwright browser、browser-use、Brave Search、Tavily 等 server。
- 缺少 `BRAVE_API_KEY` 或 `TAVILY_API_KEY` 时，对应 MCP server 会自动跳过，不阻塞主配置加载。
- `agent-shell` 和 `ai-code` 提供 Codex、Claude Code、Opencode、Aider 等 AI 编码入口。

在 Emacs 内验证 MCP 配置：

```elisp
M-x riven/gptel-mcp-verify
```

### 编辑与生产力

- `apheleia` 负责格式化，`C-S-i` 可对 region 或整个 buffer 格式化。
- `jinx` 提供拼写检查，`M-$` 修正拼写，`C-M-$` 设置语言。
- `yasnippet` 统一代码片段能力，并兼容 snippet 字段跳转。
- `desktop.el` 保存 buffer、point、窗口状态和历史，启动后按需懒恢复。
- Dired、Quickrun、Terminal、Docker、Feed、Lookup、Reader 等工具按模块维护。

### 键位系统

RivenEmacs 使用 `C-c` 作为结构化命令入口，键位定义不散落在各模块中，而是集中到：

- `lisp/keymaps/keybindings-spec-core.el`
- `lisp/keymaps/keybindings-spec-ai.el`
- `lisp/keymaps/keybindings-spec-lsp.el`
- `lisp/keymaps/keybindings-spec-org.el`
- `lisp/keymaps/keybindings-engine.el`

键位引擎会清理 RivenEmacs 拥有的前缀、注册 which-key 描述，并在发现重复或缺失命令时输出提示。

## 依赖管理

查看脚本帮助：

```bash
bash scripts/riven-deps.sh --help
```

### 常用命令

- `install`：安装指定依赖组。
- `doctor`：检查依赖、环境变量和 `emacs --batch -l init.el` 加载结果。
- `fix`：安装缺失依赖，并交互式提示补齐环境变量。
- `list`：打印依赖组和脚本追踪的环境变量。

### 安装示例

```bash
# 默认安装 core + lsp
bash scripts/riven-deps.sh install

# 安装全部依赖组
bash scripts/riven-deps.sh install --all

# 只安装 AI + MCP 栈
bash scripts/riven-deps.sh install --ai --mcp

# 安装 jinx 所需拼写检查运行时
bash scripts/riven-deps.sh install --spell
```

### 修复示例

```bash
# 默认修复 core + lsp + ai + mcp + markdown + spell
bash scripts/riven-deps.sh fix

# 只提示并写入环境变量，不安装依赖
bash scripts/riven-deps.sh fix --env-only

# 只安装依赖，不提示环境变量
bash scripts/riven-deps.sh fix --deps-only

# 将环境变量写入指定 shell profile
bash scripts/riven-deps.sh fix --profile ~/.zshrc
```

### 依赖组

- `core`：`emacs`、`git`、`ripgrep`、`fd`、`node`、`npm`、`python3`
- `lsp`：`ruff`、`pyright`、`typescript-language-server`、`vtsls`、`vue-language-server`、`emacs-lsp-booster` 等
- `ai`：`cursor-agent-acp`、`codex`、`codex-acp`、`claude-agent-acp`、`claude`、`opencode`
- `mcp`：filesystem、memory、sequential-thinking、everything、playwright、browser-use、brave、tavily MCP 工具
- `markdown`：`go-grip`
- `spell`：`enchant` + `hunspell`，供 `jinx` 使用
- `extra`：Docker 和额外开发工具

## 环境变量

配置会读取以下变量；`scripts/riven-deps.sh` 会追踪并可交互式写入其中除 `ZAYFEN_API_KEY` 外的变量：

- `DEFAULT_WORKSPACE`
- `ZAYFEN_API_KEY`
- `DEEPSEEK_API_KEY`
- `GROQ_API_KEY`
- `ANTHROPIC_AUTH_TOKEN`
- `ANTHROPIC_BASE_URL`
- `OPENAI_API_KEY`
- `BRAVE_API_KEY`
- `TAVILY_API_KEY`
- `HTTP_PROXY` / `HTTPS_PROXY`

## 常用验证命令

```bash
# 检查完整配置加载
emacs --batch -l init.el --eval "(message \"Config loaded successfully\")"

# Byte compile 单个文件
emacs --batch -f batch-byte-compile lisp/core/init-config.el

# Byte compile 全部一级 lisp 文件
emacs --batch -f batch-byte-compile lisp/*.el

# 测试 init.el 基础加载
emacs --batch -l init.el --eval "(message \"All modules loaded\")"
```

## Tree-sitter

RivenEmacs 使用 `treesit-auto` 管理 Tree-sitter mode remap 和 grammar 安装提示。

手动安装 grammar：

```elisp
M-x auto-install-treesit-grammars
M-x install-essential-treesit-grammars
```

排障时优先检查：

1. grammar 源是否可访问。
2. `git`、`gcc`、`make` 等构建工具是否已安装。
3. 是否只需要先运行 `M-x install-essential-treesit-grammars` 安装最小基础集合。

## 设计取向

RivenEmacs 的目标不是堆叠插件，而是把 Emacs 组织成一个长期可演进的开发系统：启动路径短、模块职责清楚、重型能力延迟加载、外部依赖可检查、AI 能力可选接入。这样既能保持日常编辑的响应速度，也能在需要时进入完整 IDE 和 AI 辅助开发模式。
