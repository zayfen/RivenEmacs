# RivenEmacs

A highly modular and performance-optimized Emacs configuration designed for modern development workflows.

## Requirements

### Core Dependencies

- **Emacs 28+** with native compilation support
- **Python 3** for various language servers and tools
- **Node.js** for JavaScript/TypeScript development
- **Git** for version control integration

### Optional Dependencies

#### Markdown Preview
- `pip install grip` (recommended)
- Alternative: `cargo install mdopen` or `go install github.com/chrishrb/go-grip@latest`

#### macOS Specific
- `brew install coreutils` (for enhanced dired functionality)

#### Language Servers
- **Python**: `pip install python-lsp-server` or `pip install pyright`
- **JavaScript/TypeScript**: `npm install -g typescript-language-server`
- **Rust**: `rustup component add rust-analyzer`
- **C/C++**: Install `clangd`

## Configuration

### Environment Variables

You can customize RivenEmacs behavior using environment variables:

- `DEFAULT_WORKSPACE`: Default workspace directory (default: `~/`)
- `HTTP_PROXY`/`http_proxy`: HTTP proxy server
- `HTTPS_PROXY`/`https_proxy`: HTTPS proxy server
- `GROQ_API_KEY`: API key for Groq AI services
- `DEEPSEEK_API_KEY`: API key for DeepSeek AI services

### Customization

Use `M-x customize-group RET rivenEmacs RET` to customize configuration through Emacs' built-in customization interface.

## Features

- **Performance Optimized**: Fast startup with lazy loading
- **Modular Design**: Easy to extend and customize
- **Modern LSP Integration**: Full IDE experience with lsp-bridge
- **AI Integration**: GPT integration for code assistance
- **Multiple Language Support**: Web, Rust, Python, and more
- **Tree-sitter Integration**: Enhanced syntax highlighting and parsing

## Tree-sitter Configuration

RivenEmacs includes tree-sitter support for enhanced syntax highlighting. By default, tree-sitter grammars are **not** automatically installed to avoid startup delays and network issues.

### Manual Installation

To install tree-sitter grammars manually:

```elisp
;; Install all supported grammars
M-x auto-install-treesit-grammars

;; Install only essential grammars (recommended)
M-x install-essential-treesit-grammars
```

### Automatic Installation

To enable automatic installation on startup:

```elisp
;; Through customize interface
M-x customize-group RET rivenEmacs RET
;; Set "rivenEmacs-auto-install-treesit" to t

;; Or add to your configuration
(setq rivenEmacs-auto-install-treesit t)
```

### Troubleshooting

If you encounter tree-sitter installation errors:

1. **Network Issues**: Ensure you have internet connectivity and can access GitHub
2. **Git Authentication**: Some repositories may require git authentication
3. **Missing Dependencies**: Ensure you have `git`, `gcc`, and build tools installed
4. **Selective Installation**: Use `M-x install-essential-treesit-grammars` for core languages only


