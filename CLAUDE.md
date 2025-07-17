# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**RivenEmacs** is a highly modular and performance-optimized Emacs configuration designed for modern development workflows. It features lazy loading, tree-sitter integration, AI capabilities, and comprehensive language support.

## Architecture

### Core Structure
- **early-init.el**: Performance optimizations and early initialization
- **init.el**: Main configuration loader with modular design
- **lisp/**: Modular configuration files organized by functionality
- **local/**: User-specific data (cache, bookmarks, sessions)
- **elpa/**: Package installation directory

### Key Modules
- **init-config.el**: Centralized configuration management with defcustom variables
- **init-use-package.el**: Package management setup
- **init-lsp-bridge.el**: LSP integration with lsp-bridge
- **init-gpt.el**: AI integration (GPTel, Aider, Claude Code)
- **init-keybindings.el**: Global keybindings using general.el
- **init-terminal.el**: Terminal integration (eat, vterm)

## Development Commands

### Emacs Configuration Commands
```bash
# Start Emacs with this config
emacs

# Start Emacs without loading custom.el (clean state)
emacs -q -l ~/.emacs.d/init.el

# Byte compile all configuration files
emacs --batch -f batch-byte-compile lisp/*.el

# Check configuration syntax
emacs --batch -l init.el --eval "(message \"Config loaded successfully\")"
```

### Package Management
- **Install packages**: Automatic via use-package on startup
- **Update packages**: `M-x package-list-packages` then `U x`
- **Clean packages**: `M-x package-autoremove`

### Tree-sitter Grammar Management
```elisp
;; Install tree-sitter grammars
M-x install-essential-treesit-grammars  ;; Core languages
M-x auto-install-treesit-grammars       ;; All supported grammars

;; Configure auto-installation
(setq rivenEmacs-auto-install-treesit t)
```

### LSP Configuration
- **Language servers**: Configured via lsp-bridge
- **Python**: Uses "ruff" and "pyright_ruff" multi-server
- **JavaScript/TypeScript**: Uses "typescript_eslint" and "volar_emmet"
- **Rust**: Uses rust-analyzer

## Environment Configuration

### Environment Variables
- `DEFAULT_WORKSPACE`: Default project directory (default: `~/`)
- `GROQ_API_KEY`: API key for Groq AI services
- `DEEPSEEK_API_KEY`: API key for DeepSeek AI services
- `ANTHROPIC_AUTH_TOKEN`: API key for Claude AI services
- `HTTP_PROXY`/`HTTPS_PROXY`: Network proxy configuration

### Proxy Setup
```elisp
;; Enable proxy
(setq rivenEmacs-use-proxy t)
;; Or use environment variables
(setenv "HTTP_PROXY" "127.0.0.1:7890")
```

## Key Bindings

### Global Leader Key: `SPC`
- **SPC b**: Buffer management
- **SPC c**: Code actions (LSP)
- **SPC p**: Project navigation
- **SPC f**: Find files
- **SPC g**: Git operations
- **SPC n**: Org-mode notes
- **SPC k**: Kill/delete operations
- **SPC !**: Flymake diagnostics

### Global Bindings
- `M-.`: Go to definition (lsp-bridge)
- `M-,`: Return from definition
- `M-?`: Find references
- `C-x o t`: Terminal (eat)
- `C-x o d`: Docker
- `M-*`: Claude Code

### AI Integration
- **SPC a a**: Aider AI assistant
- **SPC a c**: GPTel console
- **SPC a g**: GPTel menu
- **SPC a t**: AI translation
- **SPC a r**: AI refactoring

## Language Support

### Web Development
- HTML, CSS, JavaScript, TypeScript
- Vue, React, Tailwind CSS
- ESLint integration

### Systems Programming
- Rust (rust-analyzer)
- C/C++ (clangd)
- Python (ruff, pyright)

### Configuration Files
- JSON, YAML, TOML
- Docker, docker-compose
- EditorConfig

## Customization

### Adding New Languages
1. Install language server
2. Add mode to `rivenEmacs-lsp-modes` in init-config.el
3. Configure lsp-bridge server in init-lsp-bridge.el

### Adding Keybindings
- Use general.el syntax in init-keybindings.el
- Follow existing patterns with leader keys

### Custom Variables
- Use `M-x customize-group RET rivenEmacs RET`
- Or set directly: `(setq rivenEmacs-default-workspace "~/projects/")`

## Performance Tuning

### Startup Optimization
- Lazy loading via use-package
- Deferred package initialization
- Native compilation cache in `local/eln/`

### Memory Management
- Aggressive garbage collection during startup
- Restored to normal after initialization
- Configurable via early-init.el

## Troubleshooting

### Common Issues
1. **Tree-sitter errors**: Check network connectivity and git authentication
2. **LSP not starting**: Verify language server installation
3. **Slow startup**: Check for package conflicts or large init files
4. **Proxy issues**: Configure rivenEmacs-use-proxy and environment variables

### Debug Commands
```elisp
;; Check configuration
M-x describe-variable RET rivenEmacs-auto-install-treesit RET

;; Check LSP status
M-x lsp-bridge-diagnostics

;; Check package status
M-x list-packages
```

## Testing

### Configuration Validation
```bash
# Test basic load
emacs --batch --eval "(load \"init.el\")" --eval "(message \"Success\")"

# Test specific module
emacs --batch --eval "(require 'init-config)"
```

### Package Testing
- Use `emacs -Q` for clean environment testing
- Test individual modules with `(require 'module-name)`