#!/usr/bin/env bash
set -euo pipefail

SCRIPT_NAME="$(basename "$0")"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OS="$(uname -s)"

if [[ "$OS" == "Darwin" ]]; then
  PLATFORM="macos"
elif [[ "$OS" == "Linux" ]]; then
  PLATFORM="ubuntu"
else
  echo "[ERROR] Unsupported OS: $OS"
  exit 1
fi

log() { echo "[INFO] $*"; }
ok() { echo "[ OK ] $*"; }
warn() { echo "[WARN] $*"; }
err() { echo "[ERR ] $*" >&2; }

has_cmd() { command -v "$1" >/dev/null 2>&1; }

ENV_SPECS=(
  "DEFAULT_WORKSPACE|recommended|0|Default project workspace root"
  "DEEPSEEK_API_KEY|recommended|1|gptel default DeepSeek backend"
  "GROQ_API_KEY|optional|1|Optional Groq model backend"
  "ANTHROPIC_AUTH_TOKEN|optional|1|Anthropic API token for integrations"
  "ANTHROPIC_BASE_URL|optional|0|Anthropic custom API endpoint"
  "OPENAI_API_KEY|optional|1|OpenAI key used by some agent tools"
  "BRAVE_API_KEY|optional|1|Brave Search MCP API key"
  "TAVILY_API_KEY|optional|1|Tavily MCP API key"
  "HTTP_PROXY|optional|0|HTTP proxy URL"
  "HTTPS_PROXY|optional|0|HTTPS proxy URL"
)

usage() {
  cat <<'EOF'
Usage:
  scripts/riven-deps.sh install [--core] [--lsp] [--ai] [--mcp] [--markdown] [--spell] [--extra] [--all]
  scripts/riven-deps.sh doctor
  scripts/riven-deps.sh fix [--core] [--lsp] [--ai] [--mcp] [--markdown] [--spell] [--extra] [--all] [--deps-only] [--env-only] [--profile FILE]
  scripts/riven-deps.sh list

Commands:
  install   Install dependencies for current platform (Ubuntu/macOS).
  doctor    Check dependencies, environment variables, and batch init load.
  fix       Auto-install missing dependencies and prompt for missing env vars.
  list      Print dependency groups detected from this config.

Install groups:
  --core      Core CLI tools required by this config
  --lsp       Language server related toolchain
  --ai        Agent CLI dependencies
  --mcp       MCP server/tool dependencies for gptel
  --markdown  Markdown live preview dependency (go-grip)
  --spell     Spell-check dependencies for jinx (enchant/hunspell)
  --extra     Optional quality-of-life tools (docker/clangd)
  --all       Install all groups

Notes:
  - install default groups: --core --lsp
  - fix default groups: --core --lsp --ai --mcp --markdown --spell
EOF
}

run_cmd() {
  log "$*"
  "$@"
}

ensure_brew() {
  if ! has_cmd brew; then
    err "Homebrew is required on macOS. Install from https://brew.sh/"
    exit 1
  fi
}

ensure_apt() {
  if ! has_cmd apt-get; then
    err "apt-get not found. This script currently targets Ubuntu/Debian Linux."
    exit 1
  fi
}

brew_install() {
  local pkg="$1"
  if brew list --formula "$pkg" >/dev/null 2>&1 || brew list --cask "$pkg" >/dev/null 2>&1; then
    ok "$pkg already installed"
  else
    run_cmd brew install "$pkg"
  fi
}

brew_install_cask() {
  local pkg="$1"
  if brew list --cask "$pkg" >/dev/null 2>&1; then
    ok "$pkg cask already installed"
  else
    run_cmd brew install --cask "$pkg"
  fi
}

apt_install() {
  local pkg="$1"
  if dpkg -s "$pkg" >/dev/null 2>&1; then
    ok "$pkg already installed"
  else
    run_cmd sudo apt-get install -y "$pkg"
  fi
}

npm_global_install() {
  local pkg="$1"
  if npm list -g --depth=0 "$pkg" >/dev/null 2>&1; then
    ok "npm package already installed: $pkg"
  else
    run_cmd npm install -g "$pkg"
  fi
}

pip_user_install() {
  local pkg="$1"
  if python3 -m pip show "$pkg" >/dev/null 2>&1; then
    ok "python package already installed: $pkg"
  else
    run_cmd python3 -m pip install --user "$pkg"
  fi
}

install_core() {
  log "Installing core dependencies..."
  if [[ "$PLATFORM" == "macos" ]]; then
    ensure_brew
    brew_install emacs
    brew_install git
    brew_install ripgrep
    brew_install fd
    brew_install node
    brew_install python
    brew_install zstd
    brew_install cmake
    brew_install pkg-config
    brew_install libtool
  else
    ensure_apt
    run_cmd sudo apt-get update
    apt_install emacs-nox
    apt_install git
    apt_install ripgrep
    apt_install fd-find
    apt_install nodejs
    apt_install npm
    apt_install python3
    apt_install python3-pip
    apt_install zstd
    apt_install cmake
    apt_install pkg-config
    apt_install build-essential
    apt_install libtool
    if ! has_cmd fd && has_cmd fdfind; then
      mkdir -p "$HOME/.local/bin"
      ln -sf "$(command -v fdfind)" "$HOME/.local/bin/fd"
      ok "Created fd symlink -> fdfind at ~/.local/bin/fd"
      warn "If needed, add ~/.local/bin to PATH"
    fi
  fi
}

install_lsp() {
  log "Installing LSP/tooling dependencies..."
  if ! has_cmd npm; then
    warn "npm not found. Please run install --core first."
    return
  fi

  npm_global_install typescript
  npm_global_install typescript-language-server
  npm_global_install vscode-langservers-extracted
  npm_global_install @vue/language-server
  npm_global_install vtsls
  npm_global_install @tailwindcss/language-server
  npm_global_install pyright
  npm_global_install @fsouza/prettierd

  if has_cmd python3; then
    pip_user_install ruff
  else
    warn "python3 not found; skip ruff install"
  fi

  if has_cmd emacs-lsp-booster; then
    ok "emacs-lsp-booster already installed"
  elif has_cmd cargo; then
    run_cmd cargo install --locked emacs-lsp-booster
  else
    warn "cargo not found; skip emacs-lsp-booster install (needed for eglot acceleration)"
  fi

  if has_cmd rustup; then
    run_cmd rustup component add rust-analyzer || true
  elif [[ "$PLATFORM" == "macos" ]]; then
    brew_install rust-analyzer
  else
    apt_install rust-analyzer || warn "Failed to install rust-analyzer from apt; consider rustup."
  fi

  if [[ "$PLATFORM" == "macos" ]]; then
    brew_install llvm
  else
    apt_install clangd
  fi
}

install_ai() {
  log "Installing AI agent dependencies..."
  if has_cmd npm; then
    npm_global_install @blowmage/cursor-agent-acp
  else
    warn "npm not found; skip cursor-agent-acp"
  fi

  if [[ "$PLATFORM" == "macos" ]]; then
    ensure_brew
    brew_install_cask claude-code
    run_cmd brew tap anomalyco/tap || true
    brew_install opencode
  else
    warn "claude-code/opencode auto-install is macOS-only in current config."
  fi
}

install_mcp() {
  log "Installing MCP dependencies..."
  if has_cmd npm; then
    npm_global_install @modelcontextprotocol/server-filesystem
    npm_global_install @modelcontextprotocol/server-memory
    npm_global_install @modelcontextprotocol/server-sequential-thinking
    npm_global_install @modelcontextprotocol/server-everything
    npm_global_install @playwright/mcp
    npm_global_install @modelcontextprotocol/server-brave-search
    npm_global_install tavily-mcp
  else
    warn "npm not found; skip npm-based MCP servers"
  fi

  if has_cmd python3; then
    if ! has_cmd uvx; then
      run_cmd python3 -m pip install --user uv
    else
      ok "uvx already installed"
    fi

    if has_cmd browser-use && browser-use --help >/dev/null 2>&1; then
      ok "browser-use CLI already installed"
    else
      run_cmd python3 -m pip install --user "browser-use[cli]"
    fi
  else
    warn "python3 not found; skip browser-use/uv installation"
  fi
}

install_markdown() {
  log "Installing markdown preview dependency..."
  if has_cmd go; then
    run_cmd go install github.com/chrishrb/go-grip@latest
  else
    warn "go not found; install Go first to use go-grip."
  fi
}

install_spell() {
  log "Installing spell-check dependencies for jinx..."
  if [[ "$PLATFORM" == "macos" ]]; then
    ensure_brew
    brew_install enchant
    brew_install hunspell
  else
    ensure_apt
    run_cmd sudo apt-get update
    apt_install enchant-2
    apt_install hunspell
    apt_install hunspell-en-us
  fi
}

install_extra() {
  log "Installing optional extra dependencies..."
  if [[ "$PLATFORM" == "macos" ]]; then
    ensure_brew
    brew_install docker
  else
    apt_install docker.io
  fi
}

check_cmd() {
  local cmd="$1"
  local level="$2"
  local desc="$3"

  if has_cmd "$cmd"; then
    ok "$cmd ($desc)"
    return 0
  fi

  if [[ "$level" == "required" ]]; then
    err "$cmd missing ($desc)"
    return 1
  fi

  warn "$cmd missing ($desc)"
  return 0
}

check_env_var() {
  local var_name="$1"
  local level="$2"
  local desc="$3"
  local val="${!var_name:-}"

  if [[ -n "$val" ]]; then
    ok "$var_name set ($desc)"
    return 0
  fi

  case "$level" in
    required)
      err "$var_name missing ($desc)"
      return 1
      ;;
    recommended)
      warn "$var_name missing ($desc)"
      ;;
    *)
      warn "$var_name missing ($desc)"
      ;;
  esac
  return 0
}

check_emacs_runtime() {
  local log_file="/tmp/riven-emacs-doctor.log"

  if ! has_cmd emacs; then
    err "emacs missing; skip runtime check"
    return 1
  fi

  if [[ ! -f "$REPO_ROOT/init.el" ]]; then
    err "init.el not found at $REPO_ROOT"
    return 1
  fi

  if emacs --batch -l "$REPO_ROOT/init.el" --eval "(message \"RivenEmacs doctor init ok\")" \
    >"$log_file" 2>&1; then
    ok "Emacs batch init check passed"
    return 0
  fi

  err "Emacs batch init check failed"
  warn "Last 25 log lines from $log_file:"
  tail -n 25 "$log_file" || true
  return 1
}

doctor() {
  local required_failed=0
  echo "== RivenEmacs dependency doctor ($PLATFORM) =="

  echo
  echo "[Core - required]"
  check_cmd emacs required "Run RivenEmacs configuration" || required_failed=1
  check_cmd git required "Magit/diff-hl/version control" || required_failed=1
  check_cmd rg required "consult-ripgrep/deadgrep search backend" || required_failed=1
  if has_cmd fd || has_cmd fdfind; then
    ok "fd/fdfind (consult-fd backend)"
  else
    err "fd/fdfind missing (consult-fd backend)"
    required_failed=1
  fi
  check_cmd node required "JS/TS ecosystem" || required_failed=1
  check_cmd npm required "Global language servers and MCP packages" || required_failed=1
  check_cmd python3 required "Python tooling and integrations" || required_failed=1

  echo
  echo "[LSP - recommended]"
  check_cmd emacs-lsp-booster optional "Eglot JSON-RPC acceleration wrapper"
  check_cmd ruff optional "Python lint target in eglot workflows"
  check_cmd pyright-langserver optional "Pyright backend for Python"
  check_cmd typescript-language-server optional "TypeScript language server"
  check_cmd vscode-eslint-language-server optional "ESLint language server"
  check_cmd vue-language-server optional "Vue language server"
  check_cmd vtsls optional "TypeScript/Vue server"
  check_cmd tailwindcss-language-server optional "Tailwind language server"
  check_cmd rust-analyzer optional "Rust language server"
  check_cmd clangd optional "C/C++ language server"

  echo
  echo "[AI Agent - optional]"
  check_cmd cursor-agent-acp optional "Cursor ACP agent CLI"
  check_cmd claude optional "Claude Code CLI"
  check_cmd opencode optional "OpenCode CLI"

  echo
  echo "[MCP - recommended]"
  check_cmd mcp-server-filesystem optional "Filesystem MCP server"
  check_cmd mcp-server-memory optional "Memory MCP server"
  check_cmd mcp-server-sequential-thinking optional "Sequential Thinking MCP server"
  check_cmd mcp-server-everything optional "Everything MCP server"
  check_cmd playwright-mcp optional "Playwright browser MCP server"
  check_cmd browser-use optional "browser-use MCP capability"
  check_cmd uvx optional "browser-use MCP launcher"
  check_cmd mcp-server-brave-search optional "Brave Search MCP server"
  check_cmd tavily-mcp optional "Tavily MCP server"

  echo
  echo "[Feature-specific optional]"
  check_cmd go-grip optional "Markdown grip-mode live preview backend"
  check_cmd go optional "Install go-grip automatically"
  if has_cmd enchant-2 || has_cmd enchant-lsmod-2 || has_cmd enchant; then
    ok "enchant backend (jinx spell-check backend)"
  else
    warn "enchant backend missing (jinx spell-check backend)"
  fi
  check_cmd hunspell optional "Dictionary backend for jinx/enchant"
  check_cmd zstd optional "undo-fu-session compression optimization"
  check_cmd docker optional "docker.el workflows"
  check_cmd cmake optional "tree-sitter/vterm native builds"
  check_cmd make optional "tree-sitter/vterm native builds"
  check_cmd gcc optional "tree-sitter grammar compilation"

  if [[ "$PLATFORM" == "macos" ]]; then
    check_cmd sourcekit-lsp optional "Swift language server"
    check_cmd xcrun optional "sourcekit-lsp path fallback"
  else
    check_cmd xdg-open optional "Open file explorer integration"
  fi

  echo
  echo "[Environment variables]"
  local spec var_name level secret desc
  for spec in "${ENV_SPECS[@]}"; do
    IFS='|' read -r var_name level secret desc <<< "$spec"
    check_env_var "$var_name" "$level" "$desc" || required_failed=1
  done

  echo
  echo "[Emacs runtime]"
  check_emacs_runtime || required_failed=1

  echo
  if [[ "$required_failed" -ne 0 ]]; then
    err "Doctor finished: missing required dependencies or runtime check failed."
    echo "Try: scripts/riven-deps.sh fix"
    exit 1
  fi

  ok "Doctor finished: required dependencies are installed and runtime check passed."
}

list_deps() {
  cat <<'EOF'
Detected dependency groups (from current config):

core:
  emacs, git, rg(ripgrep), fd/fdfind, node, npm, python3

lsp/tooling:
  emacs-lsp-booster, ruff, pyright-langserver, typescript-language-server,
  vscode-eslint-language-server, vue-language-server, vtsls,
  tailwindcss-language-server, rust-analyzer, clangd

ai agents:
  cursor-agent-acp, claude, opencode

mcp:
  mcp-server-filesystem, mcp-server-memory,
  mcp-server-sequential-thinking, mcp-server-everything,
  playwright-mcp, browser-use, uvx,
  mcp-server-brave-search, tavily-mcp

spell:
  enchant-2(or enchant-lsmod-2/enchant), hunspell

environment variables:
  DEFAULT_WORKSPACE, DEEPSEEK_API_KEY, GROQ_API_KEY,
  ANTHROPIC_AUTH_TOKEN, ANTHROPIC_BASE_URL,
  OPENAI_API_KEY, BRAVE_API_KEY, TAVILY_API_KEY,
  HTTP_PROXY, HTTPS_PROXY

feature-specific:
  go-grip, go, docker, zstd, cmake/make/gcc, sourcekit-lsp(macOS)
EOF
}

run_install_groups() {
  local want_core="$1"
  local want_lsp="$2"
  local want_ai="$3"
  local want_mcp="$4"
  local want_markdown="$5"
  local want_spell="$6"
  local want_extra="$7"

  (( want_core )) && install_core
  (( want_lsp )) && install_lsp
  (( want_ai )) && install_ai
  (( want_mcp )) && install_mcp
  (( want_markdown )) && install_markdown
  (( want_spell )) && install_spell
  (( want_extra )) && install_extra

  ok "Install steps finished."
}

install_main() {
  local want_core=0
  local want_lsp=0
  local want_ai=0
  local want_mcp=0
  local want_markdown=0
  local want_spell=0
  local want_extra=0

  if [[ $# -eq 0 ]]; then
    want_core=1
    want_lsp=1
  else
    while [[ $# -gt 0 ]]; do
      case "$1" in
        --core) want_core=1 ;;
        --lsp) want_lsp=1 ;;
        --ai) want_ai=1 ;;
        --mcp) want_mcp=1 ;;
        --markdown) want_markdown=1 ;;
        --spell) want_spell=1 ;;
        --extra) want_extra=1 ;;
        --all)
          want_core=1
          want_lsp=1
          want_ai=1
          want_mcp=1
          want_markdown=1
          want_spell=1
          want_extra=1
          ;;
        *)
          err "Unknown install option: $1"
          usage
          exit 1
          ;;
      esac
      shift
    done
  fi

  run_install_groups "$want_core" "$want_lsp" "$want_ai" "$want_mcp" "$want_markdown" "$want_spell" "$want_extra"
}

shell_single_quote() {
  local value="$1"
  value=${value//\'/\'\"\'\"\'}
  printf "'%s'" "$value"
}

upsert_env_export() {
  local file="$1"
  local var_name="$2"
  local value="$3"
  local quoted
  local line
  local tmp_file

  quoted="$(shell_single_quote "$value")"
  line="export ${var_name}=${quoted}"

  mkdir -p "$(dirname "$file")"
  touch "$file"

  tmp_file="$(mktemp)"
  awk -v var_name="$var_name" -v line="$line" '
    BEGIN { updated = 0 }
    $0 ~ "^[[:space:]]*export[[:space:]]+" var_name "=" {
      if (!updated) {
        print line
        updated = 1
      }
      next
    }
    { print }
    END {
      if (!updated) print line
    }
  ' "$file" > "$tmp_file"
  mv "$tmp_file" "$file"
}

detect_profile_file() {
  if [[ -n "${RIVENEMACS_ENV_FILE:-}" ]]; then
    printf "%s" "$RIVENEMACS_ENV_FILE"
    return
  fi

  case "$(basename "${SHELL:-}")" in
    zsh) printf "%s" "$HOME/.zshrc" ;;
    bash) printf "%s" "$HOME/.bashrc" ;;
    *) printf "%s" "$HOME/.profile" ;;
  esac
}

prompt_and_set_env_var() {
  local profile_file="$1"
  local var_name="$2"
  local level="$3"
  local secret="$4"
  local desc="$5"
  local default_value=""
  local prompt_value=""

  if [[ -n "${!var_name:-}" ]]; then
    ok "$var_name already set in current shell"
    return
  fi

  if [[ "$var_name" == "DEFAULT_WORKSPACE" ]]; then
    default_value="$HOME"
  fi

  if [[ "$secret" == "1" ]]; then
    if [[ -n "$default_value" ]]; then
      read -r -s -p "$var_name ($desc) [default: $default_value, Enter to use default, blank to skip]: " prompt_value
    else
      read -r -s -p "$var_name ($desc) [blank to skip]: " prompt_value
    fi
    echo
  else
    if [[ -n "$default_value" ]]; then
      read -r -p "$var_name ($desc) [default: $default_value, Enter to use default, blank to skip]: " prompt_value
    else
      read -r -p "$var_name ($desc) [blank to skip]: " prompt_value
    fi
  fi

  if [[ -z "$prompt_value" && -n "$default_value" ]]; then
    prompt_value="$default_value"
  fi

  if [[ -z "$prompt_value" ]]; then
    if [[ "$level" == "recommended" || "$level" == "required" ]]; then
      warn "Skipped $var_name"
    else
      log "Skipped optional $var_name"
    fi
    return
  fi

  upsert_env_export "$profile_file" "$var_name" "$prompt_value"
  export "${var_name}=${prompt_value}"
  ok "Set $var_name in $profile_file"
}

fix_env_vars() {
  local profile_file="$1"

  if [[ ! -t 0 ]]; then
    warn "No interactive TTY detected; skip env var prompt setup."
    warn "Set variables manually, or rerun in interactive shell."
    return
  fi

  log "Environment variable fix target: $profile_file"
  local spec var_name level secret desc
  for spec in "${ENV_SPECS[@]}"; do
    IFS='|' read -r var_name level secret desc <<< "$spec"
    prompt_and_set_env_var "$profile_file" "$var_name" "$level" "$secret" "$desc"
  done

  ok "Environment variable setup finished."
  echo "Run this to reload current shell config:"
  echo "  source $profile_file"
}

fix_main() {
  local want_core=0
  local want_lsp=0
  local want_ai=0
  local want_mcp=0
  local want_markdown=0
  local want_spell=0
  local want_extra=0
  local do_deps=1
  local do_env=1
  local profile_file=""
  local group_selected=0

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --core) want_core=1; group_selected=1 ;;
      --lsp) want_lsp=1; group_selected=1 ;;
      --ai) want_ai=1; group_selected=1 ;;
      --mcp) want_mcp=1; group_selected=1 ;;
      --markdown) want_markdown=1; group_selected=1 ;;
      --spell) want_spell=1; group_selected=1 ;;
      --extra) want_extra=1; group_selected=1 ;;
      --all)
        want_core=1
        want_lsp=1
        want_ai=1
        want_mcp=1
        want_markdown=1
        want_spell=1
        want_extra=1
        group_selected=1
        ;;
      --deps-only) do_env=0 ;;
      --env-only) do_deps=0 ;;
      --profile)
        shift
        if [[ $# -eq 0 ]]; then
          err "--profile requires a file path"
          exit 1
        fi
        profile_file="$1"
        ;;
      *)
        err "Unknown fix option: $1"
        usage
        exit 1
        ;;
    esac
    shift
  done

  if (( do_deps )); then
    if (( ! group_selected )); then
      want_core=1
      want_lsp=1
      want_ai=1
      want_mcp=1
      want_markdown=1
      want_spell=1
    fi

    run_install_groups "$want_core" "$want_lsp" "$want_ai" "$want_mcp" "$want_markdown" "$want_spell" "$want_extra"
  fi

  if (( do_env )); then
    if [[ -z "$profile_file" ]]; then
      profile_file="$(detect_profile_file)"
    fi
    fix_env_vars "$profile_file"
  fi

  log "Running final doctor check..."
  doctor
}

main() {
  if [[ $# -lt 1 ]]; then
    usage
    exit 1
  fi

  local cmd="$1"
  shift

  case "$cmd" in
    install) install_main "$@" ;;
    doctor) doctor ;;
    fix) fix_main "$@" ;;
    list) list_deps ;;
    -h|--help|help) usage ;;
    *)
      err "Unknown command: $cmd"
      usage
      exit 1
      ;;
  esac
}

main "$@"
