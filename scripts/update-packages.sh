#!/usr/bin/env bash
# update-packages.sh --- upgrade ELPA and package-vc packages for RivenEmacs.
#
# Usage: scripts/update-packages.sh [OPTIONS]
# Run with --help for the full option list.
#
# The script loads early-init.el (package directories, archives and
# custom.el) in a batch Emacs session, so it uses the same package set as
# the interactive configuration, then runs scripts/update-packages.el.
# It never loads init.el and never starts the Emacs configuration itself.

set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
EMACS_BIN=${EMACS:-emacs}

if ! command -v "$EMACS_BIN" >/dev/null 2>&1; then
  echo "error: Emacs executable not found: $EMACS_BIN" >&2
  echo "       Set EMACS=/path/to/emacs to choose another binary." >&2
  exit 127
fi

if ! command -v git >/dev/null 2>&1; then
  echo "warning: git not found; VC package updates will fail." >&2
fi

if [ ! -f "$ROOT_DIR/early-init.el" ] || [ ! -f "$ROOT_DIR/scripts/update-packages.el" ]; then
  echo "error: cannot locate RivenEmacs files under $ROOT_DIR" >&2
  exit 1
fi

# The updater reads its arguments from this environment variable.  Each
# argument is one newline-separated line; arguments themselves never contain
# newlines (they are option names and package names).
export RIVENEMACS_UPDATE_ARGS
RIVENEMACS_UPDATE_ARGS=$(printf '%s\n' "$@")

exec "$EMACS_BIN" --batch -Q \
  --eval "(setq load-prefer-newer t)" \
  --eval "(setq riven/update-packages-auto-run t)" \
  -l "$ROOT_DIR/early-init.el" \
  -l "$ROOT_DIR/scripts/update-packages.el"
