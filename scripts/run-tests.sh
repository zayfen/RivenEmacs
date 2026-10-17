#!/usr/bin/env bash
# RivenEmacs offline test runner.
#
# Runs the ERT suites for the configuration and for the bundled
# emacs-codeforces library without loading init.el and without network
# access.  Every directory needed on `load-path' is added automatically, so
# tests can be started from any working directory.
#
# Usage:
#   scripts/run-tests.sh                 # all project + emacs-codeforces ERT,
#                                        # then the Python agent unit tests
#   scripts/run-tests.sh tests/test-init-terminal.el
#   scripts/run-tests.sh emacs-codeforces/test/emacs-codeforces-auth-test.el
#   scripts/run-tests.sh --no-python     # ERT only
#
# Environment:
#   EMACS                            Emacs executable to use (default: emacs).
#   RIVENEMACS_RUN_INIT_TESTS=1      Also run the tests that start a nested
#                                    `emacs -l init.el' (package installs and
#                                    network access may happen; not the default).
#   RIVENEMACS_SKIP_PYTHON_TESTS=1   Skip the Python agent unit tests.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

EMACS_BIN="${EMACS:-emacs}"
run_python=1
test_args=()

usage() {
  sed -n '4,25p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)
      usage
      exit 0
      ;;
    --no-python)
      run_python=0
      ;;
    -*)
      echo "unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
    *)
      test_args+=("$1")
      ;;
  esac
  shift
done

# Build the load-path exactly once so every test loads the repository sources.
load_path=()
add_load_path() {
  if [[ -n "${1:-}" && -d "$1" ]]; then
    load_path+=("-L" "$1")
  fi
}
add_load_path "$REPO_ROOT"
add_load_path "$REPO_ROOT/lisp"
while IFS= read -r -d '' dir; do
  # Match init.el: the user-local "lisp/local" tree is not on load-path.
  case "$dir" in
    */lisp/local|*/lisp/local/*) continue ;;
  esac
  add_load_path "$dir"
done < <(find "$REPO_ROOT/lisp" -type d -print0)
add_load_path "$REPO_ROOT/emacs-codeforces"
add_load_path "$REPO_ROOT/tests"

# Normalize test file arguments (bare names are looked up in the usual dirs).
resolve_test_file() {
  local file="$1"
  if [[ -f "$file" ]]; then
    printf '%s\n' "$file"
    return 0
  fi
  local candidate
  for prefix in "" "tests/" "emacs-codeforces/test/"; do
    candidate="$REPO_ROOT/$prefix$file"
    if [[ -f "$candidate" ]]; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done
  return 1
}

test_files=()
if [[ ${#test_args[@]} -eq 0 ]]; then
  shopt -s nullglob
  for file in "$REPO_ROOT"/tests/test-*.el; do
    test_files+=("$file")
  done
  for file in "$REPO_ROOT"/emacs-codeforces/test/emacs-codeforces-*-test.el; do
    test_files+=("$file")
  done
  shopt -u nullglob
else
  for file in "${test_args[@]}"; do
    if ! resolved="$(resolve_test_file "$file")"; then
      echo "test file not found: $file" >&2
      exit 2
    fi
    test_files+=("$resolved")
  done
fi

if [[ ${#test_files[@]} -eq 0 ]]; then
  echo "no test files found" >&2
  exit 2
fi

load_args=()
for file in "${test_files[@]}"; do
  load_args+=(-l "$file")
done

echo "== ERT: ${#test_files[@]} test file(s) =="
printf '   %s\n' "${test_files[@]}"

# The nested `emacs -l init.el' tests are opt-in: they are slow and may install
# packages over the network.  Everything else runs against the checked-in
# sources (`load-prefer-newer t` avoids stale .elc files in the worktree).
emacs_args=(
  -Q --batch
  --eval "(setq load-prefer-newer t)"
  "${load_path[@]}"
  --eval "(defun riven-run-tests-skip-init-p (test)
            (and (null (getenv \"RIVENEMACS_RUN_INIT_TESTS\"))
                 (memq (ert-test-name test)
                       '(riven/early-init-initializes-package-system
                         riven/direct-init-load-initializes-package-system
                         riven/c-x-d-opens-dirvish-on-first-use
                         riven/dired-prefers-homebrew-gls-outside-exec-path))))"
  "${load_args[@]}"
  --eval "(ert-run-tests-batch-and-exit '(not (satisfies riven-run-tests-skip-init-p)))"
)

"$EMACS_BIN" "${emacs_args[@]}"

if [[ "$run_python" -eq 1 && -z "${RIVENEMACS_SKIP_PYTHON_TESTS:-}" ]]; then
  if command -v python3 >/dev/null 2>&1; then
    echo
    echo "== Python: scripts/codeforces_agent.py unit tests =="
    # PYTHONDONTWRITEBYTECODE keeps __pycache__ out of the repository.
    PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover \
      -s "$REPO_ROOT/emacs-codeforces/test" -p 'test_*.py' -v
  else
    echo "[WARN] python3 not found; skipping Python agent unit tests" >&2
  fi
fi

echo
echo "== ALL TESTS PASSED =="
