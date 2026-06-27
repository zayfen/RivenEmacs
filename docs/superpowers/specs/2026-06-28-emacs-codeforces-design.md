# emacs-codeforces — Design Spec

- **Date:** 2026-06-28
- **Branch:** `feature/codeforces`
- **Status:** Approved (pending implementation plan)
- **Scope:** Build `emacs-codeforces`, an independent Emacs Lisp library that
  integrates Codeforces competitive-programming workflow into Emacs, plus a
  thin RivenEmacs integration module.

## Goal

A single library, loaded with `(require 'emacs-codeforces)`, that lets a user:

1. Log in to Codeforces (session-cookie injection), creating `~/.emacs-codeforces/`.
2. Browse the problem list and search by problem tags.
3. Press `RET` on a problem to open its full statement (including tags) in a
   right-side window.
4. Press `C-c C-c` in the statement to start solving.
5. Submit the solution and watch real-time judge verdicts in the statement buffer.

## Non-Goals (YAGNI — deferred to later iterations)

- Local sample test running (`riven/cf-test` against sample I/O).
- Programmatic login (username/password POST) — blocked by hCaptcha.
- Contest scheduling / countdown / standings.
- Submitting to Gym / group contests (problemset + live contest only, initially).

## Background & Key Constraints

The Codeforces surface splits cleanly into two halves, and the architecture
reflects this split:

| Operation | Public JSON API (`codeforces.com/api/...`) | Website HTML (needs session cookie) |
|---|---|---|
| Problem list + tags + rating | ✅ `problemset.problems` (supports `tags=` filter) | — |
| Full problem statement | ❌ none | ✅ scrape `/problemset/problem/{cid}/{idx}` |
| Submit code | ❌ none | ⚠️ form POST, **Cloudflare-gated in 2026** |
| Submission status / verdict | ✅ `contest.status` / `user.status` | — |
| Login | ❌ no login API (hCaptcha) | ⚠️ `POST /enter` blocked by hCaptcha |

Two load-bearing realities:

1. **No programmatic login.** The `/enter` page has hCaptcha. Login is done by
   the user copying a browser session cookie into the library (same approach as
   `cf-tool` and `leetcode.el`).
2. **Cloudflare on submit (2026).** Codeforces added Cloudflare + AES challenge
   protection that broke legacy direct-HTTP submit (`cf-tool` now ships a
   "Browser Mode"). Direct Emacs HTTP POST of a solution may be challenged.
   Strategy: attempt HTTP POST, detect the challenge, fall back to opening the
   pre-filled submit page in the user's browser. The status poll (the
   "real-time result" half of requirement 5) goes through the public API and is
   **not** affected by Cloudflare, so requirement 5's visible outcome is reliable.

### Reference implementations consulted

- `oj.el` (MELPA) — Emacs front-end that shells out to `online-judge-tools` (Python). Useful for command/UX patterns; not a native API client.
- `leetcode.el` (MELPA) — cookie-based auth + interactive solve flow; best in-Emacs UX reference.
- `cf-tool` (Go CLI, archived) — Codeforces-specific submit/cookie mechanics.

## Architecture

### Project layout

`emacs-codeforces` is an **independent library** living at the RivenEmacs repo
root. It depends only on Emacs built-ins (`url`, `json`, `org`,
`tabulated-list`, `shr`/`libxml2`). It can be required standalone and is
MELPA-publishable. RivenEmacs integrates via a thin `use-package` wrapper.

```
RivenEmacs/
├── emacs-codeforces/                    # ← independent library
│   ├── emacs-codeforces.el              # entry: provide + autoload commands + defcustom
│   ├── emacs-codeforces-client.el       # HTTP layer: url.el + persistent cookie jar
│   ├── emacs-codeforces-auth.el         # session-cookie injection + validation + credential storage
│   ├── emacs-codeforces-api.el          # read-only JSON API (problems / tags / submissions)
│   ├── emacs-codeforces-scrape.el       # statement HTML→Org conversion + website submit (csrf+form)
│   ├── emacs-codeforces-workspace.el    # ~/.emacs-codeforces layout + language templates
│   ├── emacs-codeforces-list.el         # tabulated-list-mode problem list + tag filter
│   ├── emacs-codeforces-detail.el       # Org detail buffer + C-c C-c solve + polling
│   └── templates/                       # bundled language templates (rust.rs / python.py / cpp.cpp …)
└── lisp/tools/init-codeforces.el        # ← RivenEmacs integration (thin wrapper)
```

### Module dependency graph (acyclic)

```
emacs-codeforces (entry)
 ├── client      (HTTP/cookie foundation, no deps)
 ├── auth        (depends on client)
 ├── api         (depends on client, read-only JSON)
 ├── scrape      (depends on client + auth; statement + submit)
 ├── workspace   (pure local filesystem, no deps)
 ├── list        (depends on api)
 └── detail      (depends on api + scrape + workspace)
```

Each module is single-responsibility and target < 300 lines, matching
RivenEmacs AGENTS.md ("small and focused" modules).

## Data Model

All records are plists, consistent across the library.

```elisp
;; Problem — from problemset.problems
'(:contestId 1234 :index "A" :name "..." :rating 1200
  :tags ("dp" "greedy") :timeLimit 2.0 :memoryLimit 256)

;; Submission — from contest.status polling
'(:id 9876543 :verdict "TESTING" :passedTestCount 3
  :timeConsumedMillis 124 :memoryConsumedBytes 8388608)
```

### Field semantics (verified from apiHelp/objects — load-bearing)

- `index` may be multi-char (e.g. `"F2"`, `"A1"`).
- `timeLimit` is **seconds** (may be fractional, e.g. `0.25`, `2`, `3.5`).
- `memoryLimit` is **megabytes** (integer).
- `memoryConsumedBytes` is **bytes**.
- `passedTestCount` counts only the **final** testset (excludes pretests once on TESTS).
- `verdict` enum: `OK`, `WRONG_ANSWER`, `TIME_LIMIT_EXCEEDED`,
  `MEMORY_LIMIT_EXCEEDED`, `RUNTIME_ERROR`, `COMPILATION_ERROR`, `PARTIAL`,
  `IDLENESS_LIMIT_EXCEEDED`, `CHALLENGED`, `SKIPPED`, `REJECTED`, `FAILED`,
  `TESTING`. **Absent `verdict`** ⇒ still queued/compiling.
- `programTypeId` is **not stable** — must be scraped from the live submit
  page's `<select name="programTypeId">`. Never hardcode.

## Local Filesystem Layout (requirements 1 & 5)

```
~/.emacs-codeforces/
├── credentials                  # login state: cookie + handle (chmod 600)
├── cache/
│   └── problems.json            # local cache of problemset.problems (+ timestamp)
└── workspace/
    └── {contestId}_{index}/     # one dir per problem, e.g. 1234_A/
        ├── problem.org          # statement (HTML→Org, same renderer as detail)
        └── solution.rs          # source (initialized from language template)
```

> Note: the original request wrote `.emac-codeforces` (missing an `s`); the
> library name is `emacs-codeforces`, so the directory is `.emacs-codeforces`
> for consistency. This is the agreed name.

## Auth Flow (requirement 1) — session-cookie injection

Because `/enter` has hCaptcha, no programmatic login is attempted.

1. `M-x codeforces-login`:
   - Prompts in minibuffer to paste a session cookie copied from the browser
     (instructions tell the user where to copy from), defaulting to the
     clipboard contents.
   - Writes the cookie string to `~/.emacs-codeforces/credentials` with
     `set-file-modes` 0600.
2. **Validate**: with the injected cookie, `GET https://codeforces.com/enter`.
     The page shows "Logout" when authenticated, the login form when not.
     Detect "Logout" ⇒ valid.
3. On valid: create `~/.emacs-codeforces/` (requirement 1's "create directory on
     successful login"), echo the handle.
4. On invalid: error and prompt to re-supply the cookie. All network ops wrapped
     in `condition-case`.

**Credential storage**: `credentials` holds `handle` and the full `Cookie:`
header value (e.g. `JSESSIONID=...; BMH...; ...`). `emacs-codeforces-client.el`
loads this into the `Cookie` header of every website request. If a scrape later
gets the login form instead of expected content, the library prompts the user to
re-login.

## Core Interaction Flows

### Problem list (requirements 2 & 3) — `emacs-codeforces-list.el`

`codeforces-browse-problems` opens `*Codeforces Problems*`, a
`tabulated-list-mode` derivative:

- **Columns**: `ID` (e.g. `1234A`) · `Name` · `Rating` · `Tags` (comma-joined).
- **Data source**: `emacs-codeforces-api` fetches `problemset.problems`, caches
     locally (slow first run, instant after); `g` refreshes.
- **Sort**: click a column header toggles asc/desc (built into
     `tabulated-list-mode`). Default: `contestId+index` descending (newest first).
- **Tag search (requirement 2 core)**: key `/` → `codeforces-filter-by-tags`,
     uses `consult-completing-read-multiple` with candidates = union of all tags
     in the current set. Multi-select is **AND** semantics (problem must have
     every selected tag). Empty selection clears the filter. Composable with a
     Rating range filter (key `r`, input like `800-1500`).
- **Open detail (requirement 3)**: `RET` on a row calls the detail module,
     opens the statement in a **right-side window** via a `display-buffer`
     right-side rule. Tabulated-list `RET` == "select a problem and press Enter".

### Problem detail (requirement 3) — `emacs-codeforces-detail.el`

The detail buffer is an **Org mode** buffer rendered from the scraped statement:

```
* 1234A - Problem Name                      :codeforces:
:PROPERTIES:
:Rating: 1200
:Time-Limit: 2.0 s
:Memory-Limit: 256 MB
:URL: https://codeforces.com/problemset/problem/1234/A
:END:

** Tags
dp, greedy

** Statement
<HTML → Org: paragraphs, lists, $...$ and $$...$$ math preserved as LaTeX>

** Input / Output
<sample I/O as source blocks>

** Submission Status       ← updated in place during polling
*No active submission.*
```

- **HTML→Org conversion** (`emacs-codeforces-scrape.el`): fetch
     `/problemset/problem/{cid}/{idx}`, locate `<div class="problem-statement">`,
     parse with `libxml-parse-html-region` and a hand-written recursive
     converter: `<p>`→paragraph, `<ul>/<ol>`→list, `<pre>`→`#+begin_src` block,
     math `$...$` / `$$...$$` preserved verbatim (org renders as LaTeX;
     RivenEmacs `org-latex-preview` previews it).
- **Reuse RivenEmacs org beautification**: the detail buffer auto-enables
     `olivetti-mode`, `org-modern-mode`, `org-appear-mode` (if loaded),
     matching `init-org`.
- **Buffer name**: `*CF 1234A*`; one buffer per problem, multiple may coexist.

### Start solving (requirement 4) — `C-c C-c` in detail buffer

`C-c C-c` → `codeforces-start-solving`:

1. `completing-read` to pick a language (default Rust; candidates = languages
     present in `templates/`). Choice is remembered per problem on first pick.
2. `emacs-codeforces-workspace.el` creates
     `~/.emacs-codeforces/workspace/{contestId}_{index}/`.
3. Writes `solution.<ext>` from the selected language template (Rust →
     `solution.rs`, with `fn main()` + fast-IO boilerplate). **An existing
     solution file is never overwritten.**
4. Left window stays on the detail; right window `find-file-other-window` opens
     `solution.<ext>`, cursor positioned inside.
5. Detail buffer `:Workspace:` property records the solve directory; the
     headline title becomes `● 1234A - Solving`.

### Submit + real-time result (requirement 5) — `C-c C-s` in detail buffer

`C-c C-s` → `codeforces-submit`:

1. Reads the problem's `solution.<ext>` content as `source`.
2. **Language mapping**: a template-picked Rust solution submits as Rust. To
     avoid hardcoding `programTypeId`, GET the submit page and scrape
     `<select name="programTypeId">` `<option>`s, match by "label contains the
     language name" (e.g. contains `Rust`).
3. **Submit method** (Cloudflare strategy from §"Background"):
   - **HTTP POST first**: GET the submit page for `data-csrf`, POST
       `/problemset/submit?csrf_token=...` with `submittedProblemIndex` /
       `programTypeId` / `source` / `tabSize` + cookie.
   - **Detect challenge**: if the response is a Cloudflare challenge page
       (contains `Just a moment` / `cf-chl`, or status 403/503) → **fall back**:
       `browse-url` opens the pre-filled submit page; echo prompts "complete the
       submit in the browser; results are still tracked here".
   - **On success**: parse the submission id from the success/redirect page.
4. **Real-time status poll** (core; goes through the public API, unaffected by
   Cloudflare):
   - Once a submission id is known, `run-with-timer` polls every
       `codeforces-poll-interval` seconds (default **2**) calling
       `contest.status?contestId=X&asBulk=true` and matching the submission by
       id (public API, no signing, Cloudflare-immune).
   - The detail buffer's `** Submission Status` section updates **in place**:
       `TESTING 12/30 tests…` → final `OK ✅ (124ms, 8MB)` or
       `WRONG_ANSWER on test 14 ❌`.
   - When the verdict reaches a terminal state (`OK`/`WRONG_ANSWER`/
       `COMPILATION_ERROR`/…) or `TESTING` shows no change for
       `codeforces-poll-timeout` seconds (default **60**), **stop polling** and
       update the headline prefix icon (✅/❌).

## Public API (library surface; all `autoload`)

```elisp
;; Auth (requirement 1)
codeforces-login                    ; inject cookie + validate + create ~/.emacs-codeforces/
codeforces-logout                   ; clear credentials
codeforces-logged-in-p              ; → bool

;; Browse (requirements 2 & 3)
codeforces-browse-problems          ; open tabulated list
codeforces-filter-by-tags           ; `/` key, consult multi-select tags (AND)
codeforces-filter-by-rating         ; `r` key, "800-1500"
codeforces-open-problem             ; `RET`, right-side detail buffer

;; Solve (requirements 4 & 5)
codeforces-start-solving            ; C-c C-c (in detail buffer)
codeforces-submit                   ; C-c C-s (in detail buffer)
```

## Module responsibilities

| Module | Responsibility | Key functions |
|---|---|---|
| `client.el` | HTTP abstraction (url.el + cookie jar), sync fetch, returns `(status . body)`, unified timeout/proxy | `+cf-http-get`, `+cf-http-post`, `+cf-cookie-header` |
| `auth.el` | Credential store/load/validate, directory creation | `codeforces-login`, `codeforces-logged-in-p`, `+cf-load-credentials` |
| `api.el` | Read-only JSON API, plist conversion, cache | `+cf-fetch-problems`, `+cf-fetch-submission`, `+cf-cache-problems` |
| `scrape.el` | Statement HTML→Org converter; submit form (csrf+POST); programTypeId matching; Cloudflare detection + fallback | `+cf-fetch-problem-html`, `+cf-html-to-org`, `+cf-submit-via-http`, `+cf-submit-via-browser`, `+cf-parse-language-options` |
| `workspace.el` | Local dirs + language templates | `+cf-problem-dir`, `+cf-init-solution`, `+cf-language-template` |
| `list.el` | tabulated-list derivative mode + tag/rating filter | `codeforces-browse-problems`, `codeforces-filter-by-tags` |
| `detail.el` | Org detail buffer + keymap + polling | `codeforces-open-problem`, `codeforces-start-solving`, `codeforces-submit`, `+cf-render-detail`, `+cf-poll-submission` |
| `emacs-codeforces.el` | Entry: defcustom, autoload, require all submodules, provide | — |

## Configuration (`defcustom`, `codeforces` group)

```elisp
(defcustom codeforces-home-directory
  (expand-file-name "~/.emacs-codeforces/")
  "Codeforces workspace root."
  :type 'directory)

(defcustom codeforces-default-language "rust"
  "Default language for new solutions."
  :type '(choice (const "rust") (const "python") (const "cpp") (const "java")))

(defcustom codeforces-poll-interval 2
  "Seconds between submission status polls."
  :type 'number)

(defcustom codeforces-poll-timeout 60
  "Max seconds to poll a submission before giving up."
  :type 'number)

(defcustom codeforces-templates-directory nil
  "Override directory for language templates. Nil = bundled templates/."
  :type '(choice directory (const nil)))
```

## Error Handling (follows AGENTS.md `condition-case` pattern)

- **Network failure**: `condition-case` catches, echoes a specific error, never crashes.
- **Not logged in**: every cookie-requiring op checks `codeforces-logged-in-p`
  first; if not logged in, prompt `M-x codeforces-login`.
- **Cookie expired**: scrape receives the login form instead of expected content
  → prompt to re-login.
- **Cloudflare**: challenge page detected → automatic browser fallback (§3.4).
- **Poll timeout**: still `TESTING` at `codeforces-poll-timeout` → stop,
  prompt "retry with C-c C-s or check the website".

## RivenEmacs Integration (`lisp/tools/init-codeforces.el`)

```elisp
(use-package emacs-codeforces
  :load-path "../../emacs-codeforces"   ; or :vc once published to MELPA
  :commands (codeforces-login codeforces-browse-problems)
  :custom
  (codeforces-default-language "rust"))
;; leader-key bindings via general.el
```

## Testing Strategy (verifiable without a real account)

- **Pure parsing logic in client/api/scrape**: unit tests with fixed JSON/HTML
  sample strings — problem-list parsing, HTML→Org conversion, verdict
  extraction, programTypeId matching, Cloudflare-detection function.
- **Workspace template init**: in a temp directory, test dir creation +
  template write + non-overwrite logic.
- **No network-dependent integration tests** (flaky in CI); the network layer is
  covered by a manual-verification checklist.

## Open Questions

None remaining — all major decisions resolved during brainstorming.
