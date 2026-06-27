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

> **Revision 2026-06-28 (after live testing):** Codeforces now sits behind a
> site-wide Cloudflare "Just a moment..." challenge. **Any** non-browser HTTP
> client (`curl`, Emacs `url.el`) hitting the *website* (`/enter`, statement
> pages, submit form) returns HTTP 403 + a Cloudflare challenge page — not the
> real page. Only the **public JSON API** (`codeforces.com/api/...`) is
> reachable. This forced a design pivot: every website-only operation (login
> validation, statement scraping, submit) is delegated to the user's browser
> via `browse-url`; everything Emacs renders itself comes from the public API.

The operations split like this:

| Operation | Public JSON API (`codeforces.com/api/...`) | Website HTML (needs cookie) |
|---|---|---|
| Problem list + tags + rating | ✅ `problemset.problems` (supports `tags=` filter) | — |
| Full problem statement | ❌ none | ⚠️ Cloudflare-blocked → open in browser |
| Submit code | ❌ none | ⚠️ Cloudflare-blocked → open in browser |
| Submission status / verdict | ✅ `user.status` / `contest.status` (**no auth needed** for public users) | — |
| Login | ❌ none | ⚠️ Cloudflare-blocked → store cookie only, no validation |

Two load-bearing realities:

1. **No programmatic login, no login validation.** The `/enter` page has
   hCaptcha *and* is now Cloudflare-gated. `codeforces-login` therefore only
   **stores** the user's session cookie + handle (for the `user.status` poll)
   without trying to validate it. The cookie is consumed by the browser on
   submit (the user is already logged in there).
2. **`user.status` needs no `apiSig`.** Confirmed live: `user.status?handle=X`
   returns any user's submissions without API keys. Polling our own verdicts is
   just "store handle at login, poll `user.status` for recent submissions,
   match by problem/contestId + time."

### What the Cloudflare reality changes vs. the original spec

- **Login** — was "validate via GET /enter detecting Logout link"; now "store
  cookie + handle, no network call."
- **Statement** — was "scrape HTML → Org in a buffer"; now "render the API
  metadata (name, tags, time/memory limits, URL) in Org; the full statement is
  one key (`o`) away via `browse-url`."
- **Submit** — was "HTTP POST first, browser fallback on Cloudflare"; now
  "**always** `browse-url` the submit page" (the HTTP POST path was deleted —
  it always 403s). The status poll is unchanged and reliable.
- **Status poll** — was "`contest.status?submissionId`"; now
  "`user.status?handle=X&from=1&count=5` matched by problem."

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
├── credentials                  # login state: handle + cookie (chmod 600)
├── cache/
│   └── problems.json            # local cache of problemset.problems (+ timestamp)
└── workspace/
    └── {contestId}_{index}/     # one dir per problem, e.g. 1234_A/
        └── solution.rs          # source (initialized from language template)
```

> Note: the original request wrote `.emac-codeforces` (missing an `s`); the
> library name is `emacs-codeforces`, so the directory is `.emacs-codeforces`
> for consistency. This is the agreed name.

## Auth Flow (requirement 1) — store handle + cookie, no validation

`/enter` is hCaptcha'd *and* Cloudflare-gated, so no validation is attempted.
The cookie itself is consumed by the user's browser on submit (the user is
already logged in there). What Emacs needs locally is the **handle** (for
`user.status` polling) plus the cookie string (stored for completeness / future
use), nothing more.

1. `M-x codeforces-login`:
   - Prompts for the Codeforces **handle** (e.g. `tourist`).
   - Prompts for the session **cookie** (no default; user pastes from browser).
   - Writes both to `~/.emacs-codeforces/credentials` as two lines
     (`handle=...\ncookie=...`), `set-file-modes` 0600.
2. Creates `~/.emacs-codeforces/` (cache + workspace subdirs) — requirement 1's
   "create directory on successful login."
3. That's it — no network call, no validation. Correctness is established the
   first time the user browses problems (public API works regardless) or polls
   a submission (which only needs the handle).

**Why no validation?** Cloudflare returns 403 to all non-browser clients on the
website, so any `/enter`-based check would always report "invalid" regardless of
the real cookie. Validation would be actively misleading.

**Credential access**: `+cf--handle` reads `handle=` from `credentials`;
`+cf--cookie-header` reads `cookie=` (kept for any future direct-website use,
though submit no longer needs it).

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

The detail buffer is an **Org mode** buffer rendering the **API metadata**
(name, tags, limits, URL). The full statement HTML is Cloudflare-blocked for
direct fetch, so it is one key away in the browser:

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
The full statement is rendered in the browser (Cloudflare blocks direct
fetch).  Press `o` to open it.

** Submission Status       ← updated in place during polling
*No active submission.*
```

- **Data source**: the `Problem` plist already fetched for the list (from
     `problemset.problems`) — no extra network call, no Cloudflare exposure.
- **Key `o`** → `codeforces-open-statement-in-browser`: `browse-url` opens the
     problem URL (the browser is logged in and past Cloudflare).
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
5. Detail buffer headline title becomes `● 1234A - Solving`.

### Submit + real-time result (requirement 5) — `C-c C-s` in detail buffer

`C-c C-s` → `codeforces-submit`:

1. Opens the problem's submit page in the browser via `browse-url` (the user is
   logged in there and past Cloudflare). Emacs **never** POSTs directly — that
   path always 403s.
2. Echoes: "Submit page opened in browser. Polling for the verdict…"
3. **Real-time status poll** (core; goes through the public API, unaffected by
   Cloudflare, needs no cookie — only the handle):
   - `run-with-timer` polls every `codeforces-poll-interval` seconds (default
     **3**) calling `user.status?handle=X&from=1&count=5` and matching the most
     recent submission for **this problem's contestId** whose
     `creationTimeSeconds` is at/after the submit click. The first matching
     submission is the one we track.
   - The detail buffer's `** Submission Status` section updates **in place**:
       `TESTING 12/30 tests…` → final `OK ✅ (124ms, 8MB)` or
       `WRONG_ANSWER on test 14 ❌`.
   - When the verdict reaches a terminal state (`OK`/`WRONG_ANSWER`/
       `COMPILATION_ERROR`/…) or no matching submission appears within
       `codeforces-poll-timeout` seconds (default **90**, generous since the
       user must finish submitting in the browser), **stop polling** and update
       the headline prefix icon (✅/❌).

> The poll window starts when `C-c C-s` is pressed (before the browser submit
> completes), so it reliably catches the new submission even if the user is
> slow in the browser.

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
