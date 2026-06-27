#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""codeforces_agent.py — Codeforces website agent (Cloudflare bypass via curl-cffi).

Subcommands:
  statement --contest N --index "A"
      Fetch a problem statement and print it as Org text on stdout.

  submit --contest N --index "A" --lang rust --source-file PATH
      Submit a solution using the stored cookie; print the submission id.

  login-check
      Use the stored cookie to visit the submit page; print the handle if
      authenticated, exit non-zero otherwise.

All network access goes through curl-cffi (browser TLS fingerprint) so it
passes Cloudflare.  stdout carries the result; stderr carries errors; a
non-zero exit signals failure.
"""

import argparse
import os
import re
import sys

SITE = "https://codeforces.com"

# Language -> substring matched against the programTypeId <option> label.
# Must be kept loose: Codeforces updates compiler versions over time.
LANG_NEEDLE = {
    "rust": "Rust",
    "python": "Python 3",
    "cpp": "GNU G++",
    "java": "Java",
}


def _session(cookie=None):
    """Return a curl-cffi Session that bypasses Cloudflare."""
    try:
        from curl_cffi import requests as cffi_requests
    except ImportError:
        sys.stderr.write(
            "codeforces_agent: curl-cffi is not installed. "
            "Run: pip install curl-cffi\n")
        sys.exit(3)
    s = cffi_requests.Session(impersonate="chrome")
    if cookie:
        s.headers.update({"Cookie": cookie})
    return s


def _home_dir():
    return os.path.expanduser(
        os.environ.get("CODEFORCES_HOME", "~/.emacs-codeforces"))


def _cookie_path():
    return os.path.join(_home_dir(), "cookie")


def _read_cookie():
    p = _cookie_path()
    if not os.path.exists(p):
        return None
    with open(p, "r", encoding="utf-8") as f:
        return f.read().strip()


# --------------------------------------------------------------------------
# statement: HTML -> Org
# --------------------------------------------------------------------------

# Globals set by fetch_statement() so _node_to_org can download images.
_IMG_SESSION = None
_IMG_DIR = None


def _img_to_file_link(src):
    """Download SRC (a Codeforces image URL) into _IMG_DIR and return an
    Org file: link to the cached copy, so org-display-inline-images renders it.
    Returns a raw [[src]] link (best-effort) if the cache/session is unset."""
    if not src:
        return ""
    try:
        import hashlib
        # Stable filename derived from the URL (the path already contains a
        # content hash on Codeforces' espresso CDN, so this is plenty unique).
        ext = ".png" if ".png" in src else (".jpg" if ".jpg" in src else ".png")
        fname = hashlib.md5(src.encode("utf-8")).hexdigest() + ext
        dest = os.path.join(_IMG_DIR, fname) if _IMG_DIR else None
        if dest and _IMG_SESSION and not os.path.exists(dest):
            os.makedirs(_IMG_DIR, exist_ok=True)
            resp = _IMG_SESSION.get(src, timeout=20)
            if resp.status_code == 200:
                with open(dest, "wb") as f:
                    f.write(resp.content)
        if dest and os.path.exists(dest):
            return "[[file:%s]]" % dest
    except Exception as e:
        sys.stderr.write("image download failed for %s: %s\n" % (src, e))
    # Fallback: raw link (may not render inline, but is not lost).
    return "[[%s]]" % src


def _node_to_org(node, math=False):
    """Recursively convert a BeautifulSoup node into Org text.

    Handles the Codeforces statement structure: header (title/limits),
    section-title/property-title subsections, paragraphs, lists, <pre>
    sample I/O, and inline math (rendered as LaTeX).

    When MATH is True we are inside a Codeforces math construct (tex-span,
    sub/sup of a formula), so <i> variables are emitted as plain text and
    sub/sup as LaTeX _{} / ^{} — and the whole construct is wrapped in $...$
    by the tex-span handler.

    Images are downloaded to the statement cache (via _IMG_SESSION/_IMG_DIR)
    and emitted as [[file:...]] so Org can render them inline."""
    from bs4 import NavigableString, Tag

    if isinstance(node, NavigableString):
        # In math mode, collapse the thin-space (\u2009) Codeforces inserts.
        if math:
            return str(node).replace("\u2009", " ")
        return str(node)
    if not isinstance(node, Tag):
        return ""

    name = node.name
    classes = node.get("class") or []

    def children_text():
        return "".join(_node_to_org(c, math) for c in node.children)

    def children_text_plain():
        """Children rendered without any markup (for math variables)."""
        return "".join(_node_to_org(c, True) for c in node.children)

    # --- Codeforces structural classes ---
    if "section-title" in classes:
        return "** " + node.get_text(strip=True) + "\n\n"
    if "property-title" in classes:
        # "time limit per test" etc. — render as a bold label.
        return "*" + node.get_text(strip=True) + "* "
    if name == "title" or (name == "div" and "title" in classes):
        return "* " + node.get_text(strip=True) + "\n\n"
    # Each limit property (time/memory/input/output) on its own line.
    if name == "div" and any(c in classes for c in
                             ("time-limit", "memory-limit", "input-file",
                              "output-file")):
        return children_text().strip() + "  \n"
    if "input" in classes and name == "div" and node.find("pre"):
        return "Input:\n" + _node_to_org(node.find("pre"))
    if "output" in classes and name == "div" and node.find("pre"):
        return "Output:\n" + _node_to_org(node.find("pre"))

    # --- Standard HTML ---
    if name == "p":
        return children_text().strip() + "\n\n"
    if name == "pre":
        return "#+begin_src fundamental\n%s\n#+end_src\n\n" % node.get_text().strip()
    if name in ("ul", "ol"):
        lines = []
        idx = 1
        for li in node.find_all("li", recursive=False):
            txt = "".join(_node_to_org(c) for c in li.children).strip()
            bullet = "- " if name == "ul" else "%d. " % idx
            lines.append(bullet + txt)
            if name == "ol":
                idx += 1
        return "\n".join(lines) + "\n\n"
    if name == "br":
        return "  \n"
    if name in ("b", "strong"):
        return "*" + children_text() + "*"
    if name in ("i", "em"):
        if math:
            # Math variable: emit bare text (no /.../ ).
            return children_text_plain()
        # Prose emphasis: keep as /italic/.
        return "/" + children_text() + "/"
    if name == "sub":
        # Math subscript -> LaTeX _{...}; prose subscript -> Org _{...}.
        return "_{" + children_text_plain() + "}"
    if name == "sup":
        return "^{" + children_text_plain() + "}"
    if name == "span":
        # tex-span wraps a semantic-HTML formula (variables + sub/sup).  Render
        # its content as LaTeX math wrapped in $...$ so Org previews it.
        if "tex-span" in classes:
            return "$" + children_text_plain().strip() + "$"
        # Other spans: just unwrap.
        return children_text()
    if name == "img":
        # Download the image into the statement cache and emit a [[file:...]]
        # link so org-display-inline-images can render it (Org only inlines
        # file:/attachment: links, not http URLs).  No forced newline: Org
        # displays content-less image links inline within a line.
        src = node.get("src") or ""
        if not src:
            return ""
        return _img_to_file_link(src)
    # Default: recurse (div, section, etc.)
    return children_text()


def _clean(text):
    """Tidy whitespace and normalize math delimiters.

    Codeforces delimits inline math with `$$$...$$$` and display math with
    `$$$$$$...$$$$$$` (MathJax config).  Org/preview expects `$...$` for
    inline and `$$...$$` for display, so we strip the extra `$`s.
    Code blocks are protected from this substitution."""
    # Protect #+begin_src ... #+end_src blocks from any math substitution.
    blocks = []

    def _stash(m):
        blocks.append(m.group(0))
        return "\x00%d\x00" % (len(blocks) - 1)

    text = re.sub(r"#\+begin_src.*?#\+end_src", _stash, text, flags=re.DOTALL)

    # Display math: $$$$$$...$$$$$$ -> $$...$$
    text = re.sub(r"\${6}([^\n]*?)\${6}", r"$$\1$$", text)
    # Inline math: $$$...$$$ -> $...$
    text = re.sub(r"\${3}([^\n]*?)\${3}", r"$\1$", text)

    # Restore code blocks.
    for i, b in enumerate(blocks):
        text = text.replace("\x00%d\x00" % i, b)

    # Collapse 3+ blank lines to 2.
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip() + "\n"


def fetch_statement(contest, index):
    """Fetch problem {contest}/{index} and return Org text."""
    global _IMG_SESSION, _IMG_DIR
    from bs4 import BeautifulSoup

    url = "%s/problemset/problem/%s/%s" % (SITE, contest, index)
    s = _session()
    r = s.get(url, timeout=30)
    if r.status_code != 200 or "Just a moment" in r.text:
        sys.stderr.write(
            "Failed to fetch statement (HTTP %d, possibly Cloudflare).\n"
            % r.status_code)
        sys.exit(1)

    soup = BeautifulSoup(r.text, "lxml")
    stmt = soup.select_one("div.problem-statement")
    if not stmt:
        sys.stderr.write("Problem statement div not found on the page.\n")
        sys.exit(1)

    # Set up the image cache so <img> tags download and link locally.
    _IMG_SESSION = s
    _IMG_DIR = os.path.join(_home_dir(), "cache", "img", "%s_%s" % (contest, index))

    org = _node_to_org(stmt)
    sys.stdout.write(_clean(org))


# --------------------------------------------------------------------------
# submit
# --------------------------------------------------------------------------

def _parse_csrf(html):
    """Extract the csrf token from a Codeforces page."""
    m = re.search(r'name="X-Csrf-Token"\s+content="([0-9a-f]+)"', html)
    if m:
        return m.group(1)
    m = re.search(r"data-csrf=['\"]([0-9a-f]+)['\"]", html)
    if m:
        return m.group(1)
    m = re.search(r'name=["\']csrf_token["\']\s+value=["\']([0-9a-f]+)["\']', html)
    if m:
        return m.group(1)
    return None


def _parse_language_id(html, lang):
    """Find the programTypeId whose label matches LANG."""
    from bs4 import BeautifulSoup

    soup = BeautifulSoup(html, "lxml")
    sel = soup.select_one("select[name=programTypeId]")
    if not sel:
        return None  # Not authenticated (submit form only renders for logged-in users)
    needle = LANG_NEEDLE.get(lang)
    if not needle:
        return None
    for opt in sel.select("option"):
        if needle in (opt.get_text() or ""):
            return opt.get("value")
    return None


def submit(contest, index, lang, source_file):
    """Submit the solution; print the new submission id."""
    cookie = _read_cookie()
    if not cookie:
        sys.stderr.write("Not logged in: no cookie file. Run codeforces-login.\n")
        sys.exit(2)

    if not os.path.exists(source_file):
        sys.stderr.write("Source file not found: %s\n" % source_file)
        sys.exit(2)
    with open(source_file, "r", encoding="utf-8") as f:
        source = f.read()

    s = _session(cookie=cookie)

    # 1. GET the submit page for csrf + language id.
    page = s.get("%s/problemset/submit" % SITE, timeout=30)
    if page.status_code != 200 or "Just a moment" in page.text:
        sys.stderr.write("Cloudflare blocked the submit page (HTTP %d).\n"
                         % page.status_code)
        sys.exit(1)
    csrf = _parse_csrf(page.text)
    lang_id = _parse_language_id(page.text, lang)
    if not csrf:
        sys.stderr.write("Could not find csrf token (cookie may be expired).\n")
        sys.exit(2)
    if not lang_id:
        sys.stderr.write(
            "Could not find programTypeId for language '%s'. "
            "Submit form may not be visible (not logged in).\n" % lang)
        sys.exit(2)

    # 2. POST the solution.
    data = {
        "csrf_token": csrf,
        "action": "submitSolutionFormSubmitted",
        "submittedProblemIndex": index,
        "contestId": str(contest),
        "programTypeId": lang_id,
        "source": source,
        "tabSize": "4",
    }
    resp = s.post("%s/problemset/submit" % SITE, data=data, timeout=30)

    # 3. Detect outcome.
    final_url = str(resp.url)
    if "/my" in final_url or "/status" in final_url:
        # Success — fetch the newest submission id from the /my page.
        sub_id = _extract_latest_submission_id(s, contest)
        if sub_id:
            sys.stdout.write(sub_id)
            return
        # Fallback: redirect happened but id not found; treat as success anyway.
        sys.stdout.write("OK")
        return

    # Failure: returned to the form with errors.
    from bs4 import BeautifulSoup
    soup = BeautifulSoup(resp.text, "lxml")
    err = soup.select_one("span.error")
    msg = err.get_text(strip=True) if err else "unknown error"
    sys.stderr.write("Submit rejected: %s\n" % msg)
    sys.exit(1)


def _extract_latest_submission_id(session, contest):
    """GET /contest/{id}/my and parse the newest submission id."""
    r = session.get("%s/contest/%s/my" % (SITE, contest), timeout=30)
    if r.status_code != 200:
        return None
    # Submission ids appear as data-submission-id or in /submission/<id> links.
    m = re.search(r'submission/(\d+)', r.text)
    if m:
        return m.group(1)
    m = re.search(r'data-submission-id="(\d+)"', r.text)
    if m:
        return m.group(1)
    return None


# --------------------------------------------------------------------------
# login-check
# --------------------------------------------------------------------------

def login_check():
    """Visit the submit page with the stored cookie; print handle if logged in."""
    cookie = _read_cookie()
    if not cookie:
        sys.stderr.write("No cookie stored. Run codeforces-login.\n")
        sys.exit(2)
    s = _session(cookie=cookie)
    r = s.get("%s/problemset/submit" % SITE, timeout=30)
    if r.status_code != 200 or "Just a moment" in r.text:
        sys.stderr.write("Cloudflare blocked the request.\n")
        sys.exit(1)
    # Logged-in => the language <select> is present.
    if 'name="programTypeId"' not in r.text:
        sys.stderr.write("Not logged in (cookie expired or invalid).\n")
        sys.exit(2)
    # Best-effort handle from the page.
    m = re.search(r'userLink[^>]*>([A-Za-z0-9_.-]+)<', r.text)
    if m:
        sys.stdout.write(m.group(1))
    else:
        sys.stdout.write("OK")
    return


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description="Codeforces website agent")
    sub = ap.add_subparsers(dest="cmd", required=True)

    p_st = sub.add_parser("statement", help="fetch a problem statement as Org")
    p_st.add_argument("--contest", required=True)
    p_st.add_argument("--index", required=True)

    p_su = sub.add_parser("submit", help="submit a solution")
    p_su.add_argument("--contest", required=True)
    p_su.add_argument("--index", required=True)
    p_su.add_argument("--lang", required=True,
                      choices=list(LANG_NEEDLE.keys()))
    p_su.add_argument("--source-file", required=True)

    sub.add_parser("login-check", help="verify the stored cookie")

    args = ap.parse_args()
    if args.cmd == "statement":
        fetch_statement(args.contest, args.index)
    elif args.cmd == "submit":
        submit(args.contest, args.index, args.lang, args.source_file)
    elif args.cmd == "login-check":
        login_check()


if __name__ == "__main__":
    main()
