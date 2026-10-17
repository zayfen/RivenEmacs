"""Offline unit tests for emacs-codeforces/scripts/codeforces_agent.py.

Only pure, network-free helpers are exercised here.  Tests that need
beautifulsoup4 are skipped automatically when the optional dependency is not
installed, so `python3 -m unittest discover -s emacs-codeforces/test` works on
a bare checkout.

Run from the repository root:

    python3 -m unittest discover -s emacs-codeforces/test -v
"""

import os
import sys
import tempfile
import unittest
import warnings
from pathlib import Path
from unittest import mock

_REPO_ROOT = Path(__file__).resolve().parents[2]
_SCRIPTS_DIR = _REPO_ROOT / "emacs-codeforces" / "scripts"
sys.path.insert(0, str(_SCRIPTS_DIR))

import codeforces_agent as agent  # noqa: E402

try:
    import bs4  # noqa: F401

    _HAS_BS4 = True
except ImportError:  # pragma: no cover - depends on the environment
    _HAS_BS4 = False


class CleanMathTests(unittest.TestCase):
    """`_clean' converts Codeforces math markup to Org/LaTeX markup."""

    def test_inline_math_and_mathjax_commands(self):
        cleaned = agent._clean("a $$$x \\le y$$$ b")
        self.assertIn("$x \\leq", cleaned)
        self.assertNotIn("$\\le ", cleaned)

    def test_display_math_delimiters(self):
        cleaned = agent._clean("$$$$$$x+y$$$$$$")
        self.assertIn("$$x+y$$", cleaned)

    def test_mathjax_extensions_are_latex(self):
        cleaned = agent._clean("$a \\lt b \\gt c \\dots d \\leqslant e \\geqslant f$")
        self.assertIn("<", cleaned)
        self.assertIn(">", cleaned)
        self.assertIn("\\ldots", cleaned)
        self.assertIn("\\leq", cleaned)
        self.assertIn("\\geq", cleaned)

    def test_code_blocks_are_not_rewritten(self):
        source = "#+begin_src fundamental\nprint('$$$x$$$')\n#+end_src"
        cleaned = agent._clean(source)
        self.assertIn("$$$x$$$", cleaned)

    def test_blank_lines_are_collapsed(self):
        self.assertEqual(agent._clean("a\n\n\n\nb"), "a\n\nb\n")


class ParseCsrfTests(unittest.TestCase):
    """`_parse_csrf' accepts the token shapes found on Codeforces pages."""

    def test_meta_tag(self):
        html = '<meta name="X-Csrf-Token" content="deadbeef1234">'
        self.assertEqual(agent._parse_csrf(html), "deadbeef1234")

    def test_data_attribute(self):
        self.assertEqual(agent._parse_csrf("<div data-csrf='cafebabe99'></div>"),
                         "cafebabe99")

    def test_hidden_input(self):
        html = '<input type="hidden" name="csrf_token" value="0123456789abcdef">'
        self.assertEqual(agent._parse_csrf(html), "0123456789abcdef")

    def test_missing_token_is_none(self):
        self.assertIsNone(agent._parse_csrf("<html></html>"))


class SubmitUrlTests(unittest.TestCase):
    """`_submit_url' routes regular contests vs. gyms."""

    def test_regular_contest(self):
        self.assertEqual(agent._submit_url(1234),
                         "https://codeforces.com/contest/1234/submit")

    def test_contest_id_accepts_strings(self):
        self.assertEqual(agent._submit_url("1234"),
                         "https://codeforces.com/contest/1234/submit")

    def test_gym_boundary(self):
        self.assertEqual(agent._submit_url(99999),
                         "https://codeforces.com/contest/99999/submit")
        self.assertEqual(agent._submit_url(100000),
                         "https://codeforces.com/gym/100000/submit")

    def test_invalid_contest_id_raises(self):
        with self.assertRaises(ValueError):
            agent._submit_url("not-a-number")


@unittest.skipUnless(_HAS_BS4, "beautifulsoup4 is not installed")
class LanguageIdTests(unittest.TestCase):
    """`_parse_language_id' maps language names to programTypeId values."""

    HTML = (
        '<select name="programTypeId">'
        '<option value="1">GNU G++17 7.3.0</option>'
        '<option value="2">Rust 1.85</option>'
        '<option value="3">Python 3.12</option>'
        "</select>"
    )

    @staticmethod
    def _parse(html, language):
        """Call `_parse_language_id' without bs4/lxml deprecation noise."""
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            return agent._parse_language_id(html, language)

    def test_rust_and_python(self):
        self.assertEqual(self._parse(self.HTML, "rust"), "2")
        self.assertEqual(self._parse(self.HTML, "python"), "3")

    def test_cpp(self):
        self.assertEqual(self._parse(self.HTML, "cpp"), "1")

    def test_missing_select_means_not_logged_in(self):
        self.assertIsNone(self._parse("<html></html>", "rust"))

    def test_unknown_language_is_none(self):
        self.assertIsNone(self._parse(self.HTML, "cobol"))


class CookieStorageTests(unittest.TestCase):
    """Cookie/credential files are read from CODEFORCES_HOME without network."""

    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory(prefix="cf-agent-test-")
        self.home = self._tmp.name
        self._env = mock.patch.dict(os.environ, {"CODEFORCES_HOME": self.home})
        self._env.start()

    def tearDown(self):
        self._env.stop()
        self._tmp.cleanup()

    def test_home_and_cookie_path(self):
        self.assertEqual(agent._home_dir(), self.home)
        self.assertEqual(agent._cookie_path(),
                         os.path.join(self.home, "cookie"))

    def test_missing_cookie_is_none(self):
        self.assertIsNone(agent._read_cookie())

    def test_cookie_is_stripped(self):
        Path(self.home, "cookie").write_text("  session=abc\n", encoding="utf-8")
        self.assertEqual(agent._read_cookie(), "session=abc")

    def test_handle_from_credentials(self):
        Path(self.home, "credentials").write_text(
            "handle=tourist\ncookie=session=abc\n", encoding="utf-8")
        self.assertEqual(agent._handle_from_cookie_session(), "tourist")

    def test_missing_credentials_has_no_handle(self):
        self.assertIsNone(agent._handle_from_cookie_session())
        Path(self.home, "credentials").write_text(
            "cookie=session=abc\n", encoding="utf-8")
        self.assertIsNone(agent._handle_from_cookie_session())


class ImageLinkTests(unittest.TestCase):
    """`_img_to_file_link' falls back to a raw link when caching is unavailable."""

    def test_empty_source(self):
        self.assertEqual(agent._img_to_file_link(""), "")

    def test_raw_link_fallback(self):
        agent._IMG_SESSION = None
        agent._IMG_DIR = None
        self.assertEqual(agent._img_to_file_link("https://codeforces.com/x.png"),
                         "[[https://codeforces.com/x.png]]")


if __name__ == "__main__":
    unittest.main()
