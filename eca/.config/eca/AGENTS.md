<!-- githits -->
GitHits is configured in this environment. Use the installed githits-mcp skill and GitHits MCP tools as the default OSS context layer across the full software development lifecycle: discovery, planning, research, implementation, debugging, and maintenance. Prefer GitHits before model memory or generic search. When the dependency or repository is known, default to search/docs_* for docs and code_files/code_grep/code_read for exact source and call sites. Use get_example for broad OSS-first scans of vague issues, unfamiliar errors, cross-library patterns, how others solved something, and rare real-world examples that may appear in only one or a few repos. Use pkg_* for package metadata, security, dependencies, changelogs, and upgrades. Ground answers in fetched GitHits evidence and cite package, repository, file, docs page, or version facts when available.
<!-- githits -->

## Avoid extremely long lines in tool results

The editor can hang on extremely long rendered lines. Large outputs are fine when
split across lines; do not truncate or paginate merely to reduce total output size.
Prevent long lines at the tool-output source, not by reformatting the final answer.

- In AWS MCP `aws___run_script`, return native dictionaries/lists inside the required
  `result` dictionary and end with `result`. The harness pretty-prints this structure.
- Avoid `print()` in AWS scripts except for predictably short messages. All captured
  stdout is rendered as one JSON string, with newlines escaped; the TOTAL stdout
  length matters, not the length of each printed line. The same risk applies to stderr.
- Do not stringify returned structures with `str()`, `repr()`, or `json.dumps()`.
  Parse JSON-encoded strings into native dictionaries/lists when appropriate.
- Long string values can still create huge lines inside a structured result. When
  full content is needed, return an explicitly labeled ordered list of chunk objects,
  each with an index and a short text field (aim for 120 characters per chunk).
  Do NOT use a list of bare strings: this harness renders scalar arrays on one line,
  while object elements receive line breaks. Preserve exact content and ordering so
  concatenating the text fields with no separator reconstructs the original string.
  For other large scalar arrays, likewise use indexed objects containing each value
  when needed to avoid long lines, preserving the original value types and order.
  Do not silently omit data or expose secrets to work around display limitations.
- For other tools, distinguish actual multiline text from text embedded in a JSON
  string. Prefer native structured results when the harness pretty-prints them;
  use pretty-printed text only when the output channel renders actual line breaks.
  Tool-controlled envelopes or metadata may remain outside the agent's control.
