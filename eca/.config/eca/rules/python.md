# These rules apply for python projects:
- To run type checks, use the `ty-check` tool. Do NOT run `uv run ty check` via the `run_command` tool.
- To run linting, use the `ruff-check` tool. Do NOT run `uv run ruff check` via the `run_command` tool.
- To run the formatter, use the `ruff-format` tool. Do NOT run `uv run ruff format` via the `run_command` tool.
- Each of these tools requires a `path` argument to set its scope: pass a file or directory (e.g. `src`), or `.` for the whole project.

# Tools

## Language Server `definition` Tool
- Use **simple/unqualified names only**: `Settings`, `CameraDevice`, `parse_args`, `SEVERITY2LOGGING`
- Do **NOT** use fully-qualified dotted paths: `optofidelity.station.settings.Settings` will not resolve
- Do **NOT** use `ClassName.method` dot notation: `Station.__init__` or `LicenseChecker.is_license_valid` will not resolve
- Works for classes, functions, variables, and constants defined in project source files (including all workspace members)
- Does **NOT** resolve symbols from third-party/external packages (e.g. `BaseSettings`, `Signal` from installed dependencies)
- When a name exists in multiple files, all definitions are returned

## Language Server `goto_definition` Tool
- Resolves the definition of a symbol at a specific file position — the same as the editor's "Go to Definition" action
- Takes `filePath`, `line`, and `column` (all 1-indexed) pointing to a **usage** of the symbol
- **Can resolve third-party/external packages** unlike the `definition` tool
- Returns the source line at the requested position and the symbol name at the target, so you can verify the position was correct
- Use this when `definition` cannot find a symbol (e.g. third-party imports) and you need the full source code
- Prefer `definition` for workspace symbols (simpler, no position needed); use `goto_definition` as a fallback

## Language Server `hover` Tool
- Prefer `hover` over `definition` when you need to inspect a **third-party or external symbol** — it resolves any symbol the editor can see, including installed packages
- Requires a file path, line, and column pointing to a **usage** of the symbol (e.g. an import line)
- Returns the symbol's type signature and docstring, but not the full source implementation
- Unlike `goto_definition`, does **not** return the definition's file path or source code

## GitHits
- Call to get information about how to implement something, based on real-world examples. Especially useful for modules that are new, or lack documentation.
- Can give insights for issues in open source modules, and ways to work around them.
