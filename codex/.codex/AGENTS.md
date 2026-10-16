<!-- file-edits -->
Always use `apply_patch` to create or edit text files. Never bypass it with Python, sed, Perl, shell redirection, heredocs that write files, Emacs evaluation, or similar alternative file-writing methods. If `apply_patch` is unavailable, blocked, or requires approval, report the blocker and obtain the needed approval rather than switching to another editing method. Before modifying any file or link outside the workspace, obtain explicit user confirmation even if automatic approval review would permit the action.
<!-- file-edits -->

<!-- githits -->
GitHits is installed for public OSS/package evidence. When the `githits-mcp` skill is loaded, follow it and do not call `quick_start`. Otherwise call GitHits `quick_start` once per session before any other GitHits tool.
<!-- githits -->

<!-- agent-shell -->
At the start of every session with Emacs MCP available, read `~/.codex/skills/agent-shell/SKILL.md`. When an `<agent-shell-context>` block supplies an ACP session ID, automatically call the dedicated Emacs MCP `rename-session` tool with that exact `session_id` and a concise descriptive `topic` as soon as the conversation provides something meaningful to name. Do not wait for the user to request a name or ask for confirmation. Reassess the name as the conversation develops and after resuming; update it whenever a different name would better describe the session's purpose, while keeping an accurate name stable through routine substeps. Preserve names explicitly chosen by the user unless they request a change. Follow the skill even when the task itself is unrelated to Emacs. If the session ID or tool is unavailable, report the blocker without guessing the session identity or falling back to arbitrary Elisp evaluation.
<!-- agent-shell -->
