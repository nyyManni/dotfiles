---
name: agent-shell
description: Maintain descriptive names for Agent-shell conversations through Emacs MCP, including new and resumed sessions and topic changes. Use in every identified Agent-shell session regardless of the task, and for requests about Agent-shell buffer management.
---

# Agent-shell

Keep the current conversation easy to identify in Agent-shell-manager. Routine
naming is authorized by the user's standing preference. Rename only this
conversation's buffer; preserve a name explicitly chosen by the user unless
they request a change.

## Naming

- Name the session once its purpose is clear. Reassess after a substantial
  change of task and on resume; keep an accurate name stable through substeps.
- Use `[Agent] Descriptive topic`, for example `[Codex] Agent-shell naming`
  or `[Claude] Camera discovery`. Keep the prefix short and omit the project
  name and path: Agent-shell-manager already shows the path separately.
  The user's Emacs configuration makes the manager read agent identity and
  restart configuration from buffer metadata rather than the name.
- Use a short topic or intended outcome, not a copied prompt or a status such
  as “working” or “done”. Avoid secrets in names. Add meaningful context when
  two sessions would otherwise have the same title; Emacs can uniquify names.
- Prefer an exposed native harness rename control for the identified session
  when available. Verify whether it updates the Emacs buffer too; use the
  fallback below if needed. Do not assume an app-server method documented
  elsewhere is callable from the current harness.

## Rename in one Emacs MCP call

Call the dedicated Emacs MCP `rename-session` tool (typically exposed as
`mcp__emacs__rename_session`). Pass a short plain topic; the handler adds
`[Codex]` (or the actual agent), resolves and
verifies exactly one session, handles name collisions and existing viewports,
sets the buffer-name override, and refreshes Agent-shell-manager.

Agent-shell injects an `<agent-shell-context>` text block into each prompt,
containing `ACP session ID: ...`. Use that exact ID, not a Codex thread ID,
a buffer name, the selected buffer, or a project path.

```json
{"session_id": "ACP SESSION ID FROM CONTEXT", "topic": "Agent-shell naming"}
```

The JSON result contains the verified `session_id`, `name`, `override`, and
`viewport`. No session listing, transcript inspection, viewport API discovery,
or buffer switching is needed on the agent side. The same ACP session ID is
suitable for other session-scoped Emacs APIs; use the latest injected ID after
resuming or forking.

If the context block or dedicated tool is unavailable, or MCP blocks the call, report
the specific blocker and continue the main task. Do not guess the ID, search
transcripts, reconstruct the old multi-call workflow, or change MCP security
settings. Do not fall back to `eval-elisp` for routine naming. Existing MCP
connections may need reconnecting to discover a newly registered tool. If a
tool allowlist is configured, it must include `rename-session`.
Existing agents need the request decorator attached to their ACP
client before they receive this context; new agents inherit it at startup.

## Saved sessions

An Emacs buffer name and a backend's persisted session title are separate.
Use an exposed persistent session-title control when available and verify its
result. Otherwise reapply the buffer name when the conversation resumes; do
not claim that the rename changes titles in the previous-session picker.
Changing Agent-shell's in-memory `:session :title` alone is not persistence and
may be overwritten by a backend `session/list` refresh. Do not edit backend
databases or rollout files to simulate a supported rename control.
