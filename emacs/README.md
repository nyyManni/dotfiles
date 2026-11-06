# Emacs layout

Symlink `.config/emacs` to `~/.config/emacs` (or use Stow). Keep these files
in Git:

- `early-init.el` and `init.el`: startup configuration.
- `user-lisp/*.el`: personal libraries, loaded with `require` after their
  package dependencies are available.
- `tests/*.el`: ERT tests; use `-L .config/emacs/user-lisp` when running them.
- `versions/default.el`: committed Straight package revisions.

For package tests in batch Emacs, also load `early-init.el` before loading
the packages/tests (`emacs -Q --batch -l .config/emacs/early-init.el ...`).
Otherwise the batch process can recreate generated files such as `eln-cache/`
under the config directory, even though normal interactive startup is clean.

`early-init.el` redirects `user-emacs-directory` to `~/.local/share/emacs/`.
Packages, no-littering's `etc/` and `var/`, backups, and other generated data
stay there. Auto-save contents go in `var/auto-save/`; no-littering puts
session recovery lists in `var/auto-save/sessions/`.

Native compilation output goes in `~/.cache/emacs/eln-cache/` (or
`$XDG_CACHE_HOME/emacs/eln-cache/` when an absolute cache home is set).
Old `eln-cache/` directories are left untouched; Emacs can regenerate their
contents, so they can be removed after restarting with the new configuration.

Native-cache redirection runs only when `native-comp-available-p` is true.
Neo Emacs 0.0.20 exposes the cache helper without an ELN load path, so checking
only the helper's existence is insufficient. Skipping native-only setup lets
early-init finish setting `user-lisp-auto-scrape` to nil: Emacs 31 startup
activates personal libraries without compiling them before Straight makes
their package dependencies available.

Neo Emacs 0.0.20 also draws the end-of-line cursor at the fill-column indicator
instead of point, even with the indicator's normal face height. Automatic
indicator activation is disabled only in Neo, identified by its
`neomacs-core-backend` primitive. GNU Emacs retains the indicator; `fill-column`
remains 80 and formatting settings are unchanged.

Agent Shell transcripts and screenshots for new sessions go into
`~/.local/share/emacs/var/agent-shell/<project-hash>/`, rather than
`.agent-shell/` in the project or home directory. Existing sessions retain
their cached transcript paths: leave their old directories intact until
those sessions finish. Historical transcripts are not moved automatically.

## Emacs 31 startup

Local `user-lisp/` libraries declared through `use-package` must specify both
`:straight nil` and `:ensure nil`. The first bypasses Straight; the second
bypasses package.el's global `use-package-always-ensure t` default. Without both,
startup can report an unavailable package even though the local library is
already on `load-path`. The task transport, proposal and capture declarations
are local-only and keep their command autoloads.

Fedora's `/usr/share/emacs/site-lisp/site-start.el` loads package-owned fragments
before the user init. Under Emacs 31, fragments missing a first-line binding
cookie warn independently of this configuration. The observed files belong to
`cmake-data` (`cmake-init.el`), `mercurial` (`mercurial-site-start.el`) and
`rpmdevtools` (`rpmdev-init.el`).

The semantics-preserving packaging fix is an explicit first-line cookie:

```elisp
;;; -*- lexical-binding: nil; -*-
```

That retains the fragments' existing dynamic binding. Switching them to lexical
binding is a separate code migration, not a warning-only fix. Isolated copies
with the explicit nil cookie loaded without warnings and preserved their
autoloads/hooks. The installed RPM-owned files are left unchanged; package
updates can overwrite local edits. Do not hide these warnings globally or
disable all site startup merely to silence them.

Modeline behavior tests do not assume a fixed terminal column width for the
flag glyph: Unicode width tables/fonts can change across Emacs versions.
Focus, hidden task titles, hover details and unavailable-task indications remain
covered.


## Package revisions

The version-7 Straight bootstrap and package manager use
`versions/default.el` from the version-controlled configuration directory.
Package checkouts and builds remain under `~/.local/share/emacs/straight/`;
only the lockfile lives in this repository.

New checkouts honor these revision pins. Existing repositories are not
forcibly reset during startup. To restore their recorded revisions, preserve
local changes first and explicitly run `M-x straight-thaw-versions`.

After intentional package updates, `C-u M-x straight-freeze-versions` records
the current revisions without Straight's push/check phase. Review local
changes separately: the lockfile records commits, not uncommitted patches.

## Native language modes

TOML files and `uv.lock` use Emacs's built-in `toml-ts-mode`, not the external
`toml-mode` package. The TOML tree-sitter grammar must be installed; on a new
machine, use `M-x treesit-install-language-grammar` and select `toml`.

YAML files (`.yaml` and `.yml`) use built-in `yaml-ts-mode` instead of the
external `yaml-mode` package and require the `yaml` tree-sitter grammar.

`CMakeLists.txt` and `.cmake` files use built-in `cmake-ts-mode` with the
`cmake` grammar.

Dockerfile, Containerfile, their dot-suffixed variants, and `.dockerfile`
files use built-in `dockerfile-ts-mode` with the `dockerfile` grammar.
The external mode's image-building helper commands are no longer configured.

Go source files use built-in `go-ts-mode` with the `go` grammar; `go.mod`
uses `go-mod-ts-mode` with the `gomod` grammar. The external `go-mode`
package is no longer configured, so `go.work` has no specialized mode.
Go language-server autostart policy is unchanged.

Rust files use built-in `rust-ts-mode` with the `rust` grammar and start
Eglot automatically. The server remains `rustup run stable rust-analyzer`,
with Clippy diagnostics. Eglot provides type inlay hints when rust-analyzer
supports them; Rustic is not required. Rustic-specific Cargo popup and
current-test commands are no longer configured.

C# keeps built-in `csharp-ts-mode` and uses `csharp-ls` instead of OmniSharp.
On this workstation, csharp-ls 0.28.0 is installed under
`~/.local/share/csharp-ls/`; `~/.local/bin/csharp-ls` launches it with the
private .NET 10.0.402 SDK under `~/.local/share/dotnet/10.0.402/`. The launcher
sets the SDK environment only for the server process; the system .NET 8 SDK
and ordinary `dotnet` commands are unchanged. Other machines need a compatible
[csharp-ls installation](https://www.nuget.org/packages/csharp-ls).

Lua files and Lua shebangs use built-in `lua-ts-mode` with the `lua` grammar.
Eglot's default Lua server is LuaLS; use `M-x eglot` to connect manually.
The dotfiles root's `.luarc.json` adds `/usr/share/hypr/stubs` to LuaLS's
workspace library for Hyprland API completion and navigation. This does not
change other projects or the handling of legacy Hyprlang `.conf` files.

On this workstation, the official LuaLS 3.19.2 Linux x64 release is installed
under `~/.local/share/lua-language-server/3.19.2/`, with a wrapper at
`~/.local/bin/lua-language-server`. The wrapper preserves access to the
server's bundled resources. Other machines need a working LuaLS installation;
see the [official installation guide](https://luals.github.io/#other-install).

## Python language tooling

Eglot uses `rass` to combine `ty server` and `ruff server`, taking both server
executables from `~/.local/bin/`. Type-checking and lint diagnostics reach
Flymake through Eglot; there is no separate `flymake-ruff` checker or obsolete
`pylsp` workspace configuration. Python target versions and rules come from
the project's ty/Ruff configuration, not a forced editor-side Python 3.13 flag.
Ruff format-on-save and the manual `SPC s i` isort command remain configured.

## Eshell completion

Eshell retains its completion-at-point provider. The obsolete Company idle
delay hook is removed; Eshell's Corfu configuration is unchanged.

## Org and editing packages

Org-modern styles Org buffers in place of Org-bars and Org-bullets. Built-in
Org indentation, visual line wrapping, agenda, capture, and clocking remain
configured. Org-roam is retained for knowledge-base and agent workflows.
Org-ref is no longer configured; existing documents are not converted, and
its citation-link syntax requires the package if needed later. Built-in
Org-cite remains available.

The unused custom async PDF export helper and missing async-init file
reference are removed. No export-only init file is added; synchronous Org
export remains unchanged.

The unused language-detection and Vimish Fold package declarations are
removed. Org/native folding is unaffected. Undo-tree, saved branching
history, and its visualizer binding are unchanged.

## HTTP requests

Verb replaces the old `http.el` client. Write a request under an Org heading
tagged `:verb:` and run `M-x verb-send-request-on-point-display` to send it.
Verb enables its minor mode when needed; no requests run at startup, no
global Org key prefix is changed, and existing request files are not converted.

## Editing and debugging integrations

Evil's normal-state `H`/`L` move between arguments and `K` leaves the current
argument list. Smartparens uses its upstream Python tree-sitter pair rules
instead of copied local definitions. Markdown JavaScript code blocks use
`js-ts-mode` for native highlighting.

Dape saves buffers through `dape-start-hook` and uses its default working
directory selection, including the fallback outside projects. GLSL's Eglot
contact launches `/usr/bin/glsl_analyzer` directly.

TypeScript, JSON, JSP, QML, and QML-project filename patterns match their
intended suffixes only. Typst's Eglot contact uses `tinymist` from `PATH`.
Key-chord retains all eight order/case variants of `jk` through four
registrations, since each registration already covers both key orders.

## Core packages and compatibility

Straight remains the package manager, but `use-package` and Which-key come
from Emacs itself. Native-compilation compatibility is left to Emacs and
Straight rather than a local alias for the old deny-list variable.

ECA uses its upstream Flymake compatibility guard; no fake global diagnostic
accessor is defined. C# highlighting uses the built-in mode's rules without
the inactive local replacement. gptel uses the selected backend's default
model rather than an ineffective `default-model` customization.

Short y/n prompts use `use-short-answers`, without replacing `yes-or-no-p`.
The shell environment is imported once on each platform. Doom Modeline uses
its Nerd Icons dependency, with no standalone All the Icons declaration.
Magit's branch arguments come from its Transient rather than a stale variable.

Catppuccin loads at startup, not from `server-after-make-frame-hook`. That hook
also runs when `emacsclient` reuses an existing frame, including Magit's commit
editor. Disabling and reloading the theme there caused a white frame flash.
New graphical frames inherit the active theme without this hook.
The startup fallback colors in `early-init.el` belong to `initial-frame-alist`,
not `default-frame-alist`: applying those colors to every new client frame
overrides the theme's default background and foreground.

Normal package warnings are visible rather than globally suppressed.
The effective `jsonrpc-default-request-timeout` remains 300 seconds; the
unused `jrpc-default-request-timeout` assignment is removed.

## Completion transfer

With Corfu completion active, `M-m` transfers completion to Consult in the
minibuffer. Choosing a candidate replaces the original completion region and
preserves the completion provider's metadata and exit callback.

Minibuffers use Corfu's built-in activation policy: local
`completion-at-point-functions` enable Corfu, while command-selection
minibuffers remain with Vertico. The broader custom activation hook is removed.

## Personal tasks (phase 1)

`user-lisp/task-workflow.el` implements the local personal-task pilot. Data lives in
`~/Projects/personal/tasks/tasks.org`, outside this dotfiles repository. The
task directory is created on first capture, not during startup. The personal
root must already exist; set `task-workflow-personal-root` before configuration to
select a different local personal root.

Use lowercase `SPC o t` for tasks; uppercase `SPC o T` retains clock/Org helpers:

| Key | Command | Behavior |
| --- | --- | --- |
| `SPC o t c` | `task-workflow-capture` | Snapshot the active region and provenance; without a region, open rough context. |
| `SPC o t m` | `task-workflow-capture-manual` | Explicit manual capture, without a model. |
| `SPC o t d` | `task-workflow-draft` | Open rough context for an explicitly generated, editable proposal. |
| `SPC o t t` | `task-workflow-select` | Select an unfinished task through Consult and visit it. |
| `SPC o t f` | `task-workflow-focus` | Focus the task at point, or choose one when outside a task. |
| `SPC o t g` | `task-workflow-visit-focused` | Return to the focused task by its ID. |
| `SPC o t x` | `task-workflow-clear-focus` | Explicitly clear focus. |

The previous clock/helper suffixes are preserved under uppercase `T`:

| Key | Command |
| --- | --- |
| `SPC o T i` | `org-clock-in` |
| `SPC o T o` | `org-clock-out` |
| `SPC o T r` | `org-clock-in-last` |
| `SPC o T t` | `org-clock-goto` |
| `SPC o T s` | `org-clock-display` |
| `SPC o T c` | `org-table-create` |

Capture and selection do not change focus, lifecycle, or clocking. Focus is
in-memory only and starts empty in a fresh Emacs. Completing a focused task
does not clear it; the focused-task command can still visit it for review.
Cancelling selection leaves focus and location unchanged. A missing task is
reported without silently clearing its focus identity.

Focusing a task shows a compact `⚑` icon in the active window's Doom modeline.
Inactive split-window modelines do not repeat it. Hover for the full task title
and lifecycle state; click with mouse-1 to return to its heading. With no focused
task, the icon is hidden. Capturing or selecting a task alone does not show it.

The indicator updates on focus/clear, lifecycle changes, task-file saves, and
focused-task navigation. Save a renamed heading to refresh its hover details.
A completed task keeps its icon until focus is explicitly cleared or replaced;
an unavailable task keeps its identity and shows a warning-colored icon with
the explanation in its tooltip.
Redisplay reads cached text and never scans task files.

Each task is an ordinary Org heading with its own `TASK: t` and `ID`
properties. Edit the body normally for outcome, completion criteria, resume
note, next action, and documentation; none is mandatory for capture.
Use Org's normal `C-c C-t` to change lifecycle:

| State | Meaning |
| --- | --- |
| `INBOX` | Captured, not yet clarified |
| `TODO` | Outstanding work |
| `NEXT` | A chosen next action |
| `TEST` | Verification remaining |
| `WAIT` | Waiting or blocked |
| `PAUSED` | Deliberately set aside |
| `DONE` | Completed |
| `CANCELLED` | No longer being pursued |

This vocabulary is task-file-local. Global Org TODO states and existing capture
templates are unchanged. The picker includes waiting/paused tasks and excludes
completed, cancelled, and archived headings. It respects unsaved buffer edits
without saving them. Selection has no navigation preview and does not persist
picker history.

Only marked headings in visible `.org` files immediately inside the personal
`tasks/` directory are included; unrelated TODOs, attachments, nested directories,
and Emacs lock files are excluded. Keep the `TASK` and `ID` properties when
renaming or refiling into another `.org` file in this directory. Duplicate or
missing task IDs are reported, not silently regenerated by selection. Missing
roots are reported as unavailable.

The existing task directory is added to the agenda at configuration or capture.
No trackers, sessions, worktrees, customer roots, or persistent focus are part
of this phase. Use the personal pilot before continuing the
[rollout plan](../docs/task-workflow-rollout.md).

Task regressions, from the dotfiles repository root:

```sh
emacs -Q --batch -l emacs/.config/emacs/early-init.el \
  -L "$HOME/.local/share/emacs/straight/build/compat" \
  -L "$HOME/.local/share/emacs/straight/build/consult" \
  -l emacs/.config/emacs/tests/task-workflow-tests.el \
  -f ert-run-tests-batch-and-exit
```


## Task drafting transport (increment A)

`user-lisp/task-workflow-gptel.el` provides isolated drafting requests through
gptel, independently of OMP and agent-shell. This is the transport gate in the
[drafting plan](../docs/task-capture-drafting-plan.md), not a new capture UI.
`SPC o t c` now uses the shared proposal flow; `SPC o t m` remains manual.

Run `M-x task-workflow-gptel-login` once and press RET at gptel's authorization
prompt. Complete the browser login; the browser may open behind Emacs. The
authorization URL is also copied to the clipboard. This uses the ChatGPT
subscription, with a separate gptel login and no API key or OMP token export.
Missing or expiring authentication makes drafting fail with a login instruction,
not open a hidden login prompt. Run the login command again to refresh it.

Credentials remain in gptel's native plaintext token cache under
`~/.local/share/emacs/.cache/gptel-openai/`, with directory mode 0700 and token
mode 0600. Task source material is not written there.

The explicit default model is `gpt-6.1-sol`, verified with this account.
`gpt-5.5` was advertised by the installed gptel backend but rejected by the
subscription endpoint with `model_not_found`. Model errors never trigger a
fallback. To use an API key instead, explicitly configure both
`task-workflow-gptel-backend` and `task-workflow-gptel-model` for a dedicated
OpenAI Responses backend; that alternative has not been live-account tested.
Backend curl argument overrides are rejected.

Adapter API:

- `task-workflow-gptel-draft SOURCE CALLBACK :root ROOT :owner BUFFER` returns
  an asynchronous request handle. ROOT must be an existing local owning root.
  Killing OWNER cancels only its request and discards late callbacks.
- CALLBACK receives the handle once, with state `done`, `failed`, or `cancelled`.
  Read the proposal with `task-workflow-gptel-request-result`, or the failure with
  `task-workflow-gptel-request-error`. Invalid input/setup signals an error before
  starting; callbacks do not run for failed setup.
- `:dry-run t` prepares and validates the real payload without authentication
  or network I/O. Dispose of it with `task-workflow-gptel-cancel`.
- `task-workflow-gptel-cancel HANDLE` stops only that request's curl process.

Each request has a fresh hidden buffer and a fixed system prompt and schema.
Only the explicit source string is sent: no chat history, tools/MCP, extra
context, or prompt transforms. Payload overrides that add tools or conversation
state, change source/instructions/model, or enable server-side storage are
rejected before sending. JSON proposals are validated as data; no Org or Lisp
from a response is executed. No task files are created.

Curl receives request content through stdin, not command-line arguments.
Request/response logging stays disabled for these requests even if ordinary
gptel logging is `debug`; that setting is left unchanged. Request buffers and
transport references are released on completion/cancellation. The caller owns
the returned proposal; do not log request handles or confidential results.

Live checks returned a structured photograph-organization proposal and proved
that cancelling one request left another running to completion, with no gptel
log buffer created under global debug logging. Deterministic regression coverage
also uses a local SSE server; the normal suite does not call OpenAI or require
credentials. The adapter uses the installed gptel 0.9.9.6 FSM/process APIs to
resume a validated dry-run and isolate cancellation/logging; recheck those gates
when upgrading gptel.

Increments B and C add the shared editable proposal and selection intake below.
Jira/Plane adapters and customer routing remain gated.

## Reviewed task proposals (increment B)

Use `SPC o t d` (`M-x task-workflow-draft`) to open a transient Org proposal.
Paste rough, multiline context into **Context / exact source** and optionally
describe your intent. The protected preview shows the personal destination,
provider and model. Opening a draft makes no model request and creates no task.
The normal personal destination remains `~/Projects/personal/tasks/tasks.org`.

The flow reuses one pane: source → draft → generated/revised candidate → saved
task. Earlier drafts remain available through normal buffer switching, with
their edits intact, but are not automatically displayed side by side. Unrelated
windows are preserved. If the request's original pane is no longer visible or
is dedicated, the candidate stays in the background and a message names it;
completion does not open a new split or steal another pane.
An isolated authenticated Emacs run verified capture → generate → revise →
accept with two panes throughout: one workflow pane and one unchanged unrelated
pane. Earlier candidate buffers and manual edits remained intact.


1. Press `C-c C-g` to generate. Context and intent freeze as the source snapshot.
2. Review the separate candidate buffer. Edit Title, Outcome, Description,
   Completion criteria, Next action and Questions inside their example blocks.
   Criteria and questions use one item per line. Block boundaries are protected;
   pasted Org-looking text stays quoted data.
3. For a revision, edit **Revision feedback**, then press `C-c C-g` again.
   The request includes the original snapshot, your edited proposal and feedback.
   Every response has a separate buffer, displayed in the proposal's existing
   pane; earlier candidates and edits made while waiting are never overwritten.
4. Press `C-c C-c` on the candidate you want to accept. This saves one INBOX task
   with a TASK marker, stable ID, reviewed wording and exact source/provenance,
   then visits it in that pane. Acceptance does not focus the task, start a clock or launch
   an agent. All candidates from this capture share one acceptance identity:
   repeated or sibling acceptance only revisits the saved task, never rewrites
   its notes or creates a second task.
5. `C-c C-k` cancels this buffer's pending request without saving a task. It
   leaves the buffer available for inspection, but generation/acceptance there
   are disabled. Killing a pending buffer also cancels its owned request.
   Other captures and sibling candidates are unaffected.

Only one request may be pending per proposal buffer. Accepting a capture cancels
its siblings' pending requests; late responses cannot resurrect it. Generation,
rendering and save errors retain source and edits for explicit retry. A failed
save retains its allocated ID, so another `C-c C-c` cannot duplicate an entry.
There is no automatic manual-capture fallback.

Drafts and feedback are not file-backed and have no autosave, backups or input
history. They are absent from task selection and the agenda until acceptance.
Source and model text remain inert example-block content, including Org
headings, drawers, Babel forms and links. Saved source JSON retains the exact
original text; model output cannot assign IDs, root ownership or lifecycle.
Manual capture and proposal acceptance use the same task writer.

Verified in live Emacs with the authenticated backend: payload inspection,
generation, revision after manual edits, acceptance, repeat acceptance, reopen
with exact source, cancellation and pending-buffer closure. The actual review
surface was inspected visually. Regression coverage includes failed save/retry,
late delivery, unsafe Org input and focused-modeline preservation.

Increment C extends this same proposal flow with selection snapshots and local
provenance; it does not introduce another generator or task writer.

## Selection capture and provenance (increment C)

Select text and press `SPC o t c` (`M-x task-workflow-capture`). This works with
unsaved file edits and non-file buffers, including Evil visual selections.
Without an active nonempty region it opens editable rough context, like
`SPC o t d`. `SPC o t m` is always explicit manual title-only capture.

The proposal shows the exact selection and protected **Source provenance**
before any model request. Selection Context is already frozen; Optional intent
remains editable until the first `C-c C-g`. Use the same generate/revise,
accept and cancel controls as B. Editing or closing the original buffer cannot
change the captured source, even before generation starts.

Provenance records buffer name, major mode, modified status and capture time.
File-backed selections also record their observed absolute path and inclusive
absolute start/end lines. Narrowing and selection direction do not change line
numbers; an end at the next line's beginning excludes that next line. Available
local project metadata includes project name/root and a project-relative file
label. Git metadata records the actual checkout root, current branch and HEAD
revision when available; detached HEAD omits the branch. Untracked files and
linked worktrees are supported. Missing file/project/Git facts are omitted.

Only the selected text is copied, without truncation. Collection does not add
surrounding text, other files, diffs, environment variables or chat history.
The source/provenance snapshot is application-owned and preserved exactly in
the accepted task's source JSON. Org-looking selections remain inert data.

Non-ASCII context, intent, feedback, filenames and project labels are preserved
in prompts, previews and saved source JSON. Native JSON serialization returns
UTF-8 bytes; these are decoded to Unicode before editor-buffer insertion.
The exact-payload isolation check remains enabled.

This is still personal intake. `task-workflow-capture-excluded-source-roots`
defaults to `("~/Projects/work/")`: direct work paths, symlink aliases and
non-file buffers rooted there are refused before generation. Remote selections
are refused without remote project/VC probes. This guard does not register
customer roots or authorize customer capture; Jira/Plane routing remains later.

Verification: 123 behavioral tests pass, including real Git/untracked/worktree
fixtures. Live Emacs exercised the actual Evil visual `SPC o t c` binding and
the source/provenance preview. An authenticated file-backed smoke changed and
closed the unsaved source before generation, accepted the result, reopened it,
and verified exact provenance, no surrounding-text leakage and one inert task.
Focus and the compact indicator remain unchanged.

## Agent Shell prompt submission

In insert state, `C-return` accepts the selected Corfu completion before
submitting the prompt. For example, `/ren` with `rename` selected submits
`/rename`, not the partial prefix. With no active completion or no selected
candidate, it submits the text as typed. This uses Agent Shell's normal submit
command, preserving its busy-session queue and prefix-argument behavior.
`RET` still inserts a newline, and `TAB` accepts completion without submitting.
Other modes' Corfu bindings are unchanged.

Context and prompt commands are available under the same leader prefix as
session navigation:

| Key | Command | Behavior |
| --- | --- | --- |
| `SPC c a` | `agent-shell-send-dwim` | Carry the region or relevant context to the project's last-accessed shell. |
| `SPC c r` | `agent-shell-send-region-to` | Select a shell to receive the region. |
| `SPC c f` | `agent-shell-send-file-to` | Select a shell to receive a file. |
| `SPC c p` | `agent-shell-prompt-compose` | Compose a prompt in a dedicated viewport buffer. |
| `SPC c s` | `agent-shell-prompt-steer` | Send a correction into the running turn, when supported by the agent. |

Prefer the explicit-target region/file commands when several conversations are
open. `C-c C-o` switches between the shell and its viewport; `C-c C-c` submits
from the composer. Ordinary busy-session submission queues work for later.
Steering changes the running turn instead and may interrupt it or replace its
current instruction; it is not an additional queued task.

## OMP session navigation

`SPC c c` (`consult-agent-shell-sessions`) switches between open conversations.
Its labels prefer OMP's native session title, then a descriptive buffer name,
then the agent's cached prompt. Long labels are collapsed to one line and
limited to 64 display columns. Status, mode, model, permission count, and
directory remain visible. Identical rows are distinguished by buffer name.

Activity status follows Agent Shell's current turn: Ready when idle, Working
while processing, and Waiting when blocked on a permission or question.
Retained tool-call records do not keep a finished turn marked Working.
Killed, starting, and unknown lifecycle states remain manager-controlled.

`SPC c n` (`agent-shell-omp-start`) offers saved sessions and new-shell choices.
Selecting an OMP session already open in this Emacs instance focuses its most
recently visited buffer instead of loading it again. Only the newly created
bootstrap buffer/client is discarded. Unopened sessions still load normally;
other agents and new-shell choices are unchanged. Existing duplicate sessions
are not merged or closed.

New OMP conversations are named automatically after their first successfully
completed ordinary prompt. Emacs waits for turn callbacks and queued user work,
then sends native `/rename` without arguments. OMP's title-generation models
generate and persist the name; ACP title notifications update both the manager
and Consult. Buffer names and unsent drafts are left untouched. No global
agent instruction or Emacs rename-tool call is needed.

Automatic naming runs once per newly created conversation. Restored/forked
sessions are not renamed, and a native title or an explicit `/rename` cancels
pending automatic naming. Cancelled/failed turns and slash-command replies do
not trigger it. If native generation fails or cannot identify a task, OMP's
normal failure output is shown; there is no repeated background retry.
Existing unnamed conversations are not renamed retroactively.

Use `/rename <title>` to choose a different saved title, or `/rename` to
generate a fresh one later. The session-context decorator leaves slash commands
untouched so OMP can dispatch them natively; ordinary prompts still carry the
exact ACP session ID for Emacs tools.

After editing the navigation libraries, reload both without restarting Emacs:

```elisp
(load "nyy-agent-shell-session" nil t)
(load "consult-agent-shell-sessions" nil t)
```

## Agent Shell file links

`RET` and mouse clicks on rendered local links open the file in Emacs.
Named fragments such as `emacs/README.md#personal-tasks-phase-1` are ignored
for file-level navigation, matching Markdown mode; they do not jump to a
named heading. Existing `#L123` and `#L123-L130` links still jump to lines or
select a range. Literal filenames containing `#` take precedence over fragment
stripping, and web URLs retain their normal browser behavior.

The local-fragment handler is installed by `nyy-agent-shell-session.el`.
Already-rendered links use it immediately; no conversation reload or
re-render is needed.

## OMP local artifacts

OMP `local://name` links refer to artifacts under
`~/.omp/agent/sessions/<project>/<timestamp>_<session-id>/local/name`, not
Agent Shell's transcript directory. `nyy-agent-shell-session.el` resolves them
using the originating shell's exact ACP session ID, including when followed
from its viewport. RET and mouse clicks use the normal Emacs file opener;
ordinary file and web links keep their existing behavior. Follow these links
in the session that produced them, since the same name can exist in multiple
sessions. Missing artifacts report an error instead of opening a browser.

If OMP uses a nonstandard session root, set
`nyy/agent-shell-omp-sessions-directory` to that root. To activate changes
without restarting Emacs, evaluate `M-: (load "nyy-agent-shell-session" nil t)`.
Existing rendered links use the updated handler without re-rendering.

## Agent AWS environment

`nyy/agent-shell-configure` sets `agent-shell-command-prefix` to
`nyy/agent-shell-without-aws`. Agents and commands launched through Agent Shell
lose inherited AWS profiles, access keys, session tokens, web-identity settings,
and container credential endpoints/tokens. Their AWS config and credentials files
are redirected to `/dev/null`, and EC2 metadata credential discovery is disabled.
Emacs and its normal direnv environment are unchanged; project MCP selection
through `AGENT_SHELL_MCP_EXTRAS` is preserved.

AWS MCP definitions must explicitly set their own `AWS_PROFILE`,
`AWS_CONFIG_FILE`, and `AWS_SHARED_CREDENTIALS_FILE` in their server environment.
Resolve those file paths in Emacs before starting the agent, using the Emacs
environment overrides or the absolute defaults `~/.aws/config` and
`~/.aws/credentials`. Otherwise the MCP child inherits the agent's `/dev/null`
paths. Keep customer-specific profiles in the private customer files.

Restart Emacs and start fresh agent sessions after changing this setup. Existing
agent processes retain their original environment. This prevents accidental
ambient credential discovery, not deliberate access to AWS files or privileged
Emacs/MCP tools; a separate OS user or sandbox is needed for that boundary.

## Agent uv credential stores

Agent commands use customer-specific **plaintext** uv credential stores instead
of the normal uv store or generic keyring. Normal terminals retain their existing
direnv-selected `UV_CONFIG_FILE`, keyring authentication, and publishing setup.
There are no project-specific uv configuration files to maintain.

Register the shared uv config in each trusted private customer Lisp file:

```elisp
(nyy/agent-shell-register-uv-credentials
 "~/.config/uv/uv_example.toml" "example")
```

Customer IDs must match `[a-z0-9][a-z0-9_-]*` in full. The exact expanded config
path selects `$XDG_DATA_HOME/uv/agent-credentials/example`, falling back to
`~/.local/share/uv/agent-credentials/example` when the data root is unset or
relative. Use the same data root for Emacs and the trusted refresher. Registration
is reset when private definitions reload; removed customer mappings do not linger.

Before building a local agent command, the prefix refreshes direnv for that
session's directory, not the last visited buffer. Unregistered configs, sessions
without an explicit config, and remote sessions select the reserved
`_unconfigured` store. Never put credentials there. Remote sessions do not import
local direnv or select a local customer's store.

The agent-only environment disables the subprocess keyring, redirects `NETRC`
to `/dev/null`, removes inherited named-index and publishing credentials and
index URL overrides, and filters native-auth preview environment settings.
Other tools in agent processes also lose `.netrc` discovery. Shared uv configs
must contain no passwords and must not enable `native-auth` or all preview
features: native storage bypasses `UV_CREDENTIALS_DIR`. Explicit command-line
overrides remain possible; this is ambient isolation, not a sandbox.

Keep a username in authenticated index URLs and use that **same username** in
the refresh configuration (`aws` for CodeArtifact; for Azure, match the username
already in the index URL). The credential store is scoped to the HTTPS origin,
not the Azure organization/feed URL path. Separate customer stores are required
even when two customers share `pkgs.dev.azure.com`.

Install the trusted host refresher and optional timers following
[the user-service instructions](../systemd/README.md#per-customer-agent-shell-uv-credentials).
Provision the dedicated CodeArtifact read role or establish host-side Azure
Artifacts sign-in first, refresh each store, then reload the private mappings
and start fresh agent sessions. Azure can reuse its credential provider without
a PAT. Neither agent startup nor direnv obtains cloud credentials or refreshes
tokens. Missing/expired credentials fail rather than falling back to the normal
keyring/store. A PAT is an optional narrower credential, not an Azure prerequisite;
when used, copying it on a timer does not renew its expiry.

Public regression tests use fictional customers. Separate stores prevent
accidental mixing but remain readable by the same Unix user; enforceable
cross-customer isolation still requires a sandbox or separate OS identity.


## Agent Shell and private customer configuration

`init.el` delegates Agent Shell setup to `user-lisp/nyy-agent-shell-config.el`.
That module owns public MCP defaults, keybindings, and hooks. Routing and
directory-policy enforcement live in `nyy-agent-shell-mcp.el`; session behavior
lives in `nyy-agent-shell-session.el`.

Private customer definitions are regular files outside Git in
`~/.local/share/emacs/etc/customers/*.el` (or
`$XDG_DATA_HOME/emacs/etc/customers/`). They are trusted executable Lisp, loaded
alphabetically. Keep this directory private and back it up separately; do not
symlink it into dotfiles. A missing directory is supported, but errors in an
existing file are reported rather than silently ignored.

Register each customer server using the following shape:

```elisp
(nyy/agent-shell-mcp-register-server
 '((name . "customer-mcp")
   (command . "example-mcp")
   (env . (((name . "EXAMPLE_TOKEN")
            (value . (lambda ()
                       (nyy/agent-shell-keyring-secret "EXAMPLE_TOKEN")))))))
 '("~/Projects/work/example/"))
```

Every registered private server requires selection through
`AGENT_SHELL_MCP_EXTRAS` in direnv; directory roots add a restriction, not an
automatic enablement rule. Roots are optional for servers without a directory
restriction. Restricted servers are local-only, resolve symlinks before checking
boundaries, and retain their policy when an agent overrides server definitions.
Credentials are fetched from the keyring only after selection, at server startup.
Remote sessions do not inherit local extras.

Customer endpoints, identities, profiles, OAuth metadata, and allowed roots belong
in these private files. Existing roots retain their current locations until the
project-directory migration; update the private files during that migration.
Public tests use fictional definitions in `tests/fixtures/customers/` and do not
load the real customer files or access the keyring.

After editing private definitions, restart Emacs or evaluate
`(nyy/agent-shell-configure-mcp)` for future sessions. This resets definitions
and policies before loading them again; it does not restart existing sessions.
This refactor removes customer definitions from the current tracked files, not
from earlier commits.

Emacs 31 activates `user-lisp-directory` itself. Automatic compilation and
autoload scraping are disabled because activation happens before Straight
makes package dependencies available. Emacs 30 adds the directory to
`load-path` explicitly instead; this fallback can be removed after upgrading.

The old individual library symlinks in `~/.local/share/emacs/` are no longer
needed. Old auto-save files are deliberately left intact: use `M-x
recover-session` or `M-x recover-file` to inspect/recover them before deleting
them. Existing generated directories under the config tree are ignored, not
deleted or migrated automatically.
