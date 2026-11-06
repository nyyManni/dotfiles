# LLM-assisted task capture

## Status and scope

Increments A–C are implemented and exercised: isolated gptel transport,
rough-description and selection proposals, local provenance, and explicit
review/acceptance. Tracker/customer adapters (D) and worktree provisioning remain
gated. Manual capture, task focus and the compact modeline remain available.

Build this as the next incremental capture improvement, with a review gate
between the increments below. Tracker discovery and worktree provisioning remain
separate rollout phases; this plan defines the drafting path tracker imports
must use when those integrations arrive.

## Required flow

```text
Selected text / rough description       Explicit tracker import
              |                                  |
   snapshot text + provenance          read-only issue fetch + provenance
              |                                  |
              +------------ normalized capture input ------------+
                                        |
                          context/provider preview
                                        |
                         isolated gptel drafting request
                                        |
                          editable Org task proposal
                                        |
                           explicit accept and save
                                        |
                         ordinary root-local Org task
```

Tracker imports must not have a second path that copies an issue title/body
straight into a new task. Selecting an issue to browse it or refreshing a task
list does not generate a draft. Explicitly requesting a task from that issue
starts the same LLM round as selection capture.

The model proposes wording; the application controls identity, ownership,
provenance, lifecycle, and persistence. Acceptance neither focuses the task nor
starts a clock, agent-shell session, worktree, or tracker mutation.

## Provider-independent request boundary

Use `gptel-request` for drafting. The task workflow must not depend on OMP or
launch an agent-shell conversation to generate a proposal. Provider/model
selection is task-specific configuration; the capture/review/persistence
pipeline is independent of that choice.

### Why gptel rather than agent-shell

Agent-shell is an ACP client. It can be driven programmatically using session
events, but still needs an external ACP agent, session lifecycle, and
agent-specific tool/context/storage controls. Keep it for interactive task
execution rather than introduce those dependencies into prose drafting.

gptel provides an asynchronous request API with an explicit prompt, system
message, callback, cancellation, dry-run inspection, and structured-response
support. A callback receives the result without automatically inserting it into
the source buffer. This is the smaller implementation for a bounded request.

Evidence: [agent-shell's ACP scope](https://github.com/xenodium/agent-shell/blob/14be85a5dfa9933c3a2970a83b24daea5c2b1a74/README.org#L18-L29)
and [gptel's request contract](https://github.com/karthink/gptel/blob/e7cebe4621533a036840d47c9741682bb687d808/gptel-request.el#L2081-L2245).

### Authentication choice

Prefer gptel's native OpenAI subscription backend, registered through
`gptel-make-openai-oauth`. Upstream documents ChatGPT Plus/Pro access through
this backend without an API key. The installed gptel v0.9.9.6 has both that
constructor and its OAuth backend library available.

This is a separate gptel login to the account, not automatic reuse of OMP's
token store. Use the supported OAuth login rather than exporting or copying
credentials. Do not change the global backend/model for existing gptel chats.

An API-key backend remains an explicitly configured alternative if preferred or
needed; credentials belong in the existing secure credential mechanism, not
task files or this repository. Never silently switch authentication/billing
routes after a failed subscription request.

Source: [gptel subscription authentication](https://github.com/karthink/gptel/blob/e7cebe4621533a036840d47c9741682bb687d808/README.org#L306-L335).
Increment A exercised native OAuth authentication and actual structured requests.

### Request isolation

Use a fresh dedicated request buffer and explicit prompt for each proposal or
revision. Do not use the source buffer as a gptel conversation or inherit the
focused task/session's history. Revisions explicitly supply the source snapshot,
relevant proposal, and feedback.

Set the drafting backend/model/system message explicitly. Disable tools,
additional gptel context, and inherited prompt transforms/presets that could
augment the request. Merely using a fresh buffer is not sufficient: the
installed gptel defaults include context/preset transforms, and the user's MCP
integration can register tools. Inspect the realized request before sending.

Pass request identity and callback state separately from LLM input. In
`gptel-request`, the CONTEXT keyword is callback metadata, not additional model
context. Prefer a complete-response callback initially; verify any backend
streaming/reasoning requirements instead of assuming one callback shape.

### First implementation gate

Using synthetic, non-confidential text, establish and exercise:

1. A dedicated approved backend/model, preferably subscription OAuth, without
   altering existing chats, exporting credentials, or invoking OMP.
2. Dry-run inspection showing only the intended prompt/system/context, with
   zero tool definitions or inherited conversation/preset additions.
3. One actual authenticated request producing a usable structured proposal.
   Check schema support and callback success/error/reasoning/abort behavior for
   the selected backend rather than assuming every model behaves identically.
4. Cancellation scoped to the owned request, not unrelated gptel chats; ignore
   late callbacks after cancellation, supersession, or buffer closure.
5. An explicit authentication/setup path. Missing credentials or expired login
   must produce an actionable error, not wait invisibly for interaction.
6. No confidential prompt bodies in command arguments, generic debug logs, or
   central autosave/history. Inspect transport logging and unavoidable temporary
   storage; content-bearing files must respect the owning root.

If a requirement is unmet, report it before shipping. No tool-enabled fallback,
automatic provider switch, or unnecessary CLI/ACP abstraction. The rest of the
drafting workflow uses this one request adapter.

## Common capture input

Keep an application-owned immutable input record separate from editable proposal
text. It contains:

- A transient capture/request identity, not a persisted task ID.
- The owning root/context and approved provider/model selection.
- Optional user intent, such as "investigate this" or "implement this".
- A list of source snapshots and their provenance. The first implementation may
  have one source; the contract must not assume one external issue per task.

### Selection source

Capture the region from the live buffer, including unsaved edits. Record the
buffer name, major mode, file path and selected line range when applicable,
modified-buffer status, and capture time. Record project identity/root and Git
checkout/branch when available through local project/VC information.

Send useful project-relative labels where possible. Do not automatically read
surrounding files, the whole buffer, Git diffs, environment variables, other
conversations, or unrelated customer configuration. Buffers without files or
projects are valid sources and must not acquire invented metadata.

Capture is a snapshot: editing or closing the source buffer while generation
runs does not change it. Do not silently truncate selected material.

### Tracker source

The adapter supplies the explicitly selected issue's fetched description and
relevant metadata: customer/instance, immutable issue ID, display key, URL,
project, remote state, and fetch time. Include only deliberately selected extra
material; do not automatically fetch all comments, attachments, or linked issues.
Normalize source formatting before drafting while preserving the original
snapshot for provenance.

The LLM cannot rewrite these identifiers, root ownership, or issue relationships.
Jira and Plane adapters fetch and normalize data; neither owns a separate prompt,
proposal renderer, review UI, or task writer.

## Model output and Org proposal

Ask for a structured response with:

- Suggested title.
- Outcome and relevant description.
- Completion criteria, when supported by the input.
- A concrete next action, when known.
- Open questions or assumptions needing user judgment.

Do not ask the model to generate task IDs, TASK properties, file paths to write,
tracker transitions, assignments, or due dates not present in the input. Source
text is untrusted material to transform, not instructions to execute. Selecting
code alone may leave intent unclear; surface that rather than inventing a change.

Decode and check the response shape, then render the editable Org proposal in
Emacs. Do not treat arbitrary model output as executable Lisp, shell commands,
file-local variables, or authoritative Org metadata. The renderer owns the
single task heading and section boundaries and safely quotes captured source
material, including Org-like headings/drawers/block delimiters.

Keep source snapshots/provenance separate from the generated description.
Preserve them with an accepted task under its owning root; do not maintain a
second central task database. Source metadata is application-owned even while
the user edits the proposed wording.

## Review UI and acceptance contract

Implemented interaction:

- `SPC o t c` (`task-workflow-capture`) snapshots the active region and local
  provenance. Without a region, it opens editable rough context.
- `SPC o t d` (`task-workflow-draft`) deliberately opens rough context even
  when a region is active. No request is made until explicit generation.
- `C-c C-g`: generate, or revise with the current edited proposal and the
  multiline Revision feedback field. Every result is a separate candidate buffer.
- `C-c C-c`: accept a ready candidate and visit the saved task. All candidates
  from one capture share one acceptance identity.
- `C-c C-k`: cancel this buffer and its pending request without saving; keep
  the buffer for inspection. Killing the buffer also cancels its request.
- `task-workflow-capture-manual` is explicit title-only capture on `SPC o t m`.

Suggested internal request states are captured, generating, ready, failed,
cancelled, and accepted. They are not Org task lifecycle states. A pending draft
is not a task and is absent from task selection/agenda/focus.

Allow only one pending generation per proposal buffer. Tag callbacks with the
request identity; ignore late results after cancellation, buffer closure, or
supersession. Regeneration/revision must not replace manual edits: keep the
previous proposal intact and present the new candidate separately.
Separate candidates do not require separate windows: capture, each response and
the accepted record reuse the proposal pane. Completion may update a visible
unselected owner pane without selecting it; hidden/dedicated owner panes leave
the candidate in the background. Existing draft buffers and unrelated windows
are retained. This window policy is local to drafting, not general task navigation.
An isolated authenticated Emacs smoke exercised capture → generation → revision
→ acceptance with two panes throughout, retaining the unrelated pane and earlier
candidate edits. Existing manually arranged live windows are not auto-collapsed.

On failure, preserve source and edits, show an actionable error, and let the user
explicitly retry or choose manual capture. An unavailable LLM must not cause a
tracker issue to be silently saved as an unreviewed task.

Acceptance uses one shared persistence path with explicit manual capture. It
creates one INBOX task, TASK marker, and stable Org ID only after acceptance.
Repeated acceptance must not duplicate the task. A save failure preserves the
draft and any allocated identity for a deliberate retry rather than inserting
another heading. Never overwrite an existing task's notes on tracker refresh;
proposed rewrites to an existing task require their own explicit review.

## Data ownership and confidentiality

Provider/account selection defaults to the user's ChatGPT subscription through
gptel OAuth, with an explicitly configured API backend as an alternative.
Keep local customer boundaries explicit even when roots use the same account.

Resolve the owning root before generation or persistence. Do not infer the
storage destination from the currently focused task or whichever buffer happens
to be selected when the result arrives. A customer source must never silently
become a personal task.

Start the pilot with personal sources. Customer capture requires explicit root
registration and verified root-local drafting storage; coordinate that with the
main rollout's storage/customer gate. This does not authorize moving existing
agent sessions or weakening MCP directory policies.

Unaccepted drafts and revision history are transient by default. Disable generic
central autosave/history of their contents. If transport requires temporary
files, use restrictive permissions inside the owning root's task state area and
remove owned temporary material after the request. Do not place confidential
context in command lines, general debug logs, or this repository. Persistent
accepted source snapshots belong with the task's root-local records.

## Implementation increments

### A — Prove isolated gptel drafting requests

Configure the dedicated backend and exercise the gates above with harmless
sample text: dry-run isolation, a real authenticated structured response,
request-scoped cancellation, and explicit failure reporting. No task records
are created. Do not export credentials, invoke OMP, or alter global chat settings.

Implemented in `task-workflow-gptel.el`. The dedicated subscription backend uses
native gptel OAuth with explicit login/refresh, not OMP credentials. The explicit
model is `gpt-6.1-sol`; a real structured response succeeded. The installed
backend advertised `gpt-5.5`, but the account rejected it with `model_not_found`;
there is no automatic model fallback.

The adapter prepares every request as a dry-run and validates its source,
instructions, model, tool-free envelope, and `store: false` before resuming it.
Streaming is internal: callers receive one validated final proposal, not partial
JSON or reasoning. Owner death/cancellation discards late results. A live
two-request exercise confirmed that cancelling one curl process left the other
running through a successful response.

The live debug-logging check exposed a cross-buffer request-config leak in the
first adapter implementation. Synchronous transport setup and asynchronous
process callbacks now have isolated logging scopes. The corrected live check
created no gptel log buffer with global debug enabled, left that setting intact,
kept source/credentials out of process arguments, and released both workers.
The normal regression suite uses synthetic content and a local SSE server,
not provider calls.

Manual capture/focus/modeline behavior remains unchanged. No task records were
created by these checks. The optional API-key backend configuration has not been
live-account tested. Reverify gptel's internal FSM/process compatibility on
package upgrades before relying on the isolation guarantees.

### B — Shared editable proposal and acceptance

Add the common capture/proposal contract, Org review buffer, generation/revision
controls, and one task writer. Make rough-description drafting usable end to end
with the verified provider. Keep manual capture explicit and preserve existing
task/focus/modeline behavior.

Gate: inspect sent context, generate, manually edit, accept, revisit the saved
record, and cancel another proposal without creating a task. Exercise delayed
responses, closing a pending buffer, malformed output, failed save, repeated
acceptance, and revision after manual edits without losing the original.

Implemented in `task-workflow-draft.el`, using `task-workflow-save-entry` for
both generated proposals and `task-workflow-capture-manual`. The common capture
record fixes root/backend/model at opening and freezes source/intent at first
generation. Protected editable fields retain literal text while quoting Org
delimiters; accepted source JSON preserves the original snapshot exactly.
All candidates share one persistence entry. Failed writes preserve its allocated
ID and roll back only the owned insertion; committed writes followed by hook
errors resolve the same ID on retry without replacing notes.

Live Emacs verification covered exact sent context, real generation/revision,
edits during a pending request, acceptance, repeated acceptance, reopening exact
saved provenance, cancellation and closing a pending owner. The actual proposal
surface was visually inspected. A root-frozen save-hook bug found during that
exercise was corrected: focused-task refresh uses the user's configured context,
not a different draft destination. Focus and indicator properties were unchanged
after the corrected acceptance.

All 103 behavioral tests pass, including malformed/delayed responses, cancelled
and closed owners, manual-edit preservation, save failure/retry, shared
acceptance, inert Org input and exact source JSON. No region capture, tracker
fetch, customer routing, agent launch, clock action or worktree creation was added.

### C — Selection capture and provenance

Add active-region intake and automatic local buffer/project/VC metadata. Use
exactly the same generator, proposal UI, and acceptance path as B. Change the
normal capture binding only when this path is ready; keep manual capture readily
available.

Gate: capture unsaved file text, a non-file buffer, and a project selection;
change/close the source before the result returns; verify the original snapshot
and provenance survive acceptance. Verify the compact indicator and focus do
not change. Treat selected Org-like content as data, not extra task metadata.

Implemented in `task-workflow-capture.el`, feeding the existing proposal via
`task-workflow-draft DESCRIPTION :sources SOURCES`. The selection is copied at
capture time, including unsaved edits, and its provenance is protected in the
preview. Optional intent remains editable until Generate. Rough-description
capture keeps its B behavior. No session/input struct layout was changed.

Collection uses local project/VC APIs for project identity, relative file labels,
actual Git checkout/branch and HEAD revision. Line ranges are absolute and
inclusive, including narrowed/reversed selections and end-at-BOL boundaries.
Non-file, untracked and linked-worktree sources are covered; absent facts are
not fabricated. The intake guard excludes `~/Projects/work/` by default,
including symlink aliases, and refuses remote selections before metadata probes.
It does not enable customer capture.

Verification: all 123 behavioral tests pass. Live Emacs exercised Evil visual
capture through `SPC o t c` and the actual provenance/source preview. A real
authenticated file-backed smoke changed/closed an unsaved source before
generation, accepted the resulting task, reopened it and verified the exact
snapshot/provenance. Surrounding text stayed out; Org-like source created no
extra tasks or executable blocks. The modeline and task focus stayed unchanged.
Selection metadata endpoints are read-only before change hooks, so rejected
edits cannot disable later quoting; that boundary regression is covered.
JSON preview/prompt/persistence boundaries decode native `json-serialize` UTF-8
bytes into Unicode text before buffer insertion. Unicode context and provenance
regressions cover the real gptel payload guard and saved/reopened records.
The exact-input guard is not relaxed to accommodate encoding differences.

### D — Customer routing and tracker adapters at their rollout gates

Enable customer sources only after ownership/storage checks pass. When Jira
and Plane discovery are implemented, their explicit task-import actions submit
normalized inputs to the already working shared drafting pipeline. No duplicate
tracker-specific task creation implementation.

Gate: demonstrate both tracker sources through generation, edit, accept, and
cancel. Verify issue identity/provenance are retained, manual notes are preserved,
no tracker writes occur, and the correct root/provider is used. A refresh or
browsing action makes no LLM call. An import generation failure creates no task.

## Code boundaries and verification

Keep the Org task store/navigation in `task-workflow.el`. Put proposal/review
behavior in a focused drafting library and gptel request handling in a small
adapter library. Tracker adapters stay with their integrations. Refactor the current
capture writer once; migrate callers/tests instead of retaining parallel writers
or compatibility aliases.

Use deterministic isolated tests for snapshot correctness, root ownership,
response/Org boundaries, asynchronous lifecycle, edit preservation, and
single-acceptance behavior. Keep live model calls out of the normal test suite.
Each increment also needs a real Emacs surface exercise; the transport gate
requires one actual authenticated provider smoke, not just mock output.

This plan does not authorize issue write-back, implementation agents, automatic
worktree creation, automatic focus, or automatically accepting generated tasks.

For live library updates, do not `unload-feature` a derived proposal mode while
its buffers exist: Emacs reverts them to the parent mode and clears their local
session state. Evaluate updated definitions in place, or restart after closing
or accepting drafts. Source text surviving unload is not sufficient to preserve
the proposal lifecycle.
