# Task workflow rollout

## Status and rollout rule

Phase 1 and phase 1A increments A–C are implemented: personal task records,
isolated gptel drafting, editable proposals, and selection/provenance capture.
Customer/tracker adapters and phases 2–6 remain gated.
Agent Shell keybindings and session selection are unchanged. See the
[personal task usage guide](../emacs/README.md#personal-tasks-phase-1).

Implement one phase at a time. Each phase ends with an exercised workflow,
updated usage documentation, and a review of actual use before the next phase
starts. Passing tests alone is not the acceptance gate. Do not start later
phases merely because their prerequisites have been implemented.

## Agreed model

A task is a named intention to achieve an outcome, with persistent context for
resuming it and deciding when it is finished. It is independent of trackers,
repositories, and agents.

- One Org heading is the authoritative local record, identified by a stable
  Org ID rather than its title, file, or heading position.
- Quick capture needs only a title and an owning root; missing detail can be
  refined later. IDs are generated. No external resource is required.
- Intent, lifecycle, current focus, and agent activity are distinct concepts.
- Outcome, completion criteria, resume note, next action, and free-form notes
  describe the work. Capture must not require completing every field.
- A task can relate to zero or more issues, agent sessions, repositories,
  working checkouts, documents, and wiki pages. It can also relate to other
  tasks through parent, dependency, and related-task links.
- Resource associations record kind, target, role, and an optional label.
  Assignment, reference, working context, and result are initial broad roles.
- A resource may belong to several tasks. An association neither synchronizes
  state nor authorizes disclosure to an agent or external service.
- Tracker status, Git state, and session activity remain owned by their
  respective systems. None automatically completes or cancels the local task.

Conceptual lifecycle distinctions are inbox, open, waiting, paused, completed,
and cancelled. Decide their mapping to existing Org TODO states during phase 1;
do not add another independent status field or replace the existing global Org
state vocabulary without a demonstrated need.

## Data ownership

Register roots explicitly; do not recursively discover arbitrary directories:

```text
~/Projects/personal/
~/Projects/work/<customer>/
```

Use the same proposed layout in each root:

```text
<root>/
  tasks/
    tasks.org                  authoritative task headings and notes
    files/<task-id>/           task-owned attachments and results
    .state/
      tracker-cache/           rebuildable issue snapshots
      agent-shell/             client transcripts and artifacts
      ...                      supported provider-owned state
  worktrees/<repository>/<task-id>/
  ...                          existing repositories stay in place
```

Create directories only when their features are used. Session-owned data is
not stored under one task ID because sessions can relate to multiple tasks.

Store local targets relative to their owning root where practical; preserve
stable IDs and URLs for external resources. A global root registry contains
configuration, not copies of task text or tracker responses. Global search
combines root-local records in memory. Missing roots are reported as
unavailable, not empty.

Task files, attachments, transcripts, native sessions, and uncommitted
worktrees are not disposable caches. Version control, backup destinations,
and synchronization are explicit per-root decisions. Do not add customer
content to the public dotfiles repository. Moving data across roots is an
explicit confidentiality-boundary operation, not an ordinary rename.

## Phase 1 — Personal task records and selection

### Deliverable

A useful local task notebook under the personal root, with no tracker, agent,
or worktree dependency.

- Manual quick capture into `tasks/tasks.org`, generating a stable Org ID.
- Ordinary Org editing for intent, notes, next action, and lifecycle changes.
- Consult selection of unfinished tasks, with meaningful lifecycle annotations.
- Selection visits the heading without changing state or launching anything.
- Explicit focus and clear-focus actions, using an in-memory task identity;
  merely viewing a task does not change focus, start a clock, or run an agent.
- A cached flag icon in the active window's Doom modeline identifies task focus;
  hover shows details. Viewing or capturing another task does not change focus.
- Existing capture templates, agenda behavior, and session navigation continue
  working. Use the existing Org completion/cancellation mechanisms.

### Acceptance gate

Capture and revisit real personal tasks, including one that has no external
resources. Exercise waiting/paused and completed/cancelled behavior. Move or
rename a heading and still find it by ID. Restart Emacs and recover the records
without any online service; focus need not survive restart. Test that unrelated
Org TODOs are not accidentally imported or have their state vocabulary changed.

Implementation verification used isolated task data: real Emacs/Vertico/Consult
selection of a paused task, focus preservation/navigation/clearing, and persisted
records read by a separate Emacs instance. The complete Emacs regression suite
passed 69 tests, including 16 task behavior tests. The focused-task indicator
was also exercised in the running Doom modeline. Commands use `task-workflow-*`
and task headings use `TASK: t`, with stable IDs preserved.
User review with real personal tasks remains the gate before phase 2; no example
tasks were placed in the personal data root.

### Stop or rollback

Disable the new entry points and remove the personal task root from the new
picker. The data remains ordinary Org and remains manually usable. Do not delete
captured tasks or alter pre-existing Org files to roll back.

## Phase 1A — Shared LLM-assisted capture

The [drafting implementation plan](task-capture-drafting-plan.md) defines the
next capture improvement: selection or rough context becomes an editable task
proposal through provider-independent gptel requests, preferring native ChatGPT
subscription OAuth rather than depending on OMP.

Implement its increments one at a time: prove isolated provider transport, build
the shared proposal/review/acceptance path, then add region capture and automatic
provenance. Keep explicit manual capture available. No task is saved before
acceptance, and generation does not change task focus or start implementation.

Increments A–C are active and verified. `SPC o t c` captures the active region
and local provenance, or opens rough context without a region. `SPC o t d`
is deliberately text-only drafting; `SPC o t m` is manual title capture.
Generation/revision and acceptance remain explicit. One writer handles both
paths; only acceptance allocates a generated task's stable ID. Revisions retain
prior editable candidates and share one acceptance identity. Work/customer
selections and remote sources are not enabled by personal intake.

The same LLM round and review UI are mandatory for new tasks imported from Jira
or Plane in phases 5–6. Tracker adapters supply normalized issue snapshots; they
must not implement a direct issue-to-task persistence bypass. Refreshing or
browsing issues makes no model request. Customer drafting remains gated by
explicit ownership and verified root-local content storage.


## Phase 2 — Resource associations and task navigation

### Deliverable

Attach existing resources and navigate among them; no provisioning yet.

- Structured resource associations without storing credentials in task files.
- Attach issues/wiki URLs, local files, repositories/checkouts, Org references,
  and existing OMP sessions. Support multiple resources of the same kind.
- Task-to-task links for parent, dependency, and related work; no automatic
  scheduling or completion propagation.
- Consult task actions for Org, original checkout, task checkout, session,
  browser, and Magit, offered when the corresponding resource is present.
- Reuse the existing session picker and session identity handling. Restore a
  saved session only through an explicit action; opening a task creates no
  session. Offer target selection when several sessions or repositories exist.
- A session can navigate back to its associated task or choose among several.

### Acceptance gate

Exercise the real Consult/action surface with a task having multiple sessions
and repositories, a non-coding task, and two tasks sharing a resource. Confirm
live-session reuse, explicit restoration, and navigation after an Emacs restart.
Move a heading and an isolated test root to verify ID/relative-path behavior.
Missing files or sessions must not erase their associations. Merely highlighting
or previewing a candidate must not send a prompt or open an external URL.

### Stop or rollback

The Org records and links remain. Disable the new actions; existing session and
repository commands remain available. Do not migrate or delete historical agent
files in this phase.

## Phase 3 — Root-local agent storage and customer pilot

### Prerequisite investigation

The current Agent Shell artifact function writes centrally under Emacs's data
directory. Native OMP sessions are separately owned by OMP. Before changing
routing, establish the actual provider storage controls and the effects on
restoration, authentication, and `local://` artifact links. Do not assume that
moving transcripts also moves native sessions.

Inventory content-bearing backups/autosaves and relevant tool logs as well.
Credential storage may remain in its existing secure mechanism; it must not be
copied into task files. Directory placement is not an OS security sandbox.

### Deliverable

- Route newly managed sessions and their supported content-bearing state to
  the explicitly selected owning root, for both ordinary checkouts and task
  worktrees. Reject ambiguous ownership rather than guessing.
- Preserve provider authentication, session restoration, artifact navigation,
  lazy credential resolution, AWS filtering, and project MCP restrictions.
- Validate personal storage first, then opt in one customer at a time for
  manual task capture and navigation. Tracker refresh is still out of scope.
- Root-local artifacts must not become a workaround for directory-policy checks.

### Acceptance gate

Start, use, close, and restore a fresh session under the owning root. Produce
and follow an artifact link. Observe where transcripts, native session data,
and tested backups/logs are actually written. Repeat in isolated customer
contexts and verify that each receives only its permitted MCP servers. Confirm
that the global task picker does not persist copies of customer contents.

Unsupported provider storage or unresolved content leakage blocks customer
agent-task rollout; document the concrete limitation rather than silently
weakening the agreed data boundary.

### Stop or rollback

Do not move files underneath live sessions. Existing unmanaged sessions retain
their known historical locations and are explicit exceptions, not evidence of
root-local storage. Record how to access them. On failure, stop creating new
managed sessions and leave recorded paths intact; do not silently reroute new
customer data centrally. Historical migration is a separately reviewed operation
once affected sessions have ended, not automatic phase cleanup.

## Phase 4 — Controlled task worktrees

### Deliverable

Explicitly provision execution contexts for an existing task, first personally
and then in customer roots that passed phase 3.

- Select repository, base revision, and a named task branch/worktree location.
- Allow a task to acquire several repository workspaces through repeated
  explicit actions; do not assume one task means one repository.
- Keep worktrees inside the owning root and separate from transcript storage.
- Do not copy the original checkout's uncommitted changes, credentials, or
  environment files implicitly. Do not automatically trust direnv configuration
  or expand a customer's MCP allowlist.
- Record each successfully created resource even if a later launch step fails;
  explain partial results instead of hiding them or deleting work.
- Start OMP and compose/send task context only through explicit actions. Preview
  included task text and resources; association alone does not disclose them.
- Review through Magit. Retire worktrees only after checking active sessions,
  dirty/untracked files, and branch integration, with explicit confirmation.
  No automatic merge, branch deletion, force-removal, or cleanup on completion.

### Acceptance gate

Run a small real task in a worktree and review its changes. Demonstrate multiple
workspaces for one task. Verify the selected base, original-checkout integrity,
correct context/MCP environment, restart recovery, and dirty/untracked cleanup
refusal. Exercise a launch failure without losing the created worktree. A task
can be completed while its worktree remains available for later review.

### Stop or rollback

Stop using provisioning commands. Existing branches and worktrees remain normal
Git resources and remain navigable from Org. Disabling automation removes none
of them.

## Phase 5 — Read-only Jira discovery and import

### Deliverable

Use the first customer's scoped Jira MCP server to discover and link work.

- Inspect the actual server's schemas and Emacs MCP client connection API.
  Reuse scoped definitions/credential handling without assuming an agent's
  connection is automatically available to the picker.
- Explicit asynchronous refresh into that customer's local snapshot. The
  Consult picker reads local data and remains usable offline.
- Configure the account/project/status query for that customer. Show remote
  issue state separately from local lifecycle and agent activity.
- Draft a new task from an explicitly selected issue through the shared LLM and
  review pipeline; persist only after acceptance. Recognize existing associations
  but allow several tasks to reference the same issue. Attaching a link to an
  existing task without rewriting it remains a separate relationship action.
- Keep imported description/metadata separate from user-authored notes. Refresh
  never overwrites notes or silently changes the task's lifecycle.

### Acceptance gate

Perform a real read-only refresh, then generate, edit, accept, and cancel issue
task proposals through the common drafting path. A generation failure creates no
task. Verify pagination/completeness, existing-link handling, notes preservation,
restart/offline use, and visible refresh errors. A failure must not replace
known tasks with an apparently authoritative empty list. Locally active tasks
remain visible even if the remote issue is completed or reassigned. No tracker
mutation or LLM request occurs during refresh/browsing; drafting is explicit.

### Stop or rollback

Disable Jira refresh. Existing local tasks and issue links remain usable. The
remote tracker has not been modified.

## Phase 6 — Read-only Plane discovery and combined use

### Deliverable

Add the second customer's scoped Plane integration using the same task and
navigation contracts, while honoring the actual server's different schemas.

- Inspect Plane tools and account/project/state filtering rather than assuming
  Jira behavior. Keep remote identities scoped to their instance/customer.
- Apply the same explicit refresh, import/link, notes preservation, and stale
  data behavior as phase 5.
- New Plane tasks use the same mandatory LLM proposal and explicit acceptance
  path as selection capture and Jira, not a tracker-specific task writer.
- Offer current-context and explicit all-context task views. Aggregate records
  locally; never send one customer's task contents to the other's server or
  session.
- Preserve local tasks with no tracker and tasks with multiple external issues,
  including public references. Do not infer status synchronization from links.

### Acceptance gate

Exercise a real read-only Plane refresh and proposal accept/cancel, then use the combined picker
across personal and both customer roots. Check duplicate human-readable issue
keys, independent refresh failures, disconnected roots, and no-tracker tasks.
Verify that each request uses only its intended connection/context. Review the
workflow in actual use before considering wider automation.

### Stop or rollback

Disable either tracker independently. Local tasks, navigation, and the other
integration continue working. No global persistent customer-content index needs
repairing or migrating.

## Verification and changes at every phase

- Use isolated fictional data for automated checks; never require customer
  credentials or live trackers in the normal test suite.
- Keep regression tests for consumer-visible behavior and boundaries, not copies
  of keybindings, source wording, or internal wiring.
- Exercise the changed Emacs/Consult surface and relevant real behavior, then
  document only what was actually verified and any remaining limitations.
- Update usage documentation after proof. Keep customer identities, URLs,
  credentials, and actual task records outside this repository.
- Back up authoritative records and non-rebuildable artifacts before migrations.
  Verify recovery using isolated data, not destructive experiments on real work.

## Not authorized by this rollout

Automatic issue transitions/comments/worklogs, automatic task completion,
calendar or time-tracking integration, automatic context sharing, unattended
agent orchestration, and historical bulk data migration are not implicit later
steps. Consider them separately only after the manual workflow proves useful.

## Current implementation boundary

Phase 1 plus phase 1A/A–C: personal task records, stable Org identities,
lifecycle-aware Consult selection, explicit in-memory focus, reviewed drafting,
and immutable selection snapshots with local buffer/project/Git provenance.
No customer data migration, tracker connection, agent launch, worktree creation
or unrelated Org refactor. Customer routing and tracker adapters remain gated;
later increments and phases do not start automatically.
