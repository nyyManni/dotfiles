---
name: create-ai-knowledge-base
description: Create and manage a project knowledge-base which is Agent-friendly
---

# AI Knowledge Base Creation Guide

A step-by-step guide for an AI agent to create a structured, navigable knowledge base for a software project. The knowledge base enables AI agents to efficiently understand the codebase without reading unnecessary amounts of source code.

## Purpose

The knowledge base is a hierarchy of small Markdown files that document the project's architecture, features, and conventions. It serves as a map that lets AI agents:

- Navigate to relevant documentation without reading all source code
- Understand how features work end-to-end (vertical slicing)
- Understand individual modules in detail (horizontal slicing)
- Minimize context window usage through targeted file reads

## Core Principles

1. **Small files** — Target 20-40 lines per file. Longer (up to ~80-120) for cross-cutting feature docs. The cost of tool calls to traverse the knowledge base is much less than bloating context by reading unnecessary code.
2. **Index-driven navigation** — Every directory has an `_index.md` that is a table of contents only, never substantive documentation.
3. **Progressive detail** — Top-level files give broad overviews; deeper files give concrete specifics (field names, method signatures, query patterns).
4. **Cross-references everywhere** — Files link to related files using relative Markdown links. Every leaf file ends with a `## Related` section.
5. **Two documentation axes** — Architecture docs slice horizontally (by layer/module). Feature docs slice vertically (tracing one feature across all layers).

---

## Mono-Repo Consideration

If the repository contains multiple distinct projects (e.g., a mono-repo with separate backend, frontend, mobile app, or microservices), create a separate knowledge base subtree for each project:

```
ai-knowledge-base/
├── _index.md                  # Lists all projects, links to each project's _index.md
├── project-alpha/
│   ├── _index.md              # Root index for project-alpha (same structure as Step 3)
│   ├── overview/
│   ├── architecture/
│   ├── features/
│   └── ...
├── project-beta/
│   ├── _index.md              # Root index for project-beta
│   ├── overview/
│   ├── architecture/
│   ├── features/
│   └── ...
└── shared/                    # Shared libraries, protocols, or common code
    ├── _index.md
    └── ...
```

The root `_index.md` in this case is a simple project listing:

```markdown
# [Repo Name] Knowledge Base

Mono-repo containing [N] projects.

## Projects

- [project-alpha/](project-alpha/_index.md) — [One-sentence description]
- [project-beta/](project-beta/_index.md) — [One-sentence description]
- [shared/](shared/_index.md) — Shared libraries and protocols
```

Each project's `_index.md` then follows the standard structure from Step 3 below. Cross-project references use relative paths (e.g., `../../shared/protocol.md`).

For single-project repos, skip this and use the flat structure described in the rest of this guide.

---

## Step 1: Analyze the Project

Before creating any files, thoroughly explore the codebase to understand:

- **Tech stack** — Languages, frameworks, databases, external services
- **Architecture** — How the code is organized (layers, modules, packages)
- **Major features** — What the application does from a user perspective
- **Key conventions** — Naming, error handling, testing patterns
- **Entry points** — Where the application starts, how requests flow

Produce a mental model of:
- The directory structure of the source code
- The dependency graph between modules
- The 8-15 major features the system provides

---

## Step 2: Plan the Directory Structure

Create a directory structure that mirrors the project's architecture plus cross-cutting concerns. Adapt the sections to fit the project — not every project needs every section. Include what is relevant and omit what isn't.

Example structure (adapt as needed):

```
ai-knowledge-base/
├── _index.md                  # Root navigation
├── overview/                  # High-level project docs
├── architecture/              # Code structure, mirroring source layout
│   ├── [layer-or-module]/     # One dir per architectural layer/module
│   │   ├── [sub-module]/      # Nested as needed
├── frontend/                  # Frontend-specific docs (if applicable)
│   ├── components/
│   ├── services/
│   ├── types/
│   └── hooks/
├── features/                  # Cross-cutting feature docs (FLAT)
├── external/                  # External deps, plugins, protocols
├── config/                    # Build, deployment, configuration
├── testing/                   # Test strategy, conventions, test map
├── decisions/                 # Architectural decision records
└── changelog/                 # Chronological change log
```

**Rules:**
- Architecture directories should mirror the source code structure
- The `features/` directory is always flat (no subdirectories)
- Every directory gets exactly one `_index.md`
- Use `kebab-case` for all directory and file names
- Omit sections that don't apply (e.g., `frontend/` for a pure backend service, `external/` if there are no standalone plugins)

---

## Step 3: Create the Root Index

Create `ai-knowledge-base/_index.md` as the entry point. List only the sections you actually created:

```markdown
# [Project Name] Knowledge Base

Granular, multi-index documentation for [one-sentence project description].

## How to Use This Knowledge Base

1. **Start here** — read this root index to find the relevant section
2. **Follow indexes** — each folder has an `_index.md` that describes its contents
3. **Read only what you need** — leaf files are small (20-40 lines); only open the ones relevant to your task
4. **Cross-references** — files link to related docs via relative paths; follow links for context

Do NOT read all files. Navigate the index hierarchy to find the minimum set of files needed.

## Sections

### Overview
- [overview/](overview/_index.md) — Project summary, architecture overview, conventions

### Architecture
- [architecture/](architecture/_index.md) — [Description of how code is organized]

### Features
- [features/](features/_index.md) — Cross-cutting feature docs tracing full stack

### [Other Sections as Applicable]
- [section/](section/_index.md) — [Description]
```

---

## Step 4: Create the Overview Section

Create `overview/_index.md` and a few leaf files covering the essentials. Include what makes sense for the project:

**Typical leaf files:**
- `project-summary.md` — Tech stack table, supported platforms, core features list, data storage summary
- `architecture-overview.md` — How the codebase is structured, key directories, dependency rules
- `coding-conventions.md` — Naming, import style, error handling patterns
- `data-flow.md` — How data moves through the system
- `error-handling.md` — Error types, propagation patterns
- `dependency-injection.md` — How components are wired together

**Template for `overview/project-summary.md`:**

```markdown
# Project Summary

[One-sentence description.]

## Tech Stack

| Layer | Technology |
|-------|-----------|
| Frontend | [framework, language, UI library] |
| Backend | [language, framework] |
| Storage | [database, file storage] |
| [Other] | [relevant tech] |

## Core Features

- **[Feature 1]**: [One-line description]
- **[Feature 2]**: [One-line description]
- ...

## Data Storage

[Where and how data is stored — databases, files, caches, etc.]

## Related
- [architecture-overview.md](architecture-overview.md) — How the codebase is structured
- [data-flow.md](data-flow.md) — How data moves through the system
```

---

## Step 5: Create Architecture Docs

This is typically the largest section. Mirror the source code's module/package structure.

### 5a: Create directory structure

For each major architectural division in the source code, create a corresponding directory. Examples:

- Clean architecture: `domain/`, `application/`, `infrastructure/`, `api/`
- Feature modules: `auth/`, `payments/`, `notifications/`, `users/`
- Layer-based: `models/`, `services/`, `controllers/`, `repositories/`

### 5b: Create `_index.md` for each directory

**Template for architecture `_index.md`:**

```markdown
# [Section Name]

[One-sentence description of what this section contains.]

## [Optional Grouping]
- [entity.md](entity.md) — [One-line description]
- [subdirectory/](subdirectory/_index.md) — [One-line description]
```

Group items logically (e.g., "Core Entities", "Authentication", "Payments") when a directory has more than ~8 files.

### 5c: Create leaf files for each module/entity

Read the source code for each module and create a focused documentation file.

**Template for entity/model docs:**

```markdown
# [EntityName]

[One-sentence description.]

## [SubType or Section]

Key fields:

| Field | Type | Purpose |
|---|---|---|
| `field_name` | `Type` | What it does |

## Source

`path/to/source/file.ext`

## Related
- [RelatedEntity](related-entity.md) — How they relate
- [Service](../services/entity-service.md) — Primary consumer
```

**Template for service/logic docs:**

```markdown
# [ServiceName]

[One-sentence purpose.]

## Dependencies

| Dependency | Purpose |
|---|---|
| `DependencyName` | What it's used for |

## Key Operations

### [Operation Group]
- `method_name` — What it does. Notable edge cases or behavior.

## Related
- [Entity](../models/entity.md) — Data type it operates on
- [Repository](../repositories/entity-repo.md) — Storage implementation
```

**Template for repository/data-access docs:**

```markdown
# [RepoName]

[One-sentence description.]

## Key Operations

| Method | Query Pattern | Notes |
|---|---|---|
| `find_by_id` | `SELECT ... WHERE id = ?` | Returns Option |
| `create` | `INSERT INTO ...` | Generates UUID |

## Source

`path/to/source/file.ext`

## Related
- [Interface](../ports/entity-port.md) — Interface it implements
- [Service](../services/entity-service.md) — Primary consumer
```

---

## Step 6: Create Frontend Docs (if applicable)

Skip this step if the project has no frontend, or if the frontend is a separate project in a mono-repo (in which case it gets its own knowledge base subtree).

Mirror the frontend source structure. Typical subdirectories:

- `components/` — Organized by feature area (e.g., `notes/`, `calendar/`, `layout/`, `common/`)
- `services/` — API client functions
- `types/` — TypeScript interfaces/types
- `hooks/` — Custom hooks
- `contexts/` — React contexts or state management

**Template for component docs:**

```markdown
# [ComponentName]

[One-sentence purpose.]

## Source
`src/components/path/ComponentName.tsx`

## Props / State
- `propName` — Description
- Key state: `stateName` — What it tracks

## Behavior
- [Key user interaction or rendering behavior]
- [Conditional logic]

## Related
- [OtherComponent](other-component.md)
- [Service](../../services/service-api.md) — Data source
```

**Template for frontend service docs:**

```markdown
# [Service Name]

API client for [feature].

## Functions
- `functionName(params)` -> `ReturnType` — Description
- `otherFunction(params)` -> `ReturnType` — Description

## Related
- [Types](../types/feature-types.md) — Request/response types
- [Backend commands](../../architecture/api/commands/feature-commands.md)
```

---

## Step 7: Create Feature Docs (Cross-Cutting)

This is the most important section for understanding how features work end-to-end. Create one file per major user-facing feature. The `features/` directory is always **flat** (no subdirectories).

Feature docs are the **bridge** between the horizontal (architecture) and vertical (feature) documentation axes. They should be 60-120 lines — longer than other leaf files because they trace the full stack.

**Template for `features/[feature-name].md`:**

````markdown
# [Feature Name]

[One-sentence full-stack summary of the feature.]

## Data Flow

```
UI ([ComponentNames])
  -> Frontend service ([service].ts)
    -> [IPC/API mechanism]
      -> Backend handler ([handler])
        -> Service ([Service])
          -> Repository ([Storage])
```

## [Key Concept / Storage Model / Domain Rules]

[Feature-specific explanation of the core domain logic, storage design, or key concepts. 5-15 lines.]

## Key Operations

### [Operation Group]
- **[Operation]** — [How it works end-to-end, mentioning relevant layers.]

## Frontend Components

| Component | Role |
|-----------|------|
| [Component](../frontend/components/area/component.md) | What it does |

## Architecture Layers

| Layer | Key Files |
|-------|-----------|
| [Layer 1] | [Entity](../architecture/layer1/entity.md) |
| [Layer 2] | [Service](../architecture/layer2/service.md) |
| [Layer 3] | [Repo](../architecture/layer3/repo.md) |
| [Layer 4] | [Handler](../architecture/layer4/handler.md) |
| Frontend | [API](../frontend/services/api.md), [Types](../frontend/types/types.md) |

## Related
- [Other Feature](other-feature.md) — How they interact
- [Decision](../decisions/relevant-decision.md) — Why it was designed this way
````

Adapt the template to the project. A backend-only service won't have a "Frontend Components" table. An event-driven system might replace "Data Flow" with a message flow diagram. The key is that each feature doc shows the **complete path** through the system.

---

## Step 8: Create Remaining Sections

Include whichever of these sections are relevant to the project.

### Decisions (`decisions/`)

One file per significant architectural decision, using ADR (Architecture Decision Record) format:

```markdown
# [Decision Title]

[One-sentence context.]

## Decision
[What was decided.]

## Rationale
- [Why — bullet points]

## Alternatives Considered
- **[Alternative]** — [Why it was rejected]

## Related
- [Implementation docs](../architecture/relevant/file.md)
```

### Config (`config/`)

Document build configuration, dependency management, scripts:

```markdown
# [Config Topic]

## [Section]
| Item | Value/Purpose |
|------|-----------|
| `config_key` | What it controls |

## Source
`path/to/config/file`
```

### Testing (`testing/`)

Document test strategy, conventions, how to run tests, test file locations.

### External (`external/`)

Document external dependencies, plugins, protocols, or standalone components that aren't part of the main application code.

### Changelog (`changelog/`)

Chronological log of significant changes. Name files with timestamps: `YYYY-MM-DD_HHMM_short-description.md`.

```markdown
# [Change Title]

## Summary
[One paragraph.]

## What Changed
- [Bullet points of changes]

## Files Modified
- `path/to/file` — what changed
```

---

## Step 9: Validate Cross-References

After creating all files:

1. Verify every `_index.md` links to all its leaf files and subdirectories
2. Verify every leaf file has a `## Related` section with 2-8 links
3. Verify links are bidirectional where it makes sense (if A links to B, B should link back to A)
4. Verify relative paths are correct (count the `../` carefully)
5. Verify feature files link to all relevant architecture layer files

---

## Step 10: Wire into AGENTS.md

Add instructions to `AGENTS.md` (or equivalent agent configuration file) so that AI agents know to use the knowledge base. This is critical — without these instructions, agents won't know the knowledge base exists.

### Knowledge Base Reference Section

```markdown
## AI Knowledge Base

Detailed codebase documentation lives in `ai-knowledge-base/`. The `_index.md` files
are navigation indexes only — the actual documentation is in the leaf `.md` files they
link to. To research a topic:

1. Start at [ai-knowledge-base/_index.md](ai-knowledge-base/_index.md) to find the
   relevant section
2. Follow `_index.md` files in subfolders to narrow down to the relevant leaf docs
3. **Read the actual leaf `.md` files** (e.g., `notes.md`, `services.md`,
   `repositories.md`) — these contain the detailed documentation you need. Do NOT
   stop at `_index.md` files; they are just tables of contents.

Key sections: [list sections with links to their _index.md files]
```

### Behavioral Rules

Add these to the agent's core rules or "must always take into account" section:

```markdown
- **When researching the codebase, always navigate the `ai-knowledge-base/` first
  (starting from `_index.md`, then following relevant `_index.md` files in subfolders)
  to identify which parts of the codebase are relevant before reading any actual source
  code. Do not go directly to source files — use the knowledge base to orient yourself
  first. CRITICAL: `_index.md` files are just tables of contents — you MUST read the
  actual leaf `.md` files they link to to get the real documentation. Do not stop at
  `_index.md` files.**
- **When updating the codebase, always update the AI Knowledge Base after the changes.**
- **When delegating work to subagents, always instruct them to navigate the
  `ai-knowledge-base/` first to identify relevant parts of the codebase before reading
  source code. Instruct them that `_index.md` files are just tables of contents — they
  must read the actual leaf `.md` files to get real documentation.**
- **Always update the changelog in ai-knowledge-base.**
```

---

## Maintenance Rules

Once the knowledge base is created, keep it accurate:

1. **After every code change** — Update the relevant leaf files to reflect the new state
2. **After adding/removing source files** — Update `_index.md` files and relevant leaf docs
3. **After adding features** — Add a new feature doc in `features/` and update `features/_index.md`
4. **After architectural decisions** — Add a new decision record in `decisions/`
5. **Log significant changes** — Add a changelog entry in `changelog/`

The knowledge base is only useful if it reflects the current state of the code. Stale documentation is worse than no documentation because it actively misleads.

---

## Quick Reference

### File Naming
- All lowercase `kebab-case` with `.md` extension
- Index files: always `_index.md`
- Changelog: `YYYY-MM-DD_HHMM_short-description.md`

### File Size Targets
| File Type | Target Lines |
|-----------|-------------|
| `_index.md` (small dirs) | 6-15 |
| `_index.md` (large dirs) | 20-45 |
| Domain/entity leaf files | 25-50 |
| Service/logic leaf files | 30-60 |
| Repository/data-access leaf files | 25-45 |
| Frontend component docs | 25-55 |
| Frontend service/type docs | 15-35 |
| **Feature docs (cross-cutting)** | **60-120** |
| Decision records | 25-55 |
| Changelog entries | 15-60 |

### Every Leaf File Must Have
- A `#` title (one sentence follows it)
- Substantive content (not just links)
- A `## Related` section at the end with 2-8 cross-reference links

### Every `_index.md` Must Have
- A `#` title with one-sentence description
- Links to all child files and subdirectories
- No substantive documentation (navigation only)

### Execution Order Summary

| Step | What | Files Created |
|------|------|--------------|
| 1 | Analyze the project | None (research only) |
| 2 | Plan directory structure | Directories only |
| 3 | Root index | 1 file |
| 4 | Overview section | 3-7 files |
| 5 | Architecture section | 20-200+ files (scales with codebase) |
| 6 | Frontend docs (if applicable) | 10-100+ files |
| 7 | Feature docs (cross-cutting) | 8-20 files |
| 8 | Remaining sections | 5-30 files |
| 9 | Validate cross-references | 0 files (review only) |
| 10 | Wire into AGENTS.md | 1 file edit |
