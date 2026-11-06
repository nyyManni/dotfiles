# Project layout

Project checkouts live outside a common Git repository:

```text
~/Projects/personal/<project>/
~/Projects/work/<customer>/<project>/
```

`~/.config/user-dirs.dirs` selects `$HOME/Projects` as `XDG_PROJECTS_DIR`.
Check it with `xdg-user-dir PROJECTS`. Each checkout retains its own Git history,
local changes, ignored files, and data. Customer-wide settings and notes stay
alongside that customer's checkouts.

## Task workflow rollout

The [task workflow rollout](task-workflow-rollout.md) defines a phased,
root-local task system for personal and customer work. Phase 1 implements
personal Org task capture, selection, and focus under
`~/Projects/personal/tasks/`; see the [usage guide](../emacs/README.md#personal-tasks-phase-1).
Later phases remain planned. Existing agent data is not moved by phase 1.

## Migration on 2026-10-16

The complete `~/personal/git/` and `~/projects/` trees were renamed, not cloned or
copied selectively. The immediate post-move comparison preserved all 124
repositories' HEADs, staged/unstaged diffs, and status inventories. Existing Git
problems were not repaired as part of the move.

Private recovery material is in
`~/.local/share/project-migration-20261016/`:

- `inventory.json`: original repository states and affected symlinks.
- `config-before/`: selected configuration copies from before path changes.
- `generated-before/` and `generated-archive.json`: archived generated outputs,
  preserving their home-relative paths. No tracked source was removed.

Configuration symlinks, Codex trust paths, Emacs known projects, and active
customer path settings were updated. Emacs customer policies remain private
under `~/.local/share/emacs/etc/customers/`. Credentials and historical
transcripts were not rewritten or moved into Git.

Editable global Python tools now resolve source under `~/Projects`. Two older
tool installs needed path-only metadata repairs rather than rebuilding from
their current, incompatible manifests. Their dependency sets were preserved.
The viewer's three editable packages were rebuilt without reinstalling their
dependencies; its GUI package metadata now reflects the current checkout.

## Transitional state: do not remove the old-path links yet

```text
~/personal/git -> ~/Projects/personal
~/projects     -> ~/Projects/work
```

These compatibility links remain deliberately: running agents/shells still hold
old path strings, and existing project virtual environments contain absolute
paths in activation/entrypoint scripts. The environments were retained, not
rewritten or rebuilt underneath active development sessions. New checkouts and
new sessions should use `~/Projects`.

Before removing these links:

1. Finish or restart agents and shells using old locations. Restart Emacs and
   project language servers when convenient; visited buffers and known projects
   were updated in the running editor, but existing session internals can retain
   old paths.
2. Recreate the virtual environments of maintained projects from their reviewed
   lockfiles, preserving required extras/groups and any manual dependencies.
   Keep old environments until replacement validation succeeds. Do not relax
   stale lockfiles or broken workspace manifests simply to make a sync succeed.
3. Check any further editable dependencies, scripts, and launchers against the
   new paths. Old environments and historical records can still name old paths.
4. Remove only the two compatibility symlinks, then repeat the checks below.

Inactive development containers and the retired project identified during the
audit were intentionally left alone. Their old path references are not a reason
to retain compatibility links indefinitely. Customer Git identity configuration
remains the separate task in `TODO.md`.

## Verification checklist

- [ ] All checkouts, notes, certificates, datasets, and local databases exist.
- [ ] Repository status matches the snapshot apart from intentional migration
  edits and archived untracked generated outputs; no staged changes were lost.
- [ ] No newly broken configuration or launcher symlinks.
- [ ] New Fish/Bash terminals resolve Git, Rust, npm, uv, and Emacs correctly.
- [ ] Fresh Emacs startup and all configuration tests pass.
- [ ] New Agent Shell sessions work in personal and active customer projects;
  optional MCP selection, local directory restrictions, and keyring access work.
- [ ] Codex recognizes new paths and expected trust settings.
- [ ] Reviewed direnv configurations load; project Python entrypoints and
  required extras work without relying on the old-path links.
- [ ] Active applications build/test with regenerated outputs where needed.
- [ ] Desktop, Noctalia, shortcuts, and relevant user services work after a fresh
  login. Compare service failures by timestamp; do not clear unrelated failures.
- [ ] Personal/customer Git identities are appropriate once the TODO is done.
- [ ] No ordinary use recreates the old directories after link removal.

Keep the recovery material until the final, link-free validation is complete.
