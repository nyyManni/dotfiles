# Emacs layout

Symlink `.config/emacs` to `~/.config/emacs` (or use Stow). Keep these files
in Git:

- `early-init.el` and `init.el`: startup configuration.
- `user-lisp/*.el`: personal libraries, loaded with `require` after their
  package dependencies are available.
- `tests/*.el`: ERT tests; use `-L .config/emacs/user-lisp` when running them.

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

Agent Shell transcripts and screenshots for new sessions go into
`~/.local/share/emacs/var/agent-shell/<project-hash>/`, rather than
`.agent-shell/` in the project or home directory. Existing sessions retain
their cached transcript paths: leave their old directories intact until
those sessions finish. Historical transcripts are not moved automatically.

## Agent Shell prompt submission

In insert state, `C-return` accepts the selected Corfu completion before
submitting the prompt. For example, `/ren` with `rename` selected submits
`/rename`, not the partial prefix. With no active completion or no selected
candidate, it submits the text as typed. This uses Agent Shell's normal submit
command, preserving its busy-session queue and prefix-argument behavior.
`RET` still inserts a newline, and `TAB` accepts completion without submitting.
Other modes' Corfu bindings are unchanged.

## OMP session navigation

`SPC c c` (`consult-agent-shell-sessions`) switches between open conversations.
Its labels prefer OMP's native session title, then a descriptive buffer name,
then the agent's cached prompt. Long labels are collapsed to one line and
limited to 64 display columns. Status, mode, model, permission count, and
directory remain visible. Identical rows are distinguished by buffer name.

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
