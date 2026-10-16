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
