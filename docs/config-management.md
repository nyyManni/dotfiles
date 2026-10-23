# Application configuration

Configuration is organized as Stow packages. Existing mixed config directories
use individual file symlinks so that generated state and credentials remain
outside the repository. Do not replace these mixed directories wholesale.

## Tracked selections

Project checkout locations and migration follow-up are documented in
[`project-layout.md`](project-layout.md).

- `git`, `nvidia`, `omnisharp`, `shell`, and `vim`: XDG/path configuration.
- `emacs` and `fish`: personal libraries, tests, and shell setup.
  Emacs customer server definitions stay outside Git under
  `~/.local/share/emacs/etc/customers/`; see `emacs/README.md`.
- `systemd`: custom Emacs, SSH agent, Copilot proxy, and Hyprland helper units.
- `uwsm`: desktop environment and the Noctalia service.
- `noctalia`: native preference fragments for bars, appearance/panels, widget
  layouts, and reviewed plugin settings in `~/.config/noctalia/`. Accounts,
  private widget fields, and machine-local paths stay in local state; see
  [`noctalia/README.md`](../noctalia/README.md).
- `noctalia-greeter`: manually installed system configuration, not a Stow package.
  Stable monitor identities cover all desktop layouts while appearance sync
  remains independent; see [`noctalia-greeter/README.md`](../noctalia-greeter/README.md).
- `keyboard-layout`: the `uf` symbols source and a data-only system RPM; see
  [`keyboard-layout/README.md`](../keyboard-layout/README.md). The RPM uses the
  additive XKB extension directory; installing it and retiring the existing
  user-level override are separate, explicit operations.
- `cava`: existing shader files; there is currently no custom main config.
- `htop`: display and process-list preferences.
- `sqlite3`: interactive shell preferences.
- `zathura`: existing Noctalia theme snapshot; no main config was added.
- `gtk`: GTK 3 preferences and GTK 3/4 CSS import files. Generated
  `noctalia.css`, bookmarks, and undo histories remain local.
- `eca`: personal instructions, rules, and two personal skills. MCP/provider
  configuration, externally installed GitHits skills, and separately cloned
  skill repositories remain local.
- `opencode`: only `opencode.json`, not service registration or authentication.

uv configuration remains local under `~/.config/uv/`, including customer index
profiles and the Python version preference. These are regular files, not links
into dotfiles. The former `uv` package is no longer tracked, and `/uv/` is ignored
to prevent accidental reintroduction of customer-specific package-feed metadata.

No OAuth stores, browser profiles, environment files containing keys, service
enablement links, or nested third-party Git repositories should be added.

## Noctalia v5 service

The installed unit links to the repository copy and loads
`EnvironmentFile=%h/.local/share/noctalia/openai.env`. The environment file
was moved out of `~/.config`, remains private with mode `0600`, and is not
stored in Git. Its contents are unchanged.

The existing `app-graphical.slice` is already `PartOf=graphical-session.target`
and conflicts with UWSM's shutdown target, so lifecycle integration is already
present; no duplicate lifecycle settings are needed in the service.

Suggested follow-up: change the line to
`EnvironmentFile=-%h/.local/share/noctalia/openai.env` if Noctalia should start even
without the optional AI key. Keep it mandatory if missing credentials should
prevent startup. This behavioral change has not been applied.

Noctalia was not restarted during the migration. The old v4 `_noctalia` backup
and Eww configuration/units were removed from active configuration. Their
contents were archived outside the repository, along with the approved
uninstalled-application leftovers.

The obsolete `QT_QPA_PLATFORMTHEME=qt6ct` override was removed. Existing
applications retain their original environment until restarted or a new login.

### Noctalia preference fragments

The `noctalia` Stow package supplies a compact bar for each known secondary
display using its description and serial. Main displays, the laptop, TVs, and
unfamiliar monitors keep the full default bar. Secondary bars retain workspaces
and the inherited center widgets, but omit the end/status lane.

The original Lenovo GUI bar override was moved into `bars.toml`, alongside
the other secondary displays and the default bar preferences. `appearance.toml`
contains theme/panel/shell preferences, `widgets.toml` contains widget layouts
and aliases, and `plugins.toml` contains enabled IDs and reviewed settings.
Credentials are referenced only by environment-variable names, never values.

Accounts, location, private lock-screen images/labels, wallpaper paths, plugin
source arrays/development paths, and machine-specific commands remain in
`~/.local/state/noctalia/settings.toml`. Noctalia loads config-directory TOML
first and state settings last. GUI edits remain local until deliberately
promoted into the appropriate tracked fragment and removed from local overrides.

The split preserved the complete native effective configuration exactly.
Validation, hot reload, and live UI checks passed without restarting the shell.


## Separate service warning

Unit verification found an existing ordering cycle in the distro-provided
`foot-server.socket`: its explicit `After=graphical-session.target` conflicts
with the default socket ordering before `sockets.target`, which precedes
`basic.target` and the graphical session. This is not caused by the imported
units. The packaged unit and running Foot server were left unchanged; review
the socket's default dependencies separately before changing its boot ordering.
