# Noctalia Shell preferences

Native, version-controlled Noctalia v5 preferences. Install this package from
the dotfiles repository root:

```sh
stow --target="$HOME" noctalia
```

The files are individually linked into `~/.config/noctalia/`. Do not replace
that whole directory if it also contains local files.

| Fragment | Version-controlled preferences |
| --- | --- |
| `bars.toml` | Default bar widget lists, styling, spacing, and stable secondary-monitor overrides |
| `appearance.toml` | Theme/template selections, panels/corners/animation, notification opacity, lock transition, calendar display/refresh settings, battery warning, and existing shell behavior toggles |
| `widgets.toml` | Desktop and lock-screen widget layouts/styles/order, plus bar widget aliases and preferences |
| `plugins.toml` | Enabled plugin IDs and reviewed non-secret settings, including names of credential environment variables |

The migration preserves existing values; it does not change privacy/telemetry
choices, locking behavior, plugin activity, monitor identities, or widget
positions. Widget layouts deliberately retain their existing display selectors
and coordinates; they are not automatically adapted to different hardware.

## Configuration layers and local-only data

Noctalia loads the config-directory TOML files first, then applies
`~/.local/state/noctalia/settings.toml` as a higher-priority override layer.
Each tracked preference belongs in one fragment. The local state file retains:

- Configuration schema metadata and hardware-specific brightness/DDC settings.
- Calendar account definitions and location.
- Lock-screen/notification monitor selection lists.
- Private lock-screen image paths and personal-name labels; their surrounding
  widget layout/style remains tracked.
- Plugin source arrays, including local development directories. The public and
  local entries remain together because arrays replace rather than merge.
- The CodexBar executable path, shell avatar/custom launch command, and all
  current/default/per-monitor wallpaper file paths and last-used selection.

Credential values, OAuth stores, encrypted clipboard data, notification history,
usage/screen-time data, plugin downloads, caches, and generated application theme
files stay outside Git. `*_env` plugin settings contain variable names only;
credential values continue to come from the existing private environment file.
Enabled plugin IDs do not vendor development plugins: configure their local
sources separately when restoring another machine.

## GUI-first editing and promotion to Git

Continue editing through the GUI normally. The GUI writes local overrides; it
does not rewrite these tracked fragments or commit changes automatically.
When saving a configuration checkpoint:

1. Review the changes in local `settings.toml` and identify the intended
   non-private preference fields.
2. Copy those fields into the appropriate tracked fragment, preserving widget
   IDs, list order, and nested table structure.
3. Remove only the copied fields from local `settings.toml`, so they no longer
   shadow later changes to the repository file. Keep accounts, private widget
   fields, paths, and other local-only settings intact.
4. Validate and reload:

   ```sh
   noctalia config validate
   noctalia msg config-reload
   ```

5. Review the repository diff for credentials, private text, or local paths
   before adding and committing the fragment changes.

Promotion is needed only when committing a checkpoint, not after every GUI
adjustment. A GUI edit that remains local continues to override the tracked
preference until deliberately promoted or discarded.

`noctalia config export full` shows effective values and inherited monitor fields.
It also includes unrelated personal settings: inspect it privately, not as a
file to commit or publish wholesale. Private migration backups are under
`~/.local/share/noctalia/config-backups/`.

## Main and secondary monitor bars

| Role | Monitors | Start | Center | End |
| --- | --- | --- | --- | --- |
| Main/default | Primary monitors, laptop, TVs, unfamiliar displays | Default | Default | Default |
| Secondary | Home Dell `1JWNY13`, work Lenovo `UTP02HZR`, work HP `CNK1191FZX`, Softlandia Dell `CF74PW3` | Workspaces only | Inherit default | Empty |

Softlandia's secondary display uses the compact bar even though it is landscape.
Roles follow the existing Hyprland profiles rather than orientation alone.
Open/closed-lid profiles use the same external monitor identities.

```toml
[bar.default.monitor."Dell Inc. DELL U2719D 1JWNY13"]
start = [ "workspaces" ]
end = []
```

An omitted lane inherits the default bar; an empty array explicitly clears it.
Thus secondary bars keep the clock, world-clock (`bar`), and lunch (`lunch`)
center widgets. Styling/spacing are inherited too. Edit all four overrides when
changing the shared secondary layout. Selectors use descriptions/serials, not
unstable `DP-*` connector names, and do not include Hyprland's `desc:` prefix.

## Verification

The native full effective configuration was identical before and after splitting
preferences, including the private fields retained locally. A fresh state-directory
load also reproduced every tracked preference at native effective precision.
Validation and hot reload succeeded, visible Noctalia surface geometry stayed
unchanged, and the actual live bar was visually checked. The shell process was
not restarted; no lock or suspend was requested.

The earlier monitor-bar change was also rendered in an isolated compositor for
all four main/secondary monitor pairs. Physical docking at other locations was
not exercised. Temporary validation state, captures, and helper artifacts are
removed after verification; private migration backups remain outside Git.
