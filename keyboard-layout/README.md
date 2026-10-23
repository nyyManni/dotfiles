# Personal keyboard layouts

Data-only Fedora RPM for the `uf` XKB layout: US-style touch typing on a
Nordic keyboard, including the extra left Shift key and swapped Return/backslash
keys. The existing layout definition is unchanged.

## Sources and installation layout

- `.config/xkb/symbols/uf`: authoritative symbols file; also supports the existing
  user-level Stow setup until the RPM is installed.
- `evdev.xml`: registry fragment containing only `uf`, not a replacement for
  the distribution's full registry.
- `personal-keyboard-layouts.spec`: `noarch` RPM with no install/uninstall scripts
  or triggers.

The RPM installs two root-owned, world-readable files:

```text
/usr/share/xkeyboard-config.d/personal-keyboard-layouts/symbols/uf
/usr/share/xkeyboard-config.d/personal-keyboard-layouts/rules/evdev.xml
```

The extension root comes from libxkbcommon's `xkb_unversioned_extensions_path`
pkg-config variable. Runtime requirements are libxkbcommon 1.13+ and
xkeyboard-config 2.45+. These additive extension directories are supported by
libxkbcommon-based Wayland clients; they are not an Xorg or Linux console keymap
installation mechanism.

No distribution-owned XKB files are patched. Installing or removing the RPM
never selects a keyboard layout, changes compositor/greeter configuration, or
starts/restarts services. The package uses `LicenseRef-NotLicensed`: no new
redistribution license is granted to this personal configuration.

## Build

From the dotfiles repository root:

```sh
sudo dnf install rpm-build libxkbcommon-devel xkeyboard-config
./keyboard-layout/build-rpm.sh
```

The script can also be invoked from another working directory. Binary and
source RPMs are written under `keyboard-layout/build/RPMS/noarch/` and
`keyboard-layout/build/SRPMS/`; the entire build directory is Git-ignored.
No root privileges are used by the build itself.

For the current version on Fedora 44, the binary RPM is:

```text
keyboard-layout/build/RPMS/noarch/personal-keyboard-layouts-1.0-1.fc44.noarch.rpm
```

When changing the layout or registry, bump `Release` in the spec (or `Version`
and reset `Release` for a new version) and add a changelog entry before building.
Install the newly built RPM with DNF to upgrade the deployed files.

## Install and migrate the user override

Installation is a separate, explicit step; the build does not install anything:

```sh
sudo dnf install ./keyboard-layout/build/RPMS/noarch/personal-keyboard-layouts-1.0-1.fc44.noarch.rpm
```

Use the actual generated filename if the release or Fedora version differs.

After installation, retire the user-level `uf` override: user XKB data has
higher priority and would hide future RPM updates. If it was installed by Stow,
first preview removal from the repository root:

```sh
stow --simulate --verbose --delete --target="$HOME" keyboard-layout
```

Then run the same command without `--simulate` after reviewing the output.
Remove only the user-facing link(s), not the source in this repository. If the
layout was installed manually, inspect the path before removing the user copy.
Do not remove other custom files from a mixed `~/.config/xkb` directory.
Packaging files and build outputs are excluded from Stow by
`.stow-local-ignore`.

Keep the existing Hyprland per-device `kb_layout` and `kb_options` settings.
Once the override is removed, new keymaps resolve `uf` from the RPM instead.
Existing sessions keep their already compiled keymaps until reconfigured or
restarted.

Noctalia Greeter runs as a different account and can resolve the installed
layout without access to your home. Its `[keyboard]` selection/options still
need to be configured separately. This RPM does not switch GDM to greetd.

Before uninstalling the RPM, configure every consumer to use a layout that
will remain available, such as `us` or `fi`, or restore the user-level layout
for those consumers. Then remove it with `sudo dnf remove personal-keyboard-layouts`.

## Verification

The built RPM payload was extracted into a temporary directory, with a clean
HOME and its extension directory selected through the supported XKB search-path
override. libxkbcommon compiled `uf,fi` with the laptop's existing options;
Return/backslash/bar, extra Shift, Caps/Escape, and Alt/Ctrl behavior were
exercised. The complete compiled keymap matched the existing source, and
libxkbregistry enumerated `uf` alongside the stock US and Finnish layouts.

This validates package contents and lookup behavior without installing the RPM,
changing the active keyboard, or switching display managers. No authenticated
greeter login was performed.

References: [XKB extension packaging](https://xkbcommon.org/doc/current/packaging-keyboard-layouts.html)
and [custom configuration/search paths](https://xkbcommon.org/doc/current/custom-configuration.html#xkb-data-locations).
