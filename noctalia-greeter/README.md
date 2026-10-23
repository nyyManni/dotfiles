# Noctalia Greeter

System configuration for Noctalia Greeter with stable output identifiers,
verified with the 1.7.0 build. This directory is not a Stow package.

`greeter.toml` is installed as `/var/lib/noctalia-greeter/greeter.toml`.
It preserves the UWSM Hyprland session and Ergodox keyboard settings from the
previous system configuration. The laptop keyboard alternative remains in
comments; keyboard settings are global, not per-device.

## Monitor layouts

Monitor identities and geometry come from the description-based rules in
`hyprland/.config/hypr/hyprland.lua`. Profile membership and workspace placement
are defined under `hyprland/.config/hypr/monitors/`.

One map covers all known layouts without selecting a named profile:

| Setup | External placement | Scale / rotation |
| --- | --- | --- |
| Home | Main Dell at `0,0`; second Dell at `2560,-750` | 1; second monitor 270 degrees |
| Customer A | Main Lenovo at `0,0`; second Lenovo at `2560,-750` | 1; second monitor 270 degrees |
| Customer B | Philips at `0,0`; HP at `2560,-750` | 1; HP 270 degrees |
| Softlandia | Dells at `-5120,0` and `-2560,0` | 1; both landscape |
| Laptop only | Automatic, starting at `0,0` | 1.33333333; landscape |
| LG / IWB / Samsung TVs | Automatic placement | 2; landscape |

- External monitors use full descriptions including serials where available,
  not unstable `DP-*` connector names. The `desc:` prefix is Hyprland-specific
  and is not included in greeter identifiers.
- Absent monitors are ignored. The laptop and TVs are omitted from the fixed
  layout map and are automatically placed to the right of connected mapped
  monitors, matching the intent of Hyprland's `position = "auto"`.
- No global mode size or scale is forced. Modes are selected from the displays'
  available modes; explicit refresh requests mirror the Hyprland rules and fall
  back when unsupported.
- Unknown monitors remain usable with automatic placement/scaling and normal
  orientation. A monitor without a unique serial/description cannot be uniquely
  identified by this configuration alone.
- `output.name` is deliberately unset: the login UI is shown on all available
  outputs. The greeter's native lid handling suppresses the internal panel while
  docked when an external output is active; no custom lid scripts are installed.

`output.use_synced_settings = false` prevents stale connector-based layout,
rotation, and scale snapshots in `sync.toml` from affecting display configuration.
Appearance synchronization remains available. Existing connector-keyed wallpaper
assignments in `sync.toml` are unchanged; they are separate from stable geometry.

## Installation and updates

From the dotfiles repository root:

```sh
sudo install --backup=numbered -o greetd -g greetd -m 0644 \
  noctalia-greeter/greeter.toml /var/lib/noctalia-greeter/greeter.toml
```

The numbered backup preserves the preceding system configuration. Edit this
source when changing monitor geometry or the default session/keyboard, then
reinstall it. Appearance sync does not overwrite `greeter.toml`.

Installing this file does not switch display managers, enable/restart greetd,
or change the running desktop's monitor configuration. GDM activation remains
a separate operation. The greeter compositor reads these settings at startup.

### Fedora SELinux labels

The greeter runs in `xdm_t`. Its state directory must use `xdm_var_lib_t`,
not generic `var_lib_t`, so it can atomically replace `sync.toml` when saving
preferences. Correct Unix ownership alone is insufficient.

Register the persistent file-context rule once, then apply it:

```sh
sudo semanage fcontext -a -t xdm_var_lib_t '/var/lib/noctalia-greeter(/.*)?'
sudo restorecon -Rv /var/lib/noctalia-greeter
```

If that exact local rule already exists and needs correction, use `-m` instead
of `-a`. The rule survives reboots and future relabels. Keep SELinux enforcing;
do not loosen Unix permissions or grant additional policy permissions.

Verify the deployed labels with:

```sh
matchpathcon -V /var/lib/noctalia-greeter /var/lib/noctalia-greeter/sync.toml
```

The repair was smoke-tested by creating, writing, atomically replacing, reading,
and deleting temporary state files as `greetd` in the actual `xdm_t` domain.
New files inherited `xdm_var_lib_t`. The real `sync.toml` was not modified, and
no service restart or logout was required.

## Verification

The installed compositor and greeter were exercised with temporary wlroots
headless outputs carrying each monitor's stable description and dimensions.
The fixture varied connector numbers and swapped/reused connector names while
checking actual output positions, rotations, scales, and rendered greeter views.
All eight named Hyprland profile topologies, all three TV identities, and unknown
monitor fallback were covered. Stale connector-keyed sync data was present during
the checks to verify it could not override the new display policy.

No real DRM outputs, authenticated logins, or physical lid-switch transitions
were exercised. Headless mode also cannot verify real EDID identity strings,
hardware modes/refresh rates, or docking-driver behavior for disconnected
monitors; those still require login-screen checks at each location.
