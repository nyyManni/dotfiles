# NVIDIA Settings

Stow this package, or link its config directory, executable, and desktop entry
into the corresponding locations under `~/.config` and `~/.local`.

The `~/.local/bin/nvidia-settings` wrapper supplies
`--config "$XDG_CONFIG_HOME/nvidia/settings-rc"`, falling back to
`~/.config/nvidia/settings-rc`. The config directory is linked to dotfiles, so
preferences saved by NVIDIA Settings stay version-controlled.

The desktop entry overrides the packaged entry and explicitly invokes the
wrapper, even if the desktop session's PATH does not include `~/.local/bin`.
Shells need `~/.local/bin` before `/usr/bin` in PATH, as configured here already.

An explicit `/usr/bin/nvidia-settings` invocation bypasses the wrapper; supply
`--config` yourself when using that path. Existing shell command caches may
also need refreshing (`hash -r` in Bash, or start a new shell).

The `.config/autostart/nvidia-settings-user.desktop` entry also overrides the
packaged autostart entry. It runs the wrapper with `--load-config-only`, so login
uses the same XDG profile instead of the missing `~/.nvidia-settings-rc`.
Link that file individually into the existing `~/.config/autostart/` directory;
do not replace the whole mixed directory.
