# User services

Link these unit files and drop-ins into `~/.config/systemd/user`, or use Stow.
Noctalia's unit is supplied separately by the `uwsm` package.

Run `systemctl --user daemon-reload` after installing or editing units.
Enablement links under `*.wants/` and `*.requires/` are machine-local and are
not tracked. Environment/credential files referenced by units remain private.
Installing the files does not imply enabling or restarting every service.
