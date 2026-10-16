# Shell tool paths

Stow the `shell` package (along with `fish` and `uwsm`), or symlink
`.config/shell/tool-paths.sh` into `~/.config/shell/`.

This POSIX-shell fragment sets Rust's data directories and npm's cache
directory and puts Cargo's executables on PATH. UWSM sources it for desktop
applications. Standalone POSIX shells should source it from their startup
files, for example from `~/.bashrc` and `~/.profile`:

```sh
if [ -r "${XDG_CONFIG_HOME:-$HOME/.config}/shell/tool-paths.sh" ]; then
    . "${XDG_CONFIG_HOME:-$HOME/.config}/shell/tool-paths.sh"
fi
```

Fish has equivalent native setup in `conf.d/rustup.fish`, `conf.d/npm.fish`,
and `conf.d/xdg-paths.fish`.
Both setups respect `XDG_DATA_HOME`, `XDG_STATE_HOME`, and `XDG_CACHE_HOME`, with the usual
`~/.local/share` and `~/.cache` defaults. The settings must not depend solely
on UWSM: otherwise shells outside the desktop can recreate `~/.cargo` and
`~/.rustup`. Rustup's generated `cargo/env` scripts only update PATH.

After changing these settings, start a new shell. Log out and back in to
update the desktop session and its applications. Existing npm/npx processes
may still use the old `~/.npm` directory; do not move it while they are running.

## Histories and caches

Both shell setups put Bash, psql, SQLite, and Python (3.13+) histories in
`$XDG_STATE_HOME/{bash,psql,sqlite,python}/history`, defaulting to
`~/.local/state/`. Parent directories are created with private permissions.
Older Python interpreters do not honor `PYTHON_HISTORY`.

Ruff's cache goes in `$XDG_CACHE_HOME/ruff`, and the CUDA JIT cache goes in
`$XDG_CACHE_HOME/cuda/ComputeCache`, defaulting to `~/.cache/`.
Existing processes keep their old environment and may recreate old paths
until restarted. Fish's own history is unaffected by `HISTFILE`.

Stow the `git` and `vim` packages for the XDG Git configuration and Vim's
minimal path configuration. Vim's previously uninstalled personal settings
are preserved in `vim/.config/vim/legacy.vim` but are not enabled automatically.
Viminfo and netrw history live under `$XDG_STATE_HOME/vim/`.

## Azure, CDK, Dart, and MCP authentication

The shared POSIX setup and Fish's `xdg-paths.fish` both set:

| Variable | Location below `$XDG_DATA_HOME` (default `~/.local/share`) |
| --- | --- |
| `AZURE_CONFIG_DIR` | `azure/` |
| `CDK_HOME` | `aws-cdk/` |
| `PUB_CACHE` | `dart/pub-cache/` |
| `MCP_REMOTE_CONFIG_DIR` | `mcp-remote/` |

These stores are not dotfiles: they can contain login tokens, machine-local
state, downloaded code, and tool-managed configuration. Do not add their
contents to version control. Azure's extension directory follows
`AZURE_CONFIG_DIR` unless `AZURE_EXTENSION_DIR` is explicitly set elsewhere.

Dart projects can have absolute references to the old package cache in their
generated `.dart_tool/package_config.json`. Run `dart pub get` (or
`flutter pub get`) in affected projects to regenerate those references.
There are currently no globally activated executables in the old cache; if
you activate some later, add `$PUB_CACHE/bin` to PATH.

MCP migration is intentionally staged while the old `mcp-remote` bridge is
running: `~/.local/share/mcp-remote` temporarily points to `~/.mcp-auth`, so
new processes also find existing credentials. Once all old bridges stop,
remove that transitional symlink and move the original directory to its new
location. Log out/in before restarting desktop-launched agents so they inherit
the new variables. No credentials need to be copied into configuration files.

## AI clients, Docker CLI, and OmniSharp

| Variable | Location |
| --- | --- |
| `CLAUDE_CONFIG_DIR` | `$XDG_DATA_HOME/claude-code` |
| `CODEX_HOME` | `$XDG_DATA_HOME/codex` |
| `COPILOT_HOME` | `$XDG_DATA_HOME/github-copilot` |
| `DOCKER_CONFIG` | `$XDG_DATA_HOME/docker-cli` |
| `OMNISHARPHOME` | `$XDG_CONFIG_HOME/omnisharp` |

Claude, Codex, Copilot, and Docker mix machine-local state and potentially
credentials with settings; their stores remain outside version control.
Claude Code 2.1.246 also reads its global `.claude.json` inside an explicitly
set `CLAUDE_CONFIG_DIR`, so the previous home-level file is moved there too.

Codex migration is staged while sessions have its databases open:
`~/.local/share/codex` temporarily points to `~/.codex`. Do not move the
databases while those sessions run. Finalization also needs to retarget the
`~/.local/bin/codex` launcher and any absolute internal package symlinks that
still point at `~/.codex`; a directory rename alone is not sufficient.

OmniSharp appends `.omnisharp` to `OMNISHARPHOME`. Its settings therefore live
at `~/.config/omnisharp/.omnisharp/omnisharp.json`, supplied by the `omnisharp`
Stow package. The original settings and undo history are archived outside Git.

GnuPG/SSH stores, browser native-messaging integrations, NSS databases, and
partially relocatable .NET stores are deliberately not migrated wholesale.
Native application support is preferred over changing HOME or introducing
permanent compatibility symlinks throughout the home directory.
