# Shared POSIX-shell setup for standalone shells and the UWSM session.
# Keep the Fish equivalents in fish/conf.d/{rustup,npm,xdg-paths}.fish in sync.
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export RUSTUP_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/rustup"
export npm_config_cache="${XDG_CACHE_HOME:-$HOME/.cache}/npm"

# These tools mix configuration, mutable state, and sometimes credentials.
# Keep the whole store out of Git; only the location settings belong here.
export AZURE_CONFIG_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/azure"
export CDK_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/aws-cdk"
export PUB_CACHE="${XDG_DATA_HOME:-$HOME/.local/share}/dart/pub-cache"
export MCP_REMOTE_CONFIG_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/mcp-remote"
export CLAUDE_CONFIG_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/claude-code"
export CODEX_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/codex"
export COPILOT_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/github-copilot"
export DOCKER_CONFIG="${XDG_DATA_HOME:-$HOME/.local/share}/docker-cli"
# OmniSharp appends .omnisharp itself, so this is its parent directory.
export OMNISHARPHOME="${XDG_CONFIG_HOME:-$HOME/.config}/omnisharp"

# Histories are persistent private state, not configuration or disposable cache.
export HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/bash/history"
export PSQL_HISTORY="${XDG_STATE_HOME:-$HOME/.local/state}/psql/history"
export SQLITE_HISTORY="${XDG_STATE_HOME:-$HOME/.local/state}/sqlite/history"
export PYTHON_HISTORY="${XDG_STATE_HOME:-$HOME/.local/state}/python/history"
mkdir -p -m 700 -- "${HISTFILE%/*}" "${PSQL_HISTORY%/*}" \
    "${SQLITE_HISTORY%/*}" "${PYTHON_HISTORY%/*}"

export RUFF_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/ruff"
export CUDA_CACHE_PATH="${XDG_CACHE_HOME:-$HOME/.cache}/cuda/ComputeCache"

# Do not rely on rustup's generated env script: it only adjusts PATH and
# does not set CARGO_HOME or RUSTUP_HOME for shells outside the desktop.
case ":$PATH:" in
    *:"$CARGO_HOME/bin":*) ;;
    *) export PATH="$CARGO_HOME/bin:$PATH" ;;
esac
