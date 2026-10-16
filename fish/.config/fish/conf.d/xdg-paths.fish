# Keep these paths in sync with shell/.config/shell/tool-paths.sh.
set -l config_home "$HOME/.config"
if set -q XDG_CONFIG_HOME; and test -n "$XDG_CONFIG_HOME"
    set config_home "$XDG_CONFIG_HOME"
end
set -l data_home "$HOME/.local/share"
if set -q XDG_DATA_HOME; and test -n "$XDG_DATA_HOME"
    set data_home "$XDG_DATA_HOME"
end
set -l state_home "$HOME/.local/state"
if set -q XDG_STATE_HOME; and test -n "$XDG_STATE_HOME"
    set state_home "$XDG_STATE_HOME"
end
set -l cache_home "$HOME/.cache"
if set -q XDG_CACHE_HOME; and test -n "$XDG_CACHE_HOME"
    set cache_home "$XDG_CACHE_HOME"
end

set -gx AZURE_CONFIG_DIR "$data_home/azure"
set -gx CDK_HOME "$data_home/aws-cdk"
set -gx PUB_CACHE "$data_home/dart/pub-cache"
set -gx MCP_REMOTE_CONFIG_DIR "$data_home/mcp-remote"
set -gx CLAUDE_CONFIG_DIR "$data_home/claude-code"
set -gx CODEX_HOME "$data_home/codex"
set -gx COPILOT_HOME "$data_home/github-copilot"
set -gx DOCKER_CONFIG "$data_home/docker-cli"
# OmniSharp looks for .omnisharp/omnisharp.json under this parent.
set -gx OMNISHARPHOME "$config_home/omnisharp"

set -gx HISTFILE "$state_home/bash/history"
set -gx PSQL_HISTORY "$state_home/psql/history"
set -gx SQLITE_HISTORY "$state_home/sqlite/history"
set -gx PYTHON_HISTORY "$state_home/python/history"
command mkdir -p -m 700 -- "$state_home/bash" "$state_home/psql" \
    "$state_home/sqlite" "$state_home/python"

set -gx RUFF_CACHE_DIR "$cache_home/ruff"
set -gx CUDA_CACHE_PATH "$cache_home/cuda/ComputeCache"
