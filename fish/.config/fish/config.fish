if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Environment variable definitions
set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"

starship init fish | source

status is-login; and pyenv init --path | source
status is-interactive; and pyenv init - | source

