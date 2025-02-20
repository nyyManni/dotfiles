if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Environment variable definitions
if [ (uname) != "Darwin" ]
    # Only set SSH_AUTH_SOCK on Linux, macOS handles it via the keychain
    set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"
end

starship init fish | source

# status is-login; and pyenv init --path | source
# status is-interactive; and pyenv init - | source

alias yabai-start "yabai --stop-service"
alias yabai-stop "yabai --start-service"
alias wappuradio "mplayer http://stream.wappuradio.fi/wappuradio1.opus"

function fish_greeting
end
