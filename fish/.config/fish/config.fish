if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Environment variable definitions
if [ (uname) != "Darwin" ]
    # Only set SSH_AUTH_SOCK on Linux, macOS handles it via the keychain
    set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"
else
    alias yabai-start "yabai --stop-service"
    alias yabai-stop "yabai --start-service"
end

starship init fish | source
direnv hook fish | source

function fish_greeting
end
