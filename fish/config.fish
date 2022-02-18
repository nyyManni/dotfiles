if status is-interactive
    # Commands to run in interactive sessions can go here
end


starship init fish | source

status is-login; and pyenv init --path | source
status is-interactive; and pyenv init - | source
