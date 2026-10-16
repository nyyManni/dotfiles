# Set these even in shells that do not inherit the UWSM environment.
set -l data_home "$HOME/.local/share"
if set -q XDG_DATA_HOME; and test -n "$XDG_DATA_HOME"
    set data_home "$XDG_DATA_HOME"
end
set -gx CARGO_HOME "$data_home/cargo"
set -gx RUSTUP_HOME "$data_home/rustup"
fish_add_path --path --prepend "$CARGO_HOME/bin"
