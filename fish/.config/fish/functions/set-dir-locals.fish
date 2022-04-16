

function set-dir-locals -a venv
    printf "((python-mode . ((pyvenv-activate . \"%s\"))))" $VIRTUAL_ENV
end
