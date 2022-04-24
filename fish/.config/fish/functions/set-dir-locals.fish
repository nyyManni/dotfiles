

function set-dir-locals -a venv
    if [ -f .dir-locals.el ]
        echo ".dir-locals.el already exists. Not overwriting."
        return
    end

    if [ -z $venv ]
        printf "((python-mode . ((pyvenv-activate . \"%s\"))))" $VIRTUAL_ENV
    else
        printf "((python-mode . ((pyvenv-activate . \"%s\"))))" $venv
    end > .dir-locals.el
end
