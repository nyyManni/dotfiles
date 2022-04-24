
function __fish_mkvenv_complete_versions
    for pyversion in (pyenv install --list |grep '^  [2-3]' |string trim)
        if [ -d  $PYENV_ROOT/versions/$pyversion ]
            echo "$pyversion	installed"
        else
            echo $pyversion
        end
    end
end

complete -f -c mkvenv -a "(__fish_mkvenv_complete_versions)"
