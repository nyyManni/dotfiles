function __fish_workon_complete_venvs
    for venv in (ls -1 $WORKON_HOME)
        echo "$venv	"(pyenv-version-name $venv |string trim)
    end
end

complete -f -c workon -a "(__fish_workon_complete_venvs)"
