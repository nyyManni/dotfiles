function __fish_workon_complete_venvs
    for venv in (ls -1 $WORKON_HOME)
        echo "$venv	"(ls -l $WORKON_HOME/$venv/bin/python | sed "s|.*/[vV]ersions/\(.*\)/bin/python|\1|g" |string trim)
    end
end

complete -f -c workon -a "(__fish_workon_complete_venvs)"
