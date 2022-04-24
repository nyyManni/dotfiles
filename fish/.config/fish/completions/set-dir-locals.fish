function __fish_workon_complete_venvs
    for venv in (ls -1 $WORKON_HOME)
        echo "$venv	"(ls -l $WORKON_HOME/$venv/bin/python | sed "s|.*/versions/\(.*\)/bin/python|\1|g" |string trim)
    end
end


complete -f -c set-dir-locals -a "(__fish_workon_complete_venvs)"
