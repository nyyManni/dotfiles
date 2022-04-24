
function __fish_mkvenv_complete_versions
    pyenv install --list |grep '^  [2-3]' |string trim
end

complete -f -c mkvenv -a "(__fish_mkvenv_complete_versions)"
