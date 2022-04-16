
# Usage: mkvenv
function mkvenv -d "Create virtual environment NAME with Python version VERSION" -a name pyversion
    $PYENV_ROOT/versions/$pyversion/bin/python -m venv $WORKON_HOME/$name
    $WORKON_HOME/$name/bin/python -m pip install -U pip
    $WORKON_HOME/$name/bin/python -m pip install 'python-lsp-server[all]' pylint debugpy python-lsp-black pytest
end
