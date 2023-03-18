
# Usage: mkvenv <version> <name>
function mkvenv -d "Create virtual environment NAME with Python version VERSION" -a pyversion name
    if [ ! -d  $PYENV_ROOT/versions/$pyversion ]
        echo "==> Python $pyversion not installed, installing via pyenv..."
        pyenv install $pyversion
    end

    echo "==> Creating $WORKON_HOME/$name..."
    $PYENV_ROOT/versions/$pyversion/bin/python -m venv $WORKON_HOME/$name
    $WORKON_HOME/$name/bin/python -m pip install -U pip

    echo "==> Installing LSP libraries..."
    $WORKON_HOME/$name/bin/python -m pip install 'python-lsp-server[all]' pylint debugpy python-lsp-black pytest pylsp-mypy
    $WORKON_HOME/$name/bin/python -m pip uninstall rope

    echo "==> Finished"
end
