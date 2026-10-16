# Keep npm's cache, npx downloads, and logs out of the home directory.
set -l cache_home "$HOME/.cache"
if set -q XDG_CACHE_HOME; and test -n "$XDG_CACHE_HOME"
    set cache_home "$XDG_CACHE_HOME"
end
set -gx npm_config_cache "$cache_home/npm"
