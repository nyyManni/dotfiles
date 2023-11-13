
function spotify -d "Run spotify in a GNU screen. Attach if already running."
    if ps -ef | grep -q "/[u]sr/local/bin/ncspot"
        screen -r spotify
    else
        screen -S spotify /usr/local/bin/ncspot
    end
end
