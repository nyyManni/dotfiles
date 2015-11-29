#!/usr/bin/env perl

# Get the position and width of the active monitor
`i3-msg -t get_workspaces` =~
  /focused":true.*?x":(?<x_pos>\d+).*?width":(?<width>\d+)/;

# Launch dmenu in the found monitor
exec("i3-dmenu-desktop --dmenu='dmenu -b -i -dim 1 -t -x "
     . $+{x_pos} . " -w " . $+{width} . "'");
