#!/usr/bin/env perl

use JSON qw/decode_json/;
use strict;
use warnings;

# Position and width of the dmenu to launch
my $pos;
my $width;


# Find the workspace that is focused and get its dimensions
foreach my $output (@{decode_json(`i3-msg -t get_workspaces`)}) {
  if ($output->{'focused'}) {
    $pos = $output->{'rect'}{'x'};
    $width = $output->{'rect'}{'width'};

    # Break out of the loop, no need to check other monitors
    last;
  }
}

# Launch dmenu in the correct monitor
exec("bash", "-c", "i3-dmenu-desktop --dmenu='dmenu -b -i -o 0.75 -dim 0.1 -t -x "
     . $pos . " -w " . $width . "'");
