#!/bin/python
""" Workaround for d_menu_run, which is not able to run on
a single display when using multiple X Screens """

from subprocess import check_output, call
import json

# Read the workspace setup from i3-msg
JSON_INPUT = check_output(["i3-msg", "-t", "get_workspaces"])
MODEL = json.loads(JSON_INPUT.decode("utf-8"))

# Find the workspace that is focused
for workspace in MODEL:
    if workspace["focused"]:
        # Set the offset and length of the dmenu by the position
        # and width of the monitor the workspace is on
        pos = workspace["rect"]["x"]
        width = workspace["rect"]["width"]

        # Focused Workspace found, ignore the rest
        break

# Call dmenu_run with parameters above
call(["dmenu_run", "-b", "-o", "0.75", "-dim", "0.1",
      "-t", "-x", str(pos), "-w", str(width)])

