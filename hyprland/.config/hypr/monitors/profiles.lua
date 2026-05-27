-- Ordered list of monitor profiles. The rearrange logic in hyprland.lua
-- iterates these in order and picks the first profile whose `requires`
-- monitor descriptions are all currently connected. Keep "default" last —
-- it has no requirements and therefore always matches.

return {
    "home",
    "customer_a",
    "customer_b",
    "softlandia",
    "default",
}
