-- Workspace -> monitor bindings for the "home" profile.
-- Returns a profile table consumed by hyprland.lua's rearrange logic.
--
-- `requires`  — monitor descriptions (without the "desc:" prefix) that must
--               all be connected for this profile to be selected.
-- `rules`     — workspace -> monitor mapping. `default = true` mirrors the
--               old `default:true` workspace-rule flag.

local primary   = "Dell Inc. DELL U2719D 9JWNY13"
local secondary = "Dell Inc. DELL U2719D 1JWNY13"
local laptop    = "BOE 0x0AE0"

return {
    requires = { primary, secondary, laptop },
    rules = {
        { workspace = "1",  monitor = primary,   default = true },
        { workspace = "2",  monitor = primary },
        { workspace = "3",  monitor = primary },
        { workspace = "4",  monitor = primary },
        { workspace = "5",  monitor = primary },

        { workspace = "6",  monitor = secondary, default = true },
        { workspace = "7",  monitor = secondary },
        { workspace = "8",  monitor = secondary },
        { workspace = "9",  monitor = secondary },

        { workspace = "10", monitor = laptop,    default = true },
    },
}
