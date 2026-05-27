-- Workspace -> monitor bindings for the "softlandia" profile.
-- See monitors/home.lua for the schema description.

local primary   = "Dell Inc. DELL P2723D JZ74PW3"
local secondary = "Dell Inc. DELL P2723D CF74PW3"
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
