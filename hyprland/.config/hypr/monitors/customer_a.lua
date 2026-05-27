-- Workspace -> monitor bindings for the "customer_a" profile.
-- See monitors/home.lua for the schema description.

local primary   = "Lenovo Group Limited L27qe UTP02HZK"
local secondary = "Lenovo Group Limited L27qe UTP02HZR"
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
