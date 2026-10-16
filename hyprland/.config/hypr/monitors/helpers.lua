-- monitors/helpers.lua — shared utilities for monitor profile generation.

local M = {}

--- Generate a standard dual-monitor workspace layout.
---
--- Pattern:
---   Workspaces 1–5 → primary   (1 is default)
---   Workspaces 6–9 → secondary (6 is default)
---   Workspace  10  → laptop if present, otherwise secondary
---
--- @param primary   string  Monitor description for the primary display.
--- @param secondary string  Monitor description for the secondary display.
--- @param laptop    string? Monitor description for the internal panel (nil if lid closed).
--- @return table profile    Table with `requires` and `rules` ready for the profile loader.
function M.dual_with_optional_laptop(primary, secondary, laptop)
    local requires = { primary, secondary }
    if laptop then
        requires[#requires + 1] = laptop
    end

    local rules = {
        { workspace = "1",  monitor = primary,   default = true },
        { workspace = "2",  monitor = primary },
        { workspace = "3",  monitor = primary },
        { workspace = "4",  monitor = primary },
        { workspace = "5",  monitor = primary },

        { workspace = "6",  monitor = secondary, default = true },
        { workspace = "7",  monitor = secondary },
        { workspace = "8",  monitor = secondary },
        { workspace = "9",  monitor = secondary },

        { workspace = "10", monitor = laptop or secondary, default = true },
    }

    return {
        requires = requires,
        rules = rules,
    }
end

return M
