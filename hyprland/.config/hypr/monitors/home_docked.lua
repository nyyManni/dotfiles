-- Workspace -> monitor bindings for the "home_docked" profile.
-- Home setup with the laptop lid closed — only the two external Dell monitors.

local helpers   = dofile(os.getenv("HOME") .. "/.config/hypr/monitors/helpers.lua")

local primary   = "Dell Inc. DELL U2719D 9JWNY13"
local secondary = "Dell Inc. DELL U2719D 1JWNY13"

return helpers.dual_with_optional_laptop(primary, secondary)
