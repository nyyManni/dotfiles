-- Workspace -> monitor bindings for the "customer_a_docked" profile.
-- Customer A setup with the laptop lid closed — only the two external Lenovos.

local helpers   = dofile(os.getenv("HOME") .. "/.config/hypr/monitors/helpers.lua")

local primary   = "Lenovo Group Limited L27qe UTP02HZK"
local secondary = "Lenovo Group Limited L27qe UTP02HZR"

return helpers.dual_with_optional_laptop(primary, secondary)
