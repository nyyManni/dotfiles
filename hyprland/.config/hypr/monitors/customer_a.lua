-- Workspace -> monitor bindings for the "customer_a" profile.
-- All three monitors connected: two external Lenovos + laptop panel.

local helpers   = dofile(os.getenv("HOME") .. "/.config/hypr/monitors/helpers.lua")

local primary   = "Lenovo Group Limited L27qe UTP02HZK"
local secondary = "Lenovo Group Limited L27qe UTP02HZR"
local laptop    = "BOE 0x0AE0"

return helpers.dual_with_optional_laptop(primary, secondary, laptop)
