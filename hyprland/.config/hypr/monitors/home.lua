-- Workspace -> monitor bindings for the "home" profile.
-- All three monitors connected: two external Dells + laptop panel.

local helpers   = dofile(os.getenv("HOME") .. "/.config/hypr/monitors/helpers.lua")

local primary   = "Dell Inc. DELL U2719D 9JWNY13"
local secondary = "Dell Inc. DELL U2719D 1JWNY13"
local laptop    = "BOE 0x0AE0"

return helpers.dual_with_optional_laptop(primary, secondary, laptop)
