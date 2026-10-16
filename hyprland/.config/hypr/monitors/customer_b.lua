-- Workspace -> monitor bindings for the "customer_b" profile.
-- All three monitors connected: Philips + HP + laptop panel.

local helpers   = dofile(os.getenv("HOME") .. "/.config/hypr/monitors/helpers.lua")

local primary   = "Philips Consumer Electronics Company PHL 275E2F UHB2050003236"
local secondary = "HP Inc. HP E27q G4 CNK1191FZX"
local laptop    = "BOE 0x0AE0"

return helpers.dual_with_optional_laptop(primary, secondary, laptop)
