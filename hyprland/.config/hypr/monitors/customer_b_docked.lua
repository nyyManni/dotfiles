-- Workspace -> monitor bindings for the "customer_b_docked" profile.
-- Customer B setup with the laptop lid closed — only Philips + HP monitors.

local helpers   = dofile(os.getenv("HOME") .. "/.config/hypr/monitors/helpers.lua")

local primary   = "Philips Consumer Electronics Company PHL 275E2F UHB2050003236"
local secondary = "HP Inc. HP E27q G4 CNK1191FZX"

return helpers.dual_with_optional_laptop(primary, secondary)
