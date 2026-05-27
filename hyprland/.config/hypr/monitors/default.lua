-- Default fallback profile. `requires = {}` means it always matches; it must
-- therefore be tried last (see monitors/profiles.lua).

return {
    requires = {},
    rules = {
        { workspace = "10", monitor = "BOE 0x0AE0", default = true },
    },
}
