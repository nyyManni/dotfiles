-- Laptop-only profile: nothing docked, just the internal panel.
--
-- Matching is now EXACT (see pick_profile in hyprland.lua): this profile is
-- selected only when the internal panel is the *only* connected monitor.
-- During a dock the transient {laptop, primary} state matches no profile and
-- is therefore left untouched until the second external arrives and the full
-- profile matches.

return {
    requires = { "BOE 0x0AE0" },
    rules = {
        { workspace = "10", monitor = "BOE 0x0AE0", default = true },
    },
}
