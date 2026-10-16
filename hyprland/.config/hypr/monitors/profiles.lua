-- Ordered list of monitor profiles. The rearrange logic in hyprland.lua
-- iterates these in order and picks the first profile whose `requires`
-- monitor descriptions EXACTLY match the currently connected set (no more,
-- no less).
--
-- There is intentionally no catch-all fallback: if the connected monitors do
-- not exactly match any profile (e.g. a transient half-docked state, or an
-- unknown display), the rearrange logic does nothing and leaves the current
-- workspace placement untouched. Add an explicit profile for any monitor
-- combination you want handled.

return {
    "home",
    "home_docked",
    "customer_a",
    "customer_a_docked",
    "customer_b",
    "customer_b_docked",
    "softlandia",
    "laptop_only",
}
