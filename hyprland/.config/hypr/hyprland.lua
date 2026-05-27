-- Hyprland Lua config, translated from hyprland.conf
-- See https://wiki.hypr.land/Configuring/Start/
--
-- NOTE: Hyprland 0.55 only converted the *compositor* config to Lua.
-- hyprpaper / hyprlock / hypridle / xdg-desktop-portal-hyprland keep using
-- hyprlang for now (https://hypr.land/news/26_lua/), so those .conf files
-- are intentionally left untouched.

------------------
---- MONITORS ----
------------------
-- See https://wiki.hypr.land/Configuring/Basics/Monitors/

hl.monitor({ output = "eDP-1", mode = "preferred", position = "auto", scale = 1.33333333 })

-- Monitors at home
hl.monitor({ output = "desc:Dell Inc. DELL U2719D 9JWNY13", mode = "2560x1440@75",   position = "0x0",     scale = 1 })
hl.monitor({ output = "desc:Dell Inc. DELL U2719D 1JWNY13", mode = "2560x1440@75",   position = "2560x-750", scale = 1, transform = 3 })

-- Monitors at Customer A
hl.monitor({ output = "desc:Lenovo Group Limited L27qe UTP02HZK", mode = "2560x1440@99.95", position = "0x0",     scale = 1 })
hl.monitor({ output = "desc:Lenovo Group Limited L27qe UTP02HZR", mode = "2560x1440@99.95", position = "2560x-750", scale = 1, transform = 3 })
-- TV at Customer A
hl.monitor({ output = "desc:LG Electronics LG TV SSCR2 0x01010101", mode = "preferred", position = "auto", scale = 2.0 })

-- Monitors at Customer B
hl.monitor({ output = "desc:Philips Consumer Electronics Company PHL 275E2F UHB2050003236", mode = "2560x1440@75", position = "0x0",     scale = 1 })
hl.monitor({ output = "desc:HP Inc. HP E27q G4 CNK1191FZX",                                  mode = "2560x1440",    position = "2560x-750", scale = 1, transform = 3 })

-- TVs at Customer B
hl.monitor({ output = "desc:IWB PC Monitor",                              mode = "preferred", position = "auto", scale = 2.0 })
hl.monitor({ output = "desc:Samsung Electric Company SAMSUNG 0x01000600", mode = "preferred", position = "auto", scale = 2.0 })

-- Monitors at Softlandia
hl.monitor({ output = "desc:Dell Inc. DELL P2723D JZ74PW3", mode = "2560x1440", position = "-5120x0", scale = 1 })
hl.monitor({ output = "desc:Dell Inc. DELL P2723D CF74PW3", mode = "2560x1440", position = "-2560x0", scale = 1 })

-- bindl=,switch:off:Lid Switch,exec,hyprctl keyword monitor "eDP-1, 3840x1260, 4000x0, 1"
-- bindl=,switch:on:Lid Switch,exec,hyprctl keyword monitor "eDP-1, "


---------------------
---- MY PROGRAMS ----
---------------------

local terminal    = "footclient"
local fileManager = "thunar"
-- local menu     = "uwsm -- app walker"
local menu        = "qs -c noctalia-shell ipc call launcher toggle"


---------------
---- INPUT ----
---------------
-- See https://wiki.hypr.land/Configuring/Basics/Variables/

hl.config({
    input = {
        follow_mouse  = 1,

        repeat_delay  = 175,
        repeat_rate   = 60,

        accel_profile = "flat",

        touchpad = {
            natural_scroll       = true,
            scroll_factor        = 0.2,
            clickfinger_behavior = 1,
            tap_to_click         = false,
        },
        -- tablet = {
        --     output = "desc:Dell Inc. DELL U2719D 9JWNY13",
        -- },

        sensitivity = 0.2, -- -1.0 - 1.0, 0 means no modification.
    },
})

-- Per-device configs. See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Devices/
hl.device({
    name       = "zsa-technology-labs-ergodox-ez-1",
    kb_layout  = "us,fi",
    kb_options = "grp:ctrls_toggle",
})

hl.device({
    name       = "zsa-technology-labs-ergodox-ez",
    kb_layout  = "us,fi",
    kb_options = "grp:ctrls_toggle",
})

hl.device({
    name                  = "at-translated-set-2-keyboard",
    kb_layout             = "uf,fi",
    kb_options            = "grp:alt_space_toggle,ctrl:swap_lalt_lctl,ctrl:swap_ralt_rctl,caps:escape",
    resolve_binds_by_sym  = 1,
})

hl.device({
    -- disable thinkpad nipple, the touchpad is well good enough
    name    = "tpps/2-elan-trackpoint",
    enabled = false,
})


-----------------------
---- LOOK AND FEEL ----
-----------------------

hl.config({
    general = {
        gaps_in     = 2,
        gaps_out    = 5,
        border_size = 2,
        col = {
            active_border   = { colors = { "rgba(33ccffee)", "rgba(00ff99ee)" }, angle = 45 },
            inactive_border = "rgba(595959aa)",
        },

        layout        = "dwindle",

        -- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Tearing/ before turning this on
        allow_tearing = false,
    },

    decoration = {
        rounding       = 10,
        rounding_power = 2,

        blur = {
            enabled  = true,
            size     = 3,
            passes   = 2,
            vibrancy = 0.1696,
        },

        shadow = {
            enabled      = true,
            range        = 4,
            render_power = 3,
            color        = 0xee1a1a1a, -- rgba(1a1a1aee)
        },
    },

    animations = {
        enabled = true,
    },

    dwindle = {
        -- pseudotile was removed in 0.55 (it didn't do anything anymore).
        preserve_split = true,
    },

    master = {
        -- new_is_master = true,
    },

    misc = {
        force_default_wallpaper = 0, -- 0 / 1 to disable the anime mascot wallpapers
        middle_click_paste      = false,
    },

    binds = {
        movefocus_cycles_fullscreen = false,
    },

    xwayland = {
        force_zero_scaling = true,
    },
})


-------------------
---- ANIMATIONS ----
-------------------

hl.curve("myBezier", { type = "bezier", points = { { 0.05, 0.9 }, { 0.1, 1.05 } } })

hl.animation({ leaf = "windows",     enabled = true, speed = 1, bezier = "myBezier" })
hl.animation({ leaf = "windowsOut",  enabled = true, speed = 1, bezier = "default", style = "popin 80%" })
hl.animation({ leaf = "border",      enabled = true, speed = 3, bezier = "default" })
hl.animation({ leaf = "borderangle", enabled = true, speed = 3, bezier = "default" })
hl.animation({ leaf = "fade",        enabled = true, speed = 2, bezier = "default" })
hl.animation({ leaf = "workspaces",  enabled = true, speed = 1, bezier = "default" })


-----------------
---- GESTURE ----
-----------------

hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })


---------------------
---- KEYBINDINGS ----
---------------------

local mainMod = "SUPER"

hl.bind(mainMod .. " + return",       hl.dsp.exec_cmd("uwsm app -- " .. terminal))
hl.bind(mainMod .. " + SHIFT + C",    hl.dsp.window.close())
hl.bind(mainMod .. " + SHIFT + M",    hl.dsp.exec_cmd("uwsm stop"))
hl.bind(mainMod .. " + E",            hl.dsp.exec_cmd(fileManager))

-- Super + ESC for noctalia shell's menu
-- hl.bind(mainMod .. " + ESC", hl.dsp.exec_cmd("qs -c noctalia-shell ipc call menu toggle"))

-- hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + space",        hl.dsp.exec_cmd(menu))
hl.bind(mainMod .. " + P",            hl.dsp.exec_cmd("hyprshot -m window --clipboard-only"))
hl.bind(mainMod .. " + SHIFT + P",    hl.dsp.exec_cmd("hyprshot -m region --clipboard-only"))
hl.bind(mainMod .. " + S",            hl.dsp.layout("togglesplit"))
hl.bind(mainMod .. " + F",            hl.dsp.window.fullscreen({ mode = "maximized" }))   -- old: fullscreen, 1
hl.bind(mainMod .. " + SHIFT + F",    hl.dsp.window.fullscreen({ mode = "fullscreen" }))  -- old: fullscreen
-- hl.bind("ALT + space", hl.dsp.exec_cmd("hyprctl switchxkblayout zsa-technology-labs-ergodox-ez-keyboard next"))
hl.bind(mainMod .. " + SHIFT + delete", hl.dsp.exec_cmd("loginctl lock-session"))

hl.bind(mainMod .. " + XF86Launch6",  hl.dsp.exec_cmd("loginctl lock-session"))

-- =========================================================================
-- Suspend/resume recovery experiments (Hyprland 0.55 s2idle wedge,
-- see https://github.com/hyprwm/Hyprland/discussions/14750).
--
-- All binds use hl.dispatch() so they run in Hyprland's own event loop
-- and don't depend on the hyprctl IPC socket (which is what's wedged in
-- the failure mode). `locked = true` makes them fire under hyprlock too.
--
-- Status: candidate recovery actions, none confirmed to actually unwedge
-- the panel. Next wedge, try them in this order and note which (if any)
-- brings the screen back. DO NOT add a lid-switch auto-trigger until one
-- of these is proven to be safe + effective — earlier off->on cycle bind
-- blanked working screens without recovering them.
-- =========================================================================

-- 1) Plain DPMS enable. Cheapest. Safe to press on a working screen
--    (no-op). If the panel is just DPMS-off, this is enough.
hl.bind("CTRL + " .. mainMod .. " + SHIFT + W", function()
    hl.dispatch(hl.dsp.dpms({ action = "enable" }))
end, { locked = true })

-- 2) DPMS toggle. If "enable" is being misinterpreted as a no-op when
--    Hyprland thinks DPMS is already on (even though the panel is dark),
--    a toggle may push it through a state change.
hl.bind("CTRL + " .. mainMod .. " + SHIFT + E", function()
    hl.dispatch(hl.dsp.dpms({}))  -- defaults to toggle
end, { locked = true })

-- 3) Re-assert monitor rules. This is closest to what Hyprland's missing
--    post-suspend recovery would do (applyMonitorRule with force=true).
--    Re-running hl.monitor() in a callback re-applies the rule.
hl.bind("CTRL + " .. mainMod .. " + SHIFT + R", function()
    hl.monitor({ output = "eDP-1", mode = "preferred", position = "auto", scale = 1.33333333 })
end, { locked = true })

-- 4) Full config reload via exec_cmd as a last-resort kick. This DOES go
--    through hyprctl, so it will fail if IPC is wedged — but if IPC is
--    alive and just the panel is disabled, a reload re-runs all
--    hl.monitor() calls and forces rule re-application.
hl.bind("CTRL + " .. mainMod .. " + SHIFT + T", hl.dsp.exec_cmd("hyprctl reload"), { locked = true })

hl.bind(mainMod .. " + period",       hl.dsp.exec_cmd("/usr/bin/1password --quick-access"))

hl.bind(mainMod .. " + N",            hl.dsp.exec_cmd("qs -c noctalia-shell ipc call notifications toggleHistory"))
hl.bind(mainMod .. " + SHIFT + N",    hl.dsp.exec_cmd("qs -c noctalia-shell ipc call notifications clear"))

-- Manual rearrange trigger (replaces the old external rearrange-workspaces
-- script). The actual implementation is registered further down, after the
-- profile loader; this binding just calls the global trampoline.
hl.bind("CTRL + " .. mainMod .. " + SHIFT + r", function()
    if _G.rearrange_workspaces then _G.rearrange_workspaces() end
end)

-- Move focus with mainMod + HJKL
hl.bind(mainMod .. " + H", hl.dsp.focus({ direction = "l" }))
hl.bind(mainMod .. " + L", hl.dsp.focus({ direction = "r" }))
hl.bind(mainMod .. " + K", hl.dsp.focus({ direction = "u" }))
hl.bind(mainMod .. " + J", hl.dsp.focus({ direction = "d" }))

hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.move({ direction = "l" }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.move({ direction = "r" }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.move({ direction = "u" }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.move({ direction = "d" }))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
    local key = i % 10 -- 10 -> 0
    hl.bind(mainMod .. " + " .. key,         hl.dsp.focus({ workspace = i }))
    hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

-- Move current workspace to a specific monitor
hl.bind("CTRL + " .. mainMod .. " + SHIFT + 1", hl.dsp.workspace.move({ monitor = 1 }))
hl.bind("CTRL + " .. mainMod .. " + SHIFT + 2", hl.dsp.workspace.move({ monitor = 2 }))
hl.bind("CTRL + " .. mainMod .. " + SHIFT + 3", hl.dsp.workspace.move({ monitor = 3 }))

-- Example special workspace (scratchpad)
-- hl.bind(mainMod .. " + S",         hl.dsp.workspace.toggle_special("magic"))
-- hl.bind(mainMod .. " + SHIFT + S", hl.dsp.window.move({ workspace = "special:magic" }))

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Monitor brightness controls (bindel == { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("qs -c noctalia-shell ipc call brightness increase"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("qs -c noctalia-shell ipc call brightness decrease"), { locked = true, repeating = true })

-- Volume controls (binde == { repeating = true })
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume -l 1.4 @DEFAULT_AUDIO_SINK@ 5%+"), { repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume -l 1.4 @DEFAULT_AUDIO_SINK@ 5%-"), { repeating = true })
hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))

-- Duplicate volume binds from end of original config (bindel/bindl variants)
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"), { locked = true, repeating = true })
hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), { locked = true })

-- Requires playerctl (bindl == { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"),   { locked = true })
hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"),       { locked = true })


------------------------
---- WORKSPACE RULES ----
------------------------
-- The hyprlang config used to `source = ~/.config/hypr/monitors/active.conf`
-- (a symlink updated by the external rearrange-workspaces bash script that
-- watched socket2 monitoradded/removed events). With Lua configs we can do
-- the whole pipeline natively: pick the matching profile from
-- `monitors/profiles.lua`, register the workspace rules, and move existing
-- workspaces to their assigned monitors. The same routine runs again on
-- monitor hotplug events (debounced, so docking ‑ which fires several events
-- in quick succession ‑ only triggers one rearrangement).

local home = os.getenv("HOME")
local monitors_dir = home .. "/.config/hypr/monitors"

local GRACE_MS = 3000

local function load_profile(name)
    -- Use dofile (not require) because Hyprland's lua sandbox typically
    -- doesn't have ~/.config/hypr on package.path, and dofile needs no setup.
    local ok, profile = pcall(dofile, monitors_dir .. "/" .. name .. ".lua")
    if not ok then
        hl.notification.create({
            text = "Failed to load monitor profile '" .. name .. "': " .. tostring(profile),
            timeout = 5000,
            icon = "warning",
        })
        return nil
    end
    return profile
end

local function connected_descriptions()
    -- Returns a set { [description] = monitor } for fast membership checks.
    local set = {}
    for _, m in ipairs(hl.get_monitors() or {}) do
        if m.description and m.description ~= "" then
            set[m.description] = m
        end
    end
    return set
end

local function pick_profile()
    local profile_names = dofile(monitors_dir .. "/profiles.lua")
    local connected = connected_descriptions()

    for _, name in ipairs(profile_names) do
        local profile = load_profile(name)
        if profile then
            local all_present = true
            for _, required in ipairs(profile.requires or {}) do
                if not connected[required] then
                    all_present = false
                    break
                end
            end
            if all_present then
                return name, profile, connected
            end
        end
    end
    return nil, nil, connected
end

local function apply_workspace_rules(profile)
    -- Re-register every workspace rule from the chosen profile. Whether
    -- successive hl.workspace_rule() calls cleanly *replace* the previous
    -- mapping at runtime is not explicitly documented, but new rules are at
    -- worst additive — the move loop below is the authoritative step.
    for _, rule in ipairs(profile.rules or {}) do
        local r = { workspace = rule.workspace, monitor = "desc:" .. rule.monitor }
        if rule.default then r.default = true end
        hl.workspace_rule(r)
    end
end

local function move_existing_workspaces(profile, connected)
    -- Build workspace -> target monitor name map.
    local target_for_ws = {}
    for _, rule in ipairs(profile.rules or {}) do
        local mon = connected[rule.monitor]
        if mon then
            target_for_ws[tostring(rule.workspace)] = mon.name
        end
    end

    for _, ws in ipairs(hl.get_workspaces() or {}) do
        local key = tostring(ws.id) -- workspace rules above use numeric ids as strings
        local target = target_for_ws[key] or target_for_ws[ws.name]
        if target and ws.monitor ~= target then
            hl.dispatch(hl.dsp.workspace.move({ workspace = ws.id, monitor = target }))
        end
    end
end

local function rearrange_workspaces()
    local name, profile, connected = pick_profile()
    if not profile then
        hl.notification.create({
            text = "rearrange: no monitor profile matched",
            timeout = 4000,
            icon = "warning",
        })
        return
    end

    apply_workspace_rules(profile)
    move_existing_workspaces(profile, connected)

    hl.notification.create({
        text = "Monitor profile applied: " .. name,
        timeout = 2500,
        icon = "ok",
    })
end

-- Expose to the keybind defined earlier.
_G.rearrange_workspaces = rearrange_workspaces

-- Debounced trigger. We arm a single-shot timer on each event and disable +
-- re-enable it to push the deadline forward, mirroring the old script's
-- 3 s grace period that lets docking settle before reacting.
local rearrange_timer
rearrange_timer = hl.timer(function()
    rearrange_workspaces()
end, { timeout = GRACE_MS, type = "oneshot" })
rearrange_timer:set_enabled(false)

local function schedule_rearrange()
    rearrange_timer:set_enabled(false)
    rearrange_timer:set_enabled(true)
end

hl.on("monitor.added",          schedule_rearrange)
hl.on("monitor.removed",        schedule_rearrange)
hl.on("monitor.layout_changed", schedule_rearrange)

-- Initial application after Hyprland is fully up.
hl.on("hyprland.start", rearrange_workspaces)


-------------------
---- AUTOSTART ----
-------------------

hl.on("hyprland.start", function()
    -- hl.exec_cmd("waybar")
    hl.exec_cmd("gsettings set org.gnome.desktop.interface cursor-theme 'catppuccin-mocha-blue-cursors'")
    hl.exec_cmd("/usr/bin/qs -c noctalia-shell")
end)


----------------------
---- WINDOW RULES ----
----------------------

-- hide xwayland video bridge's main window
--
-- NOTE: In the Lua rule API (Hyprland >= 0.55) `size` is a static effect that
-- only applies to *floating* windows -- there is no `max_size`/`maxsize`
-- equivalent of the old hyprlang rule. Without `float = true` the window
-- stays tiled (taking a full slot) and `size = {1, 1}` is silently ignored.
-- Combined with `no_focus = true`, the result is an unfocusable window that
-- steals half the screen. Floating it (and parking it on workspace 10
-- silently) restores the old behaviour.
hl.window_rule({
    name             = "xwayland-videobridge",
    match            = { class = "^(xwaylandvideobridge)$" },

    float            = true,
    workspace        = "10 silent",
    opacity          = "0.0 override 0.0 override",
    no_anim          = true,
    no_initial_focus = true,
    size             = { 1, 1 },
    no_blur          = true,
    no_focus         = true,
})

hl.window_rule({
    name              = "optocal",
    match             = { title = "optocal" },

    float             = true,
    monitor           = "DP-2",
    no_initial_focus  = true,
    fullscreen_state  = "3 3",
    move              = { 0, 0 },
    suppress_event    = "activate activatefocus",
})


---------------------
---- LAYER RULES ----
---------------------

hl.layer_rule({
    name         = "noctalia",
    match        = { namespace = "noctalia-background-.*$" },

    ignore_alpha = 0.5,
    blur         = true,
    blur_popups  = true,
})
