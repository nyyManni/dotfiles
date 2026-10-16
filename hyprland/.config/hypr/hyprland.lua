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

-- Lid switch: disable internal monitor when closed, re-enable when opened.
-- This triggers monitor.removed/added events which in turn fire the profile
-- rearrange logic (e.g. switching between "home" and "home_docked").
--
-- IMPORTANT: the disable is gated on dock state. Disabling eDP-1 must ONLY
-- happen when external monitors are present (docked). In laptop-only mode,
-- closing the lid triggers a logind suspend (HandleLidSwitch=suspend); if we
-- disabled the sole panel here, the matching `switch:off` re-enable would be
-- lost across suspend (libinput does not re-emit switch transitions that
-- occur while asleep), leaving the panel stuck disabled on resume ("backlight
-- on, screen black"). Leaving eDP-1 enabled when laptop-only lets Hyprland's
-- native resume path bring the panel back. (Independent of the i915 s2idle
-- kernel bug worked around by booting 6.19.x.)
--
-- Known limitation (accepted): if you undock while the lid is already closed,
-- logind does NOT re-evaluate the lid (it only acts on lid *events*), so the
-- machine stays awake and headless until you open the lid -- at which point
-- `switch:off` re-enables eDP-1 and it recovers.

local function edp_is_active()
    for _, m in ipairs(hl.get_monitors() or {}) do
        if m.name == "eDP-1" then return true end
    end
    return false
end

local function external_monitor_count()
    local n = 0
    for _, m in ipairs(hl.get_monitors() or {}) do
        if m.name ~= "eDP-1" then n = n + 1 end
    end
    return n
end

hl.bind("switch:on:Lid Switch", function()
    -- Only disable the internal panel while docked. A laptop-only lid close
    -- falls through to a logind suspend with eDP-1 left enabled.
    if external_monitor_count() > 0 then
        hl.monitor({ output = "eDP-1", disabled = true })
    end
end, { locked = true })

hl.bind("switch:off:Lid Switch", function()
    -- Idempotent re-enable: only act if eDP-1 is currently disabled (i.e. we
    -- disabled it on a docked lid close). In laptop-only mode eDP-1 is already
    -- active here, so this is a no-op and adds no redundant modeset on wake.
    if not edp_is_active() then
        hl.monitor({ output = "eDP-1", disabled = false, mode = "preferred", position = "auto", scale = 1.33333333 })
    end
end, { locked = true })


---------------------
---- MY PROGRAMS ----
---------------------

local terminal    = "footclient"
local editor      = "emacsclient --create-frame"
local fileManager = "thunar"
-- local menu     = "uwsm -- app walker"
local menu        = "noctalia msg panel-toggle launcher"
-- local menu        = "qs -c noctalia-shell ipc call launcher toggle"


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
        preserve_split = false,
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
hl.bind(mainMod .. " + SHIFT + return",       hl.dsp.exec_cmd("uwsm app -- " .. editor))
hl.bind(mainMod .. " + SHIFT + C",    hl.dsp.window.close())
hl.bind(mainMod .. " + SHIFT + M",    hl.dsp.exec_cmd("uwsm stop"))
hl.bind(mainMod .. " + E",            hl.dsp.exec_cmd(fileManager))

-- Super + ESC for noctalia shell's menu
-- hl.bind(mainMod .. " + ESC", hl.dsp.exec_cmd("qs -c noctalia-shell ipc call menu toggle"))

-- hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + space",        hl.dsp.exec_cmd(menu))
hl.bind(mainMod .. " + SHIFT + U",    hl.dsp.exec_cmd("noctalia msg panel-toggle nyymanni/noctalia-lunch:menus"))
hl.bind(mainMod .. " + P",            hl.dsp.exec_cmd("hyprshot -m window --clipboard-only"))
hl.bind(mainMod .. " + SHIFT + P",    hl.dsp.exec_cmd("hyprshot -m region --clipboard-only"))
hl.bind(mainMod .. " + S",            hl.dsp.layout("togglesplit"))
hl.bind(mainMod .. " + F",            hl.dsp.window.fullscreen({ mode = "maximized" }))   -- old: fullscreen, 1
hl.bind(mainMod .. " + SHIFT + F",    hl.dsp.window.fullscreen({ mode = "fullscreen" }))  -- old: fullscreen
-- hl.bind("ALT + space", hl.dsp.exec_cmd("hyprctl switchxkblayout zsa-technology-labs-ergodox-ez-keyboard next"))
-- hl.bind(mainMod .. " + SHIFT + delete", hl.dsp.exec_cmd("loginctl lock-session"))
hl.bind(mainMod .. " + SHIFT + delete", hl.dsp.exec_cmd("noctalia msg session lock"))

-- hl.bind(mainMod .. " + XF86Launch6",  hl.dsp.exec_cmd("loginctl lock-session"))
hl.bind(mainMod .. " + XF86Launch6",  hl.dsp.exec_cmd("noctalia msg session lock"))

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

-- Push-to-talk: keydown starts the script, keyup signals it to exit.
-- local ptt_script = os.getenv("HOME") .. "/.config/hypr/scripts/push-to-talk.sh"
-- hl.bind("mouse:274", hl.dsp.exec_cmd(ptt_script))
-- hl.bind("mouse:274", hl.dsp.exec_cmd("kill -USR1 $(cat /tmp/push-to-talk.pid)"), { release = true })


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

    -- Count distinct connected descriptions so we can require an EXACT match
    -- below (same set, not just a subset).
    local connected_count = 0
    for _ in pairs(connected) do
        connected_count = connected_count + 1
    end

    for _, name in ipairs(profile_names) do
        local profile = load_profile(name)
        if profile then
            -- Exact-set match: every required description must be connected
            -- AND the connected set must contain nothing extra. This makes
            -- transient half-docked states (e.g. {laptop, primary} mid-dock)
            -- and unknown monitor combinations match no profile at all, so
            -- the caller leaves the current placement untouched instead of
            -- churning. The grace period is no longer needed because the
            -- partial state is simply a no-op; the final monitor event lands
            -- on the fully-connected set and matches.
            local required = profile.requires or {}
            local all_present = true
            for _, desc in ipairs(required) do
                if not connected[desc] then
                    all_present = false
                    break
                end
            end
            if all_present and #required == connected_count then
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

    -- Returns the number of workspaces actually moved so the caller can tell
    -- whether anything changed (and therefore whether to notify / re-fire).
    --
    -- NOTE: ws.monitor is a MONITOR OBJECT (CLuaMonitor), not a string, so we
    -- must compare its `.name` field against the target name. The previous
    -- `ws.monitor ~= target` compared an object to a string and was therefore
    -- *always* true, dispatching a move for every workspace on every pass.
    -- Combined with the monitor.layout_changed trigger that those very moves
    -- re-fire, that produced an endless rearrange/notify loop.
    local moved = 0
    for _, ws in ipairs(hl.get_workspaces() or {}) do
        local key = tostring(ws.id) -- workspace rules above use numeric ids as strings
        local target = target_for_ws[key] or target_for_ws[ws.name]
        local current = ws.monitor and ws.monitor.name or nil
        if target and current ~= target then
            hl.dispatch(hl.dsp.workspace.move({ workspace = ws.id, monitor = target }))
            moved = moved + 1
        end
    end
    return moved
end

local last_applied_profile = nil
local function rearrange_workspaces(manual)
    local name, profile, connected = pick_profile()
    if not profile then
        -- No profile exactly matches the connected monitors. For automatic
        -- monitor events this is expected (a transient half-docked state, or
        -- an unrecognized setup) and we silently leave placement untouched.
        -- Only the manual keybind surfaces a notification, so invoking it by
        -- hand still tells you nothing matched.
        if manual then
            hl.notification.create({
                text = "rearrange: no monitor profile matched",
                timeout = 4000,
                icon = "warning",
            })
        end
        last_applied_profile = nil
        return
    end

    apply_workspace_rules(profile)
    local moved = move_existing_workspaces(profile, connected)

    -- Only notify when something actually changed: either we switched to a
    -- different profile than last time, or at least one workspace was moved.
    if name ~= last_applied_profile or moved > 0 then
        hl.notification.create({
            text = "Monitor profile applied: " .. name,
            timeout = 2500,
            icon = "ok",
        })
    end
    last_applied_profile = name
end

-- Expose to the keybind defined earlier. The keybind invokes it manually, so
-- pass `manual = true` to enable the "no profile matched" feedback.
_G.rearrange_workspaces = function() rearrange_workspaces(true) end

-- No debounce / grace period is needed. Profile matching is exact (see
-- pick_profile), so any transient half-docked state simply matches no profile
-- and rearrange does nothing. Each monitor event re-evaluates immediately;
-- the event that completes the recognized set is the one that applies it.
-- This reacts faster than the old grace timer and uses no background ticks.
--
-- We deliberately do NOT subscribe to monitor.layout_changed: that event is
-- re-fired by our own workspace moves (moveworkspacetomonitor changes the
-- layout), which would create a self-perpetuating loop. Docking and hotplug
-- always emit monitor.added / monitor.removed, which is sufficient.
local function on_monitor_event()
    rearrange_workspaces(false)
end

hl.on("monitor.added",   on_monitor_event)
hl.on("monitor.removed", on_monitor_event)

-- Initial application after Hyprland is fully up. Not "manual": if no profile
-- matches at startup we stay silent rather than warning.
hl.on("hyprland.start", function() rearrange_workspaces(false) end)


-------------------
---- AUTOSTART ----
-------------------

hl.on("hyprland.start", function()
    hl.exec_cmd("gsettings set org.gnome.desktop.interface cursor-theme 'catppuccin-mocha-blue-cursors'")
end)

-- Noctalia is started by its UWSM-managed user service, which runs after
-- graphical-session.target has exported the compositor environment.


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

-- Keep host SUPER bindings available while the VM viewer has focus.
hl.window_rule({
    name                 = "virt-viewer-host-shortcuts",
    match                = { class = "^virt-viewer$" },
    no_shortcuts_inhibit = true,
})


---------------------
---- LAYER RULES ----
---------------------

-- hl.layer_rule({
--     name         = "noctalia",
--     match        = { namespace = "noctalia-background-.*$" },

--     ignore_alpha = 0.5,
--     blur         = true,
--     blur_popups  = true,
-- })

------------------------
---- NOCTALIA SHELL ----
------------------------
hl.config({
  general = {
    gaps_in = 5,
    gaps_out = 7,
  },

  decoration = {
    rounding = 10,
    rounding_power = 2,

    shadow = {
      enabled = true,
      range = 4,
      render_power = 3,
      color = 0xee1a1a1a,
    },

    blur = {
      enabled = true,
      size = 3,
      passes = 2,
      vibrancy = 0.1696,
    },
  },
})

hl.layer_rule({
  name = "noctalia",
  match = {
    namespace = "^noctalia-(bar-.+|notification|dock|panel|attached-panel|osd)$",
  },
  no_anim = true,
  ignore_alpha = 0.5,
  blur = true,
  blur_popups = true,
})

-- For Noctalia Color templates
require("noctalia").apply_theme()
