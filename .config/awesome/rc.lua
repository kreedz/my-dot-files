-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
require("vicious")
require('calendar2')

-- Load Debian menu entries
require("debian.menu")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.add_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/themes/zenburn/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

--require_safe('personal')

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,
    --awful.layout.suit.tile,
    --awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    --awful.layout.suit.tile.top,
    --awful.layout.suit.fair,
    --awful.layout.suit.fair.horizontal,
    --awful.layout.suit.spiral,
    --awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen
    --awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
    names = { "term", "www", "vim", "mc", "mu", "un" },
    layout={ layouts[3], layouts[3], layouts[3], layouts[3], layouts[3], layouts[1] }
}

for s = 1, screen.count() do
    tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "Debian", debian.menu.Debian_menu.Debian },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })

-- Create a systray
mysystray = widget({ type = "systray" })
separator = widget({ type = "textbox"})
spacer = widget({ type = "textbox"})
separator.text = '|'
spacer.text = ' '

calendar2.addCalendarToWidget(mytextclock)

mpdtextbox = widget({ type = "textbox" })
function mpdcron (mpdinf)
    local mpdtext = ""
    if (mpdinf.state=="play") then
        mpdtext = "▶ " .. mpdinf.artist .. " - " .. mpdinf.title
    elseif (mpdinf.state=="pause") then
        mpdtext = "▮▮ " .. mpdinf.artist .. " - " .. mpdinf.title
    else
        mpdtext = "[ leállítva ]"
    end
    mpdtextbox.text = awful.util.escape(mpdtext)
end

mybattmon = widget({ type = "textbox", name = "mybattmon", align = "right" })
function battery_status ()
    local output={} --output buffer
    local fd=io.popen("acpitool -b", "r") --list present batteries
    local line=fd:read()
    while line do --there might be several batteries.
        local battery_num = string.match(line, "Battery \#(%d+)")
        local battery_load = string.match(line, " (%d*\.%d+)%%")
        if not battery_load then
            return "⚡"
        end
        local time_rem = string.match(line, "(%d+\:%d+)\:%d+")
        local discharging
        if string.match(line, "discharging")=="discharging" then --discharging: always red
            discharging="<span color=\"#CC7777\">"
        elseif tonumber(battery_load)>85 then --almost charged
            discharging="<span color=\"#77CC77\">"
        else --charging
            discharging="<span color=\"#CCCC77\">"
        end
        if battery_load then
            table.insert(output,discharging..battery_load.."%</span>")
        elseif battery_num and battery_load and time_rem then
            table.insert(output,discharging.."BAT#"..battery_num.." "..battery_load.."% "..time_rem.."</span>")
        elseif battery_num and battery_load then --remaining time unavailable
            --table.insert(output,discharging.."BAT#"..battery_num.." "..battery_load.."%</span>")
            table.insert(output,discharging.."BAT#"..battery_num.." "..battery_load.."%</span>")
        end --even more data unavailable: we might be getting an unexpected output format, so let's just skip this line.
        line=fd:read() --read next line
    end
    return table.concat(output," ") --FIXME: better separation for several batteries. maybe a pipe?
end
mybattmon.text = battery_status()
my_battmon_timer=timer({timeout=120})
my_battmon_timer:add_signal("timeout", function()
    --mytextbox.text = " " .. os.date() .. " "
    mybattmon.text = battery_status()
end)
my_battmon_timer:start()

-- VOLUME --
volumecfg = {}
volumecfg.cardid  = 0
volumecfg.channel = "Master"
volumecfg.widget = widget({ type = "textbox", name = "volumecfg.widget", align = "right" })
volumecfg_t = awful.tooltip({ objects = { volumecfg.widget },})
volumecfg_t:set_text("Volume")
-- command must start with a space!
volumecfg.mixercommand = function (command)
       local fd = io.popen("amixer -c " .. volumecfg.cardid .. command)
       local status = fd:read("*all")
       fd:close()

       local volume = string.match(status, "(%d?%d?%d)%%")
       volume = string.format("%d", volume)
       status = string.match(status, "%[(o[^%]]*)%]")
       if string.find(status, "on", 1, true) then
               volume = volume .. "%"
       else
               volume = volume .. "M"
       end
       volumecfg.widget.text = volume
end
volumecfg.update = function () volumecfg.mixercommand(" sget " .. volumecfg.channel)              end
volumecfg.up     = function () volumecfg.mixercommand(" sset " .. volumecfg.channel .. " 5%+")    end
volumecfg.down   = function () volumecfg.mixercommand(" sset " .. volumecfg.channel .. " 5%-")    end
volumecfg.toggle = function () volumecfg.mixercommand(" sset " .. volumecfg.channel .. " toggle") end
volumecfg.widget:buttons({
       button({ }, 4, function () volumecfg.up() end),
       button({ }, 5, function () volumecfg.down() end),
       button({ }, 1, function () volumecfg.toggle() end)
})
volumecfg.update()

-- RAM usage widget
memwidget = awful.widget.progressbar()
memwidget:set_width(15)
memwidget:set_height(30)
memwidget:set_vertical(true)
memwidget:set_background_color('#494B4F')
memwidget:set_color('#AECF96')
memwidget:set_gradient_colors({ '#AECF96', '#88A175', '#FF5656' })
-- RAM usage tooltip
memwidget_t = awful.tooltip({ objects = { memwidget.widget },})
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem,
                function (widget, args)
                    memwidget_t:set_text(" RAM: " .. args[2] .. "MB / " .. args[3] .. "MB ")
                    return args[1]
                 end, 20)

-- CPU usage widget
cpuwidget = awful.widget.graph()
cpuwidget:set_width(50)
cpuwidget:set_height(30)
cpuwidget:set_background_color("#494B4F")
cpuwidget:set_color("#FF5656")
cpuwidget:set_gradient_colors({ "#FF5656", "#88A175", "#AECF96" })
cpuwidget_t = awful.tooltip({ objects = { cpuwidget.widget },})
-- Register CPU widget
vicious.register(cpuwidget, vicious.widgets.cpu,
                    function (widget, args)
                        cpuwidget_t:set_text("CPU Usage: " .. args[1] .. "%")
                        return args[1]
                    end, 10)

-- Weather widget
weatherwidget = widget({ type = "textbox" })
weather_t = awful.tooltip({ objects = { weatherwidget },})

vicious.register(weatherwidget, vicious.widgets.weather,
                function (widget, args)
                    weather_t:set_text("City: " .. args["{city}"] .."\nWind: " .. args["{windkmh}"] .. "km/h " .. args["{wind}"] .. "\nSky: " .. args["{sky}"] .. "\nHumidity: " .. args["{humid}"] .. "%")
                    return args["{tempc}"] .. "C"
                end, 1800, "UWUU")

-- Keyboard widget --
kbdcfg = {}
kbdcfg.widget = widget({ type = "textbox", align = "right" })
kbdcfg.layout = { ["00000002"] = "us", ["00001006"] = "ru" }
kbdcfg.detect = function () kbdcfg.widget.text = kbdcfg.layout[io.popen("xset -q|grep LED| awk '{ print $10 }'"):read()] end
kbdcfg.detect()

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
        mytextclock,
        s == 1 and mysystray or nil, spacer,
        separator, spacer, memwidget.widget, spacer,
        separator, spacer, cpuwidget.widget, spacer,
        separator, spacer, kbdcfg.widget, spacer,
        separator, spacer, volumecfg.widget, spacer,
        separator, spacer, mybattmon, spacer,
        separator, spacer, weatherwidget, spacer,
        separator, spacer, mpdtextbox, spacer, separator,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),

    awful.key({ }, "XF86AudioRaiseVolume", function () volumecfg.up() end),
    awful.key({ }, "XF86AudioLowerVolume", function () volumecfg.down() end),
    awful.key({ "Shift" }, "Alt_L", function () kbdcfg.detect() end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)

   --awful.key({ modkey }, "F10", function (c) scratch.pad.set(c, 0.60, 0.60, true) end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons, --} },
                     size_hints_honor = false } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- Autostarting programm
os.execute("pgrep -u $USER -x nm-applet || (nm-applet &)")
os.execute("pgrep -u $USER -x xflux || (xflux -l 54.44N -g 55.58E &)")
os.execute("xset b off")
