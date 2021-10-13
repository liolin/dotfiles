# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from typing import List  # noqa: F401

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen
from libqtile.lazy import lazy

import os
import subprocess
import socket


@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser("~/.config/qtile/autostart.sh")
    subprocess.call([home])


mod = "mod4"
myTerm = "alacritty"
browser = "brave"

prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())
colors = [["#282c34", "#282c34"], # panel background
        ["#3d3f4b", "#434758"], # background for current screen tab
        ["#ffffff", "#ffffff"], # font color for group names
        ["#ff5555", "#ff5555"], # border line color for current tab
        ["#74438f", "#74438f"], # border line color for 'other tabs' and color for 'odd widgets'
        ["#4f76c7", "#4f76c7"], # color for the 'even widgets'
        ["#e1acff", "#e1acff"], # window name
        ["#ecbbfb", "#ecbbfb"]] # backbround for inactive screens


keys = [
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(),
        desc="Move window focus to other window"),
    Key([mod], "t", lazy.window.toggle_floating(), desc="Put the focused window to/from floating mode"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, "shift"], "c", lazy.window.kill(), desc="Kill focused window"),

    Key([mod, "shift"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "shift"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawn("rofi -show drun"),
        desc="Spawn a command using a prompt widget"),

    Key([mod, "shift"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod, "shift"], "p", lazy.spawn("cinnamon-screensaver-command --lock && systemctl suspend",
                                        desc="Lock screen using cinnamon screensaver")),

    ## Menus
    KeyChord([mod], "p", [
        Key([], "e",
            lazy.spawn("/usr/bin/env bash ~/.dmscripts/rofi_power")),
        Key([], "p",
            lazy.spawn("/usr/bin/passmenu -i"))
    ]),

    ## Multimedia Keys
    Key([], "Print", lazy.spawn("flameshot gui")),
    Key([], "XF86AudioMute", lazy.spawn("pamixer --toggle-mute")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pamixer --decrease 5")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("pamixer --increase 5")),
    Key([], "XF86AudioMicMute", lazy.spawn("amixer set Capture toggle")),
    Key([], "XF86AudioPlay", lazy.spawn("playerctl play-pause")),
    Key([], "XF86AudioPrev", lazy.spawn("playerctl previous")),
    Key([], "XF86AudioNext", lazy.spawn("playerctl next")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl 5%+")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl 5%+")),
    Key([], "XF86WLAN", lazy.spawn("nmcli radio wifi on")),

    ## Start Applications
    Key([mod], "Return", lazy.spawn(myTerm), desc="Launch terminal"),
    Key([], "F12", lazy.spawn("nemo")),

    ## Emacs
    KeyChord([mod], "d", [
        Key([], "e",
            lazy.spawn("emacsclient -c -a ''")),
        Key([], "b",
            lazy.spawn("emacsclient -c -a '' --eval '(ibuffer)'")),
        Key([], "d",
            lazy.spawn("emacsclient -c -a '' --eval '(dired nil)'")),
        Key([], "m",
            lazy.spawn("emacsclient -c -a '' --eval '(mu4e)'")),
        Key([], "s",
            lazy.spawn("emacsclient -c -a '' --eval '(eshell)'")),
        Key([], "a",
            lazy.spawn("emacsclient -c -a '' --eval '(org-agenda)'")),
    ])
]

groups = [Group("1: term"),
        Group("2: editor"),
        Group("3: web"),
        Group("4: xxx"),
        Group("5: music"),
        Group("6: chat"),
        Group("7: mail"),
        Group("8: xxx"),
        Group("9: office"),
        Group("0: game")]

for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name[0], lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        # # mod1 + shift + letter of group = move focused window to group
        Key([mod, "shift"], i.name[0], lazy.window.togroup(i.name),
            desc="move focused window to group {}".format(i.name)),
    ])

layout_theme = {"border_width": 2,
                "margin": 2,
                "border_focus": "e1acff",
                "border_normal": "1D2330"
                }
layouts = [
    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    # Try more layouts by unleashing below layouts.
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.Sep(
                    linewidth = 0,
                    padding = 6,
                    foreground = colors[2],
                    background = colors[0]
                    ),
                widget.Image(
                    filename = "~/.config/qtile/icons/python-white.png",
                    scale = "False",
                    mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm)}
                    ),
                widget.Sep(
                    linewidth = 0,
                    padding = 6,
                    foreground = colors[2],
                    background = colors[0]
                    ),
                widget.GroupBox(
                    font = "Ubuntu Bold",
                    fontsize = 9,
                    margin_y = 3,
                    margin_x = 0,
                    padding_y = 5,
                    padding_x = 3,
                    borderwidth = 3,
                    active = colors[2],
                    inactive = colors[7],
                    rounded = False,
                    highlight_color = colors[1],
                    highlight_method = "line",
                    this_current_screen_border = colors[6],
                    this_screen_border = colors [4],
                    other_current_screen_border = colors[6],
                    other_screen_border = colors[4],
                    foreground = colors[2],
                    background = colors[0]
                    ),
                widget.Prompt(
                    prompt = prompt,
                    font = "Ubuntu Mono",
                    padding = 10,
                    foreground = colors[3],
                    background = colors[1]
                    ),
                widget.Sep(
                    linewidth = 0,
                    padding = 40,
                    foreground = colors[2],
                    background = colors[0]
                    ),
                widget.WindowName(
                        foreground = colors[6],
                        background = colors[0],
                        padding = 0
                        ),
                widget.Systray(
                        background = colors[0],
                        padding = 5
                        ),
                widget.Sep(
                        linewidth = 0,
                    padding = 6,
                    foreground = colors[0],
                    background = colors[0]
                    ),
                widget.TextBox(
                    text = '',
                    background = colors[0],
                    foreground = colors[5],
                    padding = 0,
                    fontsize = 37
                    ),
                widget.Net(
                    interface = "wlan0",
                    format = '{down} ↓↑ {up}',
                    foreground = colors[2],
                    background = colors[5],
                    padding = 5
                    ),
                widget.TextBox(
                    text = '',
                    background = colors[5],
                    foreground = colors[4],
                    padding = 0,
                    fontsize = 37
                    ),
                widget.TextBox(
                    text = " 🌡",
                    padding = 2,
                    foreground = colors[2],
                    background = colors[4],
                    fontsize = 11
                    ),
                widget.ThermalSensor(
                    foreground = colors[2],
                    background = colors[4],
                    threshold = 90,
                    padding = 5
                    ),
                widget.TextBox(
                        text = '',
                        background = colors[4],
                        foreground = colors[5],
                        padding = 0,
                        fontsize = 37
                        ),
                widget.CPU(
                    foreground = colors[2],
                    background = colors[5],
                    padding = 5,
                    format = 'CPU {load_percent}%'
                ),
                widget.TextBox(
                        text = '',
                        background = colors[5],
                        foreground = colors[4],
                        padding = 0,
                        fontsize = 37
                        ),
                widget.TextBox(
                        text = " 🖬",
                        foreground = colors[2],
                        background = colors[4],
                        padding = 0,
                        fontsize = 14
                        ),
                widget.Memory(
                        foreground = colors[2],
                        background = colors[4],
                        mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e htop')},
                        padding = 5
                        ),
                widget.TextBox(
                        text = '',
                        background = colors[4],
                        foreground = colors[5],
                        padding = 0,
                        fontsize = 37
                        ),
                widget.TextBox(
                        text = " Vol:",
                        foreground = colors[2],
                        background = colors[5],
                        padding = 0
                        ),
                widget.Volume(
                        foreground = colors[2],
                        background = colors[5],
                        padding = 5
                        ),
                widget.TextBox(
                        text = '',
                        background = colors[5],
                        foreground = colors[4],
                        padding = 0,
                        fontsize = 37
                        ),
                widget.CurrentLayoutIcon(
                        custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
                        foreground = colors[0],
                        background = colors[4],
                        padding = 0,
                        scale = 0.7
                        ),
                widget.CurrentLayout(
                        foreground = colors[2],
                        background = colors[4],
                        padding = 5
                        ),
                widget.TextBox(
                        text = '',
                        background = colors[4],
                        foreground = colors[5],
                        padding = 0,
                        fontsize = 37
                        ),
                widget.Clock(
                        foreground = colors[2],
                        background = colors[5],
                        format = "%A, %B %d - %H:%M "
                        ),
                widget.TextBox(
                        text = '',
                        background = colors[5],
                        foreground = colors[4],
                        padding = 0,
                        fontsize = 37
                        ),
                widget.BatteryIcon(
                        background = colors[4]
                        ),
                widget.Battery(
                        background = colors[4],
                        format = "{char} {percent:2.0%} {hour:d}:{min:02d}"
                        ),
                widget.Sep(
                        linewidth = 0,
                        padding = 6,
                        foreground = colors[2],
                        background = colors[0]
                        ),
            ],
            24,
        ),
    ),
]

# Drag floating layouts.
mouse = [
        Drag([mod], "Button1", lazy.window.set_position_floating(),
            start=lazy.window.get_position()),
        Drag([mod], "Button3", lazy.window.set_size_floating(),
            start=lazy.window.get_size()),
        Click([mod], "Button2", lazy.window.bring_to_front())
        ]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(wm_class='pinentry-gtk-2'),  # GPG key password entry
    Match(title='branchdialog'),  # gitk
    ])
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

