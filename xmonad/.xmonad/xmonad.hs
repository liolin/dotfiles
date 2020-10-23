--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.WithAll (sinkAll, killAll)
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: [Char]
myTerminal = "alacritty"

myBrowser :: [Char]
myBrowser = "firefox"

myEditor :: [Char]
myEditor = "emacsclient"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [[Char]]
myWorkspaces    = ["term","doom","web", "music"] ++ map show [5..9]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: [Char]
myFocusedBorderColor :: [Char]
myNormalBorderColor  = "#282a36"
myFocusedBorderColor = "#50fa7b"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: [(String, X ())]
myKeys =
        [
    -- Xmonad
          ("M-C-r", spawn "xmonad --recompile && xmonad --restart")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")                            -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                                      -- Quits xmonad
        , ("M-<Return>", spawn (myTerminal))

    -- Window
        , ("M-S-c", kill)
        , ("M-S-a", killAll)                         -- Kill all windows on current workspace

    -- Layout
        , ("M-<Space>", sendMessage NextLayout)     -- Switch to next layout
        , ("M-n", refresh)                          -- Resize viewed windows to the correct size
        , ("M-<Tab>", windows W.focusDown)          -- Move focus to the next window
        , ("M-j", windows W.focusDown)              -- Move focus to the next window
        , ("M-k", windows W.focusUp  )              -- Move focus to the previous window
        , ("M-m", windows W.focusMaster  )          -- Move focus to the master window
        , ("M-S-<Return>", windows W.swapMaster)    -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown  )           -- Swap the focused window with the next window
        , ("M-S-k", windows W.swapUp    )           -- Swap the focused window with the previous window
        , ("M-h", sendMessage Shrink)               -- Shrink the master area
        , ("M-l", sendMessage Expand)               -- Expand the master area
        , ("M-t", withFocused $ windows . W.sink)   -- Push window back into tiling
        , ("M-,", sendMessage (IncMasterN 1))       -- Increment the number of windows in the master area
        , ("M-.", sendMessage (IncMasterN (-1)))    -- Deincrement the number of windows in the master area

    -- Menus
        , ("M-p", spawn "rofi -show drun")
        , ("M-S-p", spawn "/usr/bin/env bash ~/.xmonad/rofi_power") -- Run power menu
    -- Emacs
        , ("C-e e", spawn "emacsclient -c -a ''")                           -- start emacs
        , ("C-e b", spawn "emacsclient -c -a '' --eval '(ibuffer)'")        -- list emacs buffers
        , ("C-e d", spawn "emacsclient -c -a '' --eval '(dired nil)'")      -- dired emacs file manager
        , ("C-e m", spawn "emacsclient -c -a '' --eval '(mu4e)'")           -- mu4e emacs email client
        , ("C-e s", spawn "emacsclient -c -a '' --eval '(eshell)'")         -- eshell within emacs
        , ("C-e a", spawn "emacsclient -c -a '' --eval '(org-agenda)'")     -- open org agenda

    -- Applications
        , ("<F12>", spawn "pcmanfm") -- Run filemanager

    -- Multimedia Keys
        , ("<XF86AudioMute>", spawn "audioctl -m")                  -- Toggle mute
        , ("<XF86AudioLowerVolume>", spawn "audioctl -l")           -- Lower Volume
        , ("<XF86AudioRaiseVolume>", spawn "audioctl -r")           -- Raise Volume
        , ("<XF86AudioMicMute>", spawn "amixer set Capture toggle") -- Toggle microphone
        , ("<XF86AudioPlay>", spawn "cmus-remote --pause")          -- Toggle pause
        , ("<XF86AudioPrev>", spawn "cmus-remote --prev")           -- Skip backwards in playlist
        , ("<XF86AudioNext>", spawn "cmus-remote --next")           -- Skip forwards in playlist
        ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Pavucontrol"    --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: Event -> X All
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
bg1       = "#3c3836"
bg2       = "#504945"
red       = "#fb4934"
myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
    , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " : "
    , ppTitle = shorten 40
    }
   
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "/usr/bin/sh ~/.xmonad/autostart.sh"
  spawnOnce "/usr/bin/sh ~/.config/polybar/launch.sh"
  ------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  --h <- spawnPipe "xmobar"
  xmonad $ docks defaults {
    -- logHook = dynamicLogWithPP $ def { ppOutput = hPutStrLn h }
    logHook = dynamicLogWithPP (myLogHook dbus)
    } `additionalKeysP` myKeys 

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        --logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Shift-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
