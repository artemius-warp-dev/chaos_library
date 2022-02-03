import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (fromJust)
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CycleWS
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import System.IO (hPutStrLn)
import XMonad.Hooks.ManageDocks

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.

--colors
data ColorSchemes = ColorSchemes{black ,white ,gray ,yellow ,orange ,red ,purple ,blue ,cyan ,green :: String}

myGruvbox :: ColorSchemes
myGruvbox = ColorSchemes {
                          black   = "#282828",
                          white   = "#ebdbb2",
                          gray    = "#928374",
                          yellow  = "#fabd2f",
                          orange  = "#fe8019",
                          red     = "#fb4934",
                          purple  = "#d3869b",
                          blue    = "#83a598",
                          cyan    = "#8ec07c",
                          green   = "#b8bb26"
                         }

myTerminal      = "gnome-terminal"
myColor         = myGruvbox

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
myWorkspaces    = ["code","web","social","music"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]
clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- toggle fullscreen
    , ((modm,               xK_f     ), sendMessage (Toggle "Full"))

    ,((modm,                xK_b     ), sendMessage ToggleStruts)

    -- close focused window
    , ((modm,               xK_q     ), kill)

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    -- Functional keys binding
    , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10%-")
    , ((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set 'Master' 1%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set 'Master' 1%+")
    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")

    -- a basic CycleWS setup

    , ((modm,               xK_Down),  nextWS)
    , ((modm,               xK_Up),    prevWS)
    , ((modm .|. shiftMask, xK_Down),  shiftToNext)
    , ((modm .|. shiftMask, xK_Up),    shiftToPrev)

    -- ScreeenShot
    , ((modm .|. shiftMask, xK_Print), spawn "sleep 0.2; scrot -sf '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/'")
    , ((modm, xK_Print), spawn "scrot")
    -- Download youtube mp3
    , ((modm .|. shiftMask, xK_y), spawn "~/Documents/code/configs/chaos_library/scripts/youtube_dl.sh")
    ]
    -- Workspaces moveset for clickable xmobar's titles
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_4]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
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
myLayout =
         avoidStruts $
         smartBorders $
         toggleLayouts (noBorders Full) (Tall 1 (3/100) (1/2))
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
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


myXmobarPP h =
  xmobarPP
    { ppCurrent         = xmobarColor (green myColor) "" . wrap "[" "]",
      ppVisible         = xmobarColor (white myColor) "" . wrap "" "" . clickable,
      ppHidden          = xmobarColor (yellow myColor) "" . wrap "" "" . clickable,
      ppHiddenNoWindows = xmobarColor (white myColor) "" . clickable,
      ppSep             = " | ",
      ppTitle           = xmobarColor (white myColor) "" . shorten 60,
      ppLayout          = xmobarColor  (white myColor) "",
      ppOutput          = hPutStrLn h,
      --ppExtras          = [windowCount],
      ppOrder           = \(ws : l : t : e) -> [ws]
    }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
              spawn "killall trayer"
              spawnOnce "nitrogen --restore &"
              spawnOnce "compton --backend glx --paint-on-overlay --vsync opengl-swc &"
              spawnOnce "redshift -l 59.93:30.31 -t 6500:2500 &"
              spawnOnce "libinput-gestures-setup start"
              spawnOnce "auto-cpufreq --live &"
              spawnOnce "nm-applet &"
              spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 100 --tint 0x292d3e  --height 18")

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
     myXmobar <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
     xmonad $ ewmh $ docks def
            {  manageHook         = manageDocks <+> myManageHook
            ,  startupHook        = myStartupHook
            ,  layoutHook         = myLayout
            ,  logHook            = dynamicLogWithPP $ myXmobarPP myXmobar
            ,  handleEventHook    = myEventHook
            ,  modMask            = myModMask
            ,  terminal           = myTerminal
            ,  focusFollowsMouse  = myFocusFollowsMouse
            ,  clickJustFocuses   = myClickJustFocuses
            ,  workspaces         = myWorkspaces
            ,  keys               = myKeys
            ,  mouseBindings      = myMouseBindings
            } `additionalKeys` [
                ((myModMask,     xK_w), spawn "google-chrome")
                ]  






