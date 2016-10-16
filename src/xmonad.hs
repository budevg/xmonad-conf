import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Minimize
import XMonad.Layout.ResizableTile
import XMonad.Actions.CopyWindow
import XMonad.Actions.SinkAll
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.DwmPromote
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import System.FilePath
import System.Exit


inXmonDir s = do
  dir <- getXMonadDir
  return $ dir </> s

restartXmonad = do
  xmonExec <- inXmonDir "xmonad-x86_64-linux"
  restart xmonExec True

getKeysBindings cfg = cfg
  `removeKeysP`
  [ "M-q"
  , "M-S-q"
  , "M-S-<Return>"
  , "M-<Tab>"
  , "M-S-<Tab>"
  ]
  `additionalKeysP`
  [ -- restart xmonad
    ("M-<Backspace>", restartXmonad)
  , ("M-S-<Backspace>", io (exitWith ExitSuccess))

  , ("M-S-<Return>", spawn "exo-open --launch TerminalEmulator")
  , ("M-h", spawn "exo-open --launch FileManager")
  , ("M-t", spawn "exo-open --launch TerminalEmulator")
  , ("M-l", spawn "slock")
  -- , ("<Print>", spawn "xfce4-screenshooter -f")
  -- , ("M1-<Print>", spawn "xfce4-screenshooter -w")

    -- windows focus:
  , ("M1-<Tab>", windows W.focusDown)
  , ("M1-S-<Tab>", windows W.focusUp)
  , ("M-<L>", sendMessage $ Go L)
  , ("M-<R>", sendMessage $ Go R)
  , ("M-<U>", sendMessage $ Go U)
  , ("M-<D>", sendMessage $ Go D)

    -- windows layout
  , ("M-<Page_Up>", sendMessage (IncMasterN 1))
  , ("M-<Page_Down>", sendMessage (IncMasterN (-1)))
  , ("M-S-<L>", sendMessage $ Swap L)
  , ("M-S-<R>", sendMessage $ Swap R)
  , ("M-S-<U>", sendMessage $ Swap U)
  , ("M-S-<D>", sendMessage $ Swap D)
  , ("C-M-<D>", sendMessage MirrorShrink)
  , ("C-M-<U>", sendMessage MirrorExpand)
  , ("C-M-<L>", sendMessage Shrink)
  , ("C-M-<R>", sendMessage Expand)
  , ("M-m", withFocused minimizeWindow)
  , ("M-S-m", sendMessage RestoreNextMinimizedWin)
  , ("M-<Return>", dwmpromote)
  , ("M-<Delete>", kill1)
  , ("M-f", sendMessage (Toggle "Full"))

    -- float windows
  , ("M-<Home>", sinkAll)
  , ("M-o", withFocused $ windows . W.sink)

    -- cycle workspaces
  , ("C-M1-<R>", moveTo Next NonEmptyWS)
  , ("C-M1-<L>", moveTo Prev NonEmptyWS)
  , ("C-M1-S-<R>", shiftToNext >> nextWS)
  , ("C-M1-S-<L>", shiftToPrev >> prevWS)

    -- M-x prefix
  , ("M-x", submapKeys metaXKeys)

  ]
  where submapKeys ks = submap $ mkKeymap cfg ks
        metaXKeys =
          [ ("m m", spawn "amixer -D pulse set Master toggle")
          , ("m <U>", spawn "amixer -D pulse set Master 10%+")
          , ("m <D>", spawn "amixer -D pulse set Master 10%-")          ]

getLogHook xmproc = do
  dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "green" "" . shorten 50
    , ppLayout = const ""
    }

getManageHook =
  manageDocks <+> composeAll
  [ -- className =? "Firefox" --> doFloat
  ]

getLayoutHook = toggleLayouts (avoidStruts $ noBorders Full) $
  smartBorders $
  windowNavigation $
  minimize $
  desktopLayoutModifiers basicLayout

  where basicLayout = tiled ||| Mirror tiled ||| Full
        tiled   = ResizableTall nmaster delta ratio []
        nmaster = 1
        ratio   = 1 / 2
        delta   = 5 / 100

getStartupHook = do
  path <- inXmonDir "scripts/startup-hook.sh"
  spawn path

getConfig xmproc =
  let cfg = def
        { terminal = "xterm"
        , focusFollowsMouse = False
        , clickJustFocuses = False
        , borderWidth = 2
        , modMask = mod4Mask
        -- , workspaces = undefined
        , normalBorderColor = "#2e505a"
        , focusedBorderColor = "#bb1d1d"
        -- , keys = undefined
        -- , mouseBindings = undefined
        , layoutHook = getLayoutHook
        , manageHook = getManageHook
        -- , handleEventHook = undefined
        , logHook = getLogHook xmproc
        , startupHook = getStartupHook
        }
  in getKeysBindings cfg

main :: IO ()
main = do
  xmobarCfg <- inXmonDir "src/xmobar.hs"
  xmproc <- spawnPipe ("xmobar " ++ xmobarCfg)
  xmonad $ getConfig xmproc
