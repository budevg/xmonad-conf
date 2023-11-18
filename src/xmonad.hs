import           Data.Maybe                     (isJust)
import           System.Directory
import           System.Exit
import           System.FilePath
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.Minimize
import           XMonad.Actions.SinkAll
import           XMonad.Actions.Submap
import           XMonad.Actions.WithAll
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import qualified XMonad.StackSet                as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run


inXmonDir :: String -> X FilePath
inXmonDir s = do
  dir <- asks (cfgDir . directories)
  return $ dir </> s

restartXmonad = do
  xmonExec <- inXmonDir "xmonad-x86_64-linux"
  restart xmonExec True


getPromptCfg =
  def { position = Top
      , font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
      }
withConfirm msg func =
  confirmPrompt getPromptCfg msg func

copyToAllNonEmpty :: (Eq s, Eq i, Eq a)
                  => W.StackSet i l a s sd -> W.StackSet i l a s sd
copyToAllNonEmpty s = foldr copy s nonEmptyWSTags
  where nonEmptyWSTags = W.tag <$> filter (isJust . W.stack) (W.workspaces s)

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
  , ("M-S-<Backspace>", withConfirm "Quit XMonad" $ io (exitWith ExitSuccess))

  , ("M-S-<Return>", spawn "~/tools/bin/emacs")
  , ("M-h", spawn "exo-open --launch FileManager")
  , ("M-t", spawn "exo-open --launch TerminalEmulator")
  , ("M-l", spawn "slock")
  , ("<Print>", spawn "xfce4-screenshooter -f")
  , ("M1-<Print>", spawn "xfce4-screenshooter -r")
  , ("M-c", spawn "touchpad.sh")
  , ("M-v", spawn "LC_ALL=C rofi -show combi")

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
  , ("M-S-m", withLastMinimized maximizeWindowAndFocus)
  , ("M-<Return>", dwmpromote)
  , ("M-<Delete>", kill1)
  , ("M-S-<Delete>", withConfirm "kill all" $ killAll)
  , ("M-f", sendMessage (Toggle "Full"))

    -- float windows
  , ("M-<Home>", sinkAll)
  , ("M-o", withFocused $ windows . W.sink)

    -- copy windows
  , ("M-u", windows copyToAllNonEmpty)
  , ("M-S-u", killAllOtherCopies)

    -- cycle workspaces
  , ("C-M1-<R>", moveTo Next (hiddenWS :&: Not emptyWS))
  , ("C-M1-<L>", moveTo Prev (hiddenWS :&: Not emptyWS))
  , ("C-M1-S-<R>", shiftToNext >> nextWS)
  , ("C-M1-S-<L>", shiftToPrev >> prevWS)

    -- M-x prefix
  , ("M-x", submapKeys metaXKeys)

  ]
  `additionalKeysP`
  [(mask ++ "M-" ++ [key], action tag)
     | (tag, key)  <- zip (workspaces cfg) "1234567890"
     , (mask, action) <- [ ("", windows . W.greedyView)
                         , ("S-", windows . W.shift)
                         ]
  ]

  where submapKeys ks = submap $ mkKeymap cfg ks
        metaXKeys =
          [ ("m m", spawn "amixer set Master toggle")
          , ("m <U>", spawn "amixer set Master 10%+")
          , ("m <D>", spawn "amixer set Master 10%-")
          ]

getLogHook xmproc = do
  dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "green" "" . shorten 50
    , ppLayout = const ""
    }

getManageHook =
  manageDocks <+> composeAll
  [ resource =? "vlc" --> doFloat
  , className =? "ghidra-Ghidra" --> doFloat
    -- className =? "Firefox" --> doFloat
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
  let cfg = desktopConfig
        { terminal = "xterm"
        , focusFollowsMouse = True
        , clickJustFocuses = False
        , borderWidth = 2
        , modMask = mod4Mask
        , workspaces = map show [1..10 :: Int]
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
  userDir <- getHomeDirectory
  let xmonRootDir = userDir </> ".xmonad"
  let xmobarCfg = xmonRootDir </> "src/xmobar.hs"
  let xmobarBin = xmonRootDir </> "xmobar"
  xmproc <- spawnPipe (xmobarBin ++ " " ++ xmobarCfg)
  launch (getConfig xmproc) $ Directories { dataDir = xmonRootDir
                                          , cfgDir = xmonRootDir
                                          , cacheDir = xmonRootDir
                                          }
