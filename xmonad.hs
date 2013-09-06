import XMonad
import XMonad.Config.Xfce
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Arrow
import Control.Exception
import Data.Bits
import qualified Data.Map as M
import Data.Monoid

import DBus
import DBus.Connection
import DBus.Message

main :: IO ()
main =  withConnection Session $ \dbus -> do
    getWellKnownName dbus
    xmonad $ xfceConfig
         { modMask = mod4Mask
         , terminal = "xfce4-terminal"
         , borderWidth = 2
         , keys = addPrefix (controlMask, xK_m) (newKeys)
         , layoutHook = smartBorders $ layoutHook xfceConfig
         , logHook    = dynamicLogWithPP (prettyPrinter dbus)
         , manageHook = composeAll
             [ manageHook xfceConfig
             , isFullscreen --> doFullFloat
             , title =? "VLC (XVideo output)" --> doFullFloat
             , className =? "Gcalctool" --> doCenterFloat
             , (className =? "Pidgin" <&&> title =? "Buddy List") --> doCenterFloat
             , className =? "Skype" --> doCenterFloat
             , title =? "Application Finder" --> doCenterFloat
             , title =? "File Operation Progress" --> doCenterFloat
             , title =? "Find in Files" --> doCenterFloat -- MD
             , title =? "NVIDIA X Server Settings" --> doCenterFloat
             ]
         }

newKeys x  =
    M.union (keys xfceConfig x) (M.fromList (myKeys x))
  where
    myKeys x =
        [ ((modMask x, xK_f), fullFloatFocused)
        ]

addPrefix p ms conf =
    M.singleton p . submap $ M.mapKeys (first chopMod) (ms conf)
    where
    mod = modMask conf
    chopMod = (.&. complement mod)

fullFloatFocused =
    withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

-- xmonad-log-applet hook

prettyPrinter :: DBus.Connection.Connection -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

getWellKnownName :: DBus.Connection.Connection -> IO ()
getWellKnownName dbus = tryGetName `catch` (\(DBus.Error _ _) -> getWellKnownName dbus)
  where
    tryGetName = do
        namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
        addArgs namereq [String "org.xmonad.Log", Word32 5]
        sendWithReplyAndBlock dbus namereq 0
        return ()

dbusOutput :: DBus.Connection.Connection -> String -> IO ()
dbusOutput dbus str = do
    msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
    addArgs msg [String ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
    -- If the send fails, ignore it.
    send dbus msg 0 `catch` (\(DBus.Error _ _) -> return 0)
    return ()

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left  = "<span foreground=\"" ++ fg ++ "\">"
  right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
