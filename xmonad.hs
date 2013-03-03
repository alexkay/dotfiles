{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Xfce
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

import Control.Arrow
import Data.Bits
import qualified Data.Map as M
import Data.Monoid

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
    dbus <- D.connectSession
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

prettyPrinter :: D.Client -> PP
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

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

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
