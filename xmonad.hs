import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

import Codec.Binary.UTF8.String
import Control.Arrow
import Control.OldException
import Data.Bits
import qualified Data.Map as M
import Data.Monoid

import DBus
import DBus.Connection
import DBus.Message

main :: IO ()
main =  withConnection Session $ \ dbus -> do
    getWellKnownName dbus
    xmonad $ gnomeConfig
         { terminal = "gnome-terminal"
         , borderWidth = 2
         , keys = addPrefix (controlMask, xK_m) (newKeys)
         , layoutHook = smartBorders $ layoutHook gnomeConfig
         , logHook    = dynamicLogWithPP $ defaultPP {
                   ppOutput   = \ str -> do
                     let str'  = "<span font=\"Sans 10 Bold\">" ++ str ++ "</span>"
                         str'' = sanitize $ decodeString str'
                     msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log"
                                "Update"
                     addArgs msg [String str'']
                     -- If the send fails, ignore it.
                     send dbus msg 0 `catchDyn`
                       (\ (DBus.Error _name _msg) ->
                         return 0)
                     return ()
                 , ppTitle    = pangoColor "white"
                 , ppCurrent  = pangoColor "green" . wrap "[" "]"
                 , ppVisible  = pangoColor "yellow" . wrap "(" ")"
                 , ppHidden   = const "" --wrap " " " "
                 , ppUrgent   = pangoColor "red"
                 , ppLayout   = const ""
                 , ppSep      = " "
                 }
         , manageHook = composeAll
             [ manageHook gnomeConfig
             , isFullscreen --> doFullFloat
             , title =? "VLC (XVideo output)" --> doFullFloat
             , className =? "Gcalctool" --> doCenterFloat
             , (className =? "Pidgin" <&&> title =? "Buddy List") --> doCenterFloat
             , className =? "Skype" --> doCenterFloat
             , (className =? "Gnome-panel" <&&> title =? "Run Application") --> doCenterFloat
             , title =? "Find in Files" --> doCenterFloat -- MD
             , title =? "NVIDIA X Server Settings" --> doCenterFloat
             ]
         }

newKeys x  =
    M.union (keys gnomeConfig x) (M.fromList (myKeys x))
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

getWellKnownName :: Connection -> IO ()
getWellKnownName dbus = tryGetName `catchDyn` (\(DBus.Error _ _) -> getWellKnownName dbus)
  where
    tryGetName = do
        namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
        addArgs namereq [String "org.xmonad.Log", Word32 5]
        sendWithReplyAndBlock dbus namereq 0
        return ()

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left  = "<span foreground=\"" ++ fg ++ "\">"
  right = "</span>"

sanitize :: String -> String
sanitize [] = []
sanitize (x:rest) | fromEnum x > 127 = "&#" ++ show (fromEnum x) ++ ";" ++ sanitize rest
                  | otherwise        = x : sanitize rest
