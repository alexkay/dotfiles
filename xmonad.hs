import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.Submap
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

import Control.Arrow
import Data.Bits
import qualified Data.Map as M
import Data.Monoid

main :: IO ()
main = do
    xmonad $ gnomeConfig
         { terminal = "gnome-terminal"
         , focusFollowsMouse = False
         , borderWidth = 2
         , keys = addPrefix (controlMask, xK_m) (newKeys)
         , layoutHook = smartBorders $ layoutHook gnomeConfig
         }

myKeys x =
    [ ((modMask x, xK_f), fullFloatFocused)
    ]

newKeys x  =
    M.union (keys gnomeConfig x) (M.fromList (myKeys x))

addPrefix p ms conf =
    M.singleton p . submap $ M.mapKeys (first chopMod) (ms conf)
    where
    mod = modMask conf
    chopMod = (.&. complement mod)

fullFloatFocused =
    withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f