import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.Submap

import Control.Arrow
import Data.Bits
import qualified Data.Map as M

main :: IO ()
main = do
    xmonad $ gnomeConfig
         { terminal = "gnome-terminal"
         , focusFollowsMouse = False
         , borderWidth = 2
         , keys = addPrefix (controlMask, xK_m) (keys gnomeConfig)
         }

addPrefix p ms conf =
    M.singleton p . submap $ M.mapKeys (first chopMod) (ms conf)
    where
    mod = modMask conf
    chopMod = (.&. complement mod)