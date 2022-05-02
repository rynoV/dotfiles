import XMonad
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce

main = xmonad def
    { terminal = "kitty",
      modMask = mod4Mask,
      focusedBorderColor = "#88C0D0",
      normalBorderColor = "#2E3440",
      focusFollowsMouse = False,
      startupHook = myStartupHook
    }

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "picom --no-vsync"
  spawnOnce "emacs --daemon"
  checkKeymap myConfig myKeymap
