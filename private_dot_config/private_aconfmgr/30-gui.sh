AddPackage lightdm # A lightweight display manager
AddPackage lightdm-gtk-greeter # GTK+ greeter for LightDM
AddPackage picom # X compositor that may fix tearing issues
AddPackage xf86-video-fbdev # X.org framebuffer video driver
AddPackage xmonad # Lightweight X11 tiled window manager written in Haskell
AddPackage xmonad-contrib # Add-ons for xmonad
AddPackage xorg-server # Xorg X server
AddPackage xorg-server-xephyr # A nested X server that runs as an X application
AddPackage xorg-xmessage # Display a message or query in a window

CopyFile /etc/X11/xorg.conf.d/10-monitor.conf
CopyFile /etc/lightdm/lightdm-gtk-greeter.conf
CopyFile /etc/lightdm/lightdm.conf
CopyFile /usr/share/pixmaps/lightdm-wallpaper.png
