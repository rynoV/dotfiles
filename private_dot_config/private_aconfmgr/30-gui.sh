AddPackage lightdm # A lightweight display manager
AddPackage lightdm-gtk-greeter # GTK+ greeter for LightDM
AddPackage xorg-xinit # X.Org initialisation program
AddPackage picom # X compositor that may fix tearing issues
AddPackage xf86-video-fbdev # X.org framebuffer video driver
AddPackage xf86-video-intel # X.org Intel i810/i830/i915/945G/G965+ video drivers
AddPackage xorg-server # Xorg X server
AddPackage xorg-server-xephyr # A nested X server that runs as an X application
AddPackage xorg-xmessage # Display a message or query in a window
AddPackage gtk2 # GObject-based multi-platform GUI toolkit (legacy), needed for git credential manager/gpg to popup a password entry window

CopyFile /etc/X11/xorg.conf.d/10-monitor.conf
CopyFile /etc/lightdm/lightdm-gtk-greeter.conf
CopyFile /etc/lightdm/lightdm.conf
CopyFile /usr/share/pixmaps/lightdm-wallpaper.png
