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
AddPackage slock # A simple screen locker for X
AddPackage xorg-xprop # Property displayer for X
AddPackage zenity # Display graphical dialog boxes from shell scripts
AddPackage xorg-xsetroot # Classic X utility to set your root window background to a given pattern or color
AddPackage xorg-xset # User preference utility for X

CopyFile /etc/X11/xorg.conf.d/10-monitor.conf
CopyFile /etc/lightdm/lightdm-gtk-greeter.conf
CopyFile /etc/lightdm/lightdm.conf
CopyFile /usr/share/pixmaps/lightdm-wallpaper.png

CreateLink /etc/systemd/system/sleep.target.wants/slock@calum.service /etc/systemd/system/slock@.service
CopyFile /etc/systemd/system/slock@.service
