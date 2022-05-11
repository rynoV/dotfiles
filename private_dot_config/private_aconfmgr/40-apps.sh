AddPackage bat # Cat clone with syntax highlighting and git integration
AddPackage bitwarden-cli # The command line vault
AddPackage emacs-nativecomp # The extensible, customizable, self-documenting real-time display editor with native compilation enabled
AddPackage fd # Simple, fast and user-friendly alternative to find
AddPackage fish # Smart and user friendly shell intended mostly for interactive use
AddPackage fzf # Command-line fuzzy finder
AddPackage git # the fast distributed version control system
AddPackage htop # Interactive process viewer
AddPackage kitty # A modern, hackable, featureful, OpenGL-based terminal emulator
AddPackage neofetch # A CLI system information tool written in BASH that supports displaying images.
AddPackage python-pillow # Python Imaging Library (PIL) fork. Installed for images in ranger+kitty to work
AddPackage ranger # Simple, vim-like file manager
AddPackage ripgrep-all # rga: ripgrep, but also search in PDFs, E-Books, Office documents, zip, tar.gz, etc.
AddPackage vivaldi # An advanced browser made with the power user in mind.
AddPackage z # Tracks your most used directories, based on 'frecency'
AddPackage xclip # Command line interface to the X11 clipboard
AddPackage copyq # Clipboard manager with searchable and editable history
AddPackage zathura # Minimalistic document viewer
AddPackage zathura-pdf-poppler # Adds pdf support to zathura by using the poppler engine
AddPackage broot # Fuzzy Search + tree + cd
AddPackage handlr # Powerful alternative to xdg-utils written in Rust
AddPackage rclone # Sync files to and from Google Drive, S3, Swift, Cloudfiles, Dropbox and Google Cloud Storage
AddPackage jq # Command-line JSON processor
AddPackage qutebrowser # A keyboard-driven, vim-like browser based on PyQt5
AddPackage python-adblock # Optional with qutebrowser
AddPackage light # Program to easily change brightness on backlight-controllers.
AddPackage xbindkeys # Launch shell commands with your keyboard or your mouse under X
AddPackage xorg-xset # User preference utility for X
AddPackage arandr # Provide a simple visual front end for XRandR 1.2.
AddPackage autorandr # Auto-detect connected display hardware and load appropiate X11 setup using xrandr
AddPackage libnotify # Library for sending desktop notifications, provides notify-send
AddPackage notification-daemon # Server implementation of the freedesktop.org desktop notification specification
AddPackage pass # Stores, retrieves, generates, and synchronizes passwords securely
AddPackage chromium # A web browser built for speed, simplicity, and security
AddPackage feh # Fast and light imlib2-based image viewer
AddPackage variety # Changes the wallpaper on a regular interval using user-specified or automatically downloaded images.
AddPackage alsa-utils # Advanced Linux Sound Architecture - Utilities

AddPackage --foreign aconfmgr-git # A configuration manager for Arch Linux
AddPackage --foreign git-credential-manager-core-bin # Secure, cross-platform Git credential storage with authentication to GitHub, Azure Repos, and other popular Git hosting services.
AddPackage --foreign ttf-ms-fonts # Installed to get git-credential-manager-core-bin working, based on advice here: https://github.com/AvaloniaUI/Avalonia/issues/4427#issuecomment-669173654
AddPackage --foreign ghcup-hs-bin # an installer for the general purpose language Haskell
AddPackage --foreign shell-color-scripts # A CLI for the collection of terminal color scripts. Included 52 beautiful terminal color scripts.
AddPackage --foreign xdg-utils-handlr # A shim for xdg-utils to use handlr under the hood

CreateDir /etc/light
CopyFile /etc/udev/rules.d/backlight.rules
CopyFile /etc/udev/rules.d/50-oryx.rules

CopyFile /opt/shell-color-scripts/colorscripts/awk-rgb-test
CopyFile /opt/shell-color-scripts/colorscripts/00default.sh
CopyFile /opt/shell-color-scripts/colorscripts/alpha
CopyFile /opt/shell-color-scripts/colorscripts/arch
CopyFile /opt/shell-color-scripts/colorscripts/bars
CopyFile /opt/shell-color-scripts/colorscripts/blocks1
CopyFile /opt/shell-color-scripts/colorscripts/blocks2
CopyFile /opt/shell-color-scripts/colorscripts/bloks
CopyFile /opt/shell-color-scripts/colorscripts/colorbars
CopyFile /opt/shell-color-scripts/colorscripts/colortest
CopyFile /opt/shell-color-scripts/colorscripts/colortest-slim
CopyFile /opt/shell-color-scripts/colorscripts/colorview
CopyFile /opt/shell-color-scripts/colorscripts/colorwheel
CopyFile /opt/shell-color-scripts/colorscripts/crowns
CopyFile /opt/shell-color-scripts/colorscripts/crunch
CopyFile /opt/shell-color-scripts/colorscripts/crunchbang
CopyFile /opt/shell-color-scripts/colorscripts/crunchbang-mini
CopyFile /opt/shell-color-scripts/colorscripts/darthvader
CopyFile /opt/shell-color-scripts/colorscripts/debian
CopyFile /opt/shell-color-scripts/colorscripts/dna
CopyFile /opt/shell-color-scripts/colorscripts/doom-original
CopyFile /opt/shell-color-scripts/colorscripts/doom-outlined
CopyFile /opt/shell-color-scripts/colorscripts/elfman
CopyFile /opt/shell-color-scripts/colorscripts/faces
CopyFile /opt/shell-color-scripts/colorscripts/fade
CopyFile /opt/shell-color-scripts/colorscripts/ghosts
CopyFile /opt/shell-color-scripts/colorscripts/guns
CopyFile /opt/shell-color-scripts/colorscripts/hex
CopyFile /opt/shell-color-scripts/colorscripts/illumina
CopyFile /opt/shell-color-scripts/colorscripts/jangofett
CopyFile /opt/shell-color-scripts/colorscripts/kaisen
CopyFile /opt/shell-color-scripts/colorscripts/manjaro
CopyFile /opt/shell-color-scripts/colorscripts/monster
CopyFile /opt/shell-color-scripts/colorscripts/mouseface
CopyFile /opt/shell-color-scripts/colorscripts/mouseface2
CopyFile /opt/shell-color-scripts/colorscripts/pacman
CopyFile /opt/shell-color-scripts/colorscripts/panes
CopyFile /opt/shell-color-scripts/colorscripts/pinguco
CopyFile /opt/shell-color-scripts/colorscripts/print256
CopyFile /opt/shell-color-scripts/colorscripts/pukeskull
CopyFile /opt/shell-color-scripts/colorscripts/rails
CopyFile /opt/shell-color-scripts/colorscripts/rally-x
CopyFile /opt/shell-color-scripts/colorscripts/rupees
CopyFile /opt/shell-color-scripts/colorscripts/six
CopyFile /opt/shell-color-scripts/colorscripts/space-invaders
CopyFile /opt/shell-color-scripts/colorscripts/spectrum
CopyFile /opt/shell-color-scripts/colorscripts/square
CopyFile /opt/shell-color-scripts/colorscripts/suckless
CopyFile /opt/shell-color-scripts/colorscripts/tanks
CopyFile /opt/shell-color-scripts/colorscripts/thebat
CopyFile /opt/shell-color-scripts/colorscripts/thebat2
CopyFile /opt/shell-color-scripts/colorscripts/tiefighter1
CopyFile /opt/shell-color-scripts/colorscripts/tiefighter1-no-invo
CopyFile /opt/shell-color-scripts/colorscripts/tiefighter1row
CopyFile /opt/shell-color-scripts/colorscripts/tiefighter2
CopyFile /opt/shell-color-scripts/colorscripts/tux
CopyFile /opt/shell-color-scripts/colorscripts/xmonad
CopyFile /opt/shell-color-scripts/colorscripts/zwaves
