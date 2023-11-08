#  |||||||||||     ||||||||     |\\       ||  ||||||||||||
# ||||           ||||||||||||   ||\\      ||  ||
#|||            ||||      ||||  || \\     ||  ||
#||             |||        |||  ||  \\    ||  ||
#||             |||        |||  ||   \\   ||  ||||||||
#||             |||        |||  ||    \\  ||  ||
#||             |||        |||  ||     \\ ||  ||
#|||            ||||      ||||  ||      \\||  ||
# ||||           ||||||||||||   ||       \||  ||
#  |||||||||||     ||||||||     ||        \|  ||


config.load_autoconfig()

# https://github.com/Linuus/nord-qutebrowser
config.source('nord-qutebrowser.py')

config.bind("<Ctrl-r>", "spawn --userscript ~/scripts/qutebrowser-open-org-roam.sh")
