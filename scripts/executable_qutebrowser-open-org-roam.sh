#!/usr/bin/fish

# Generate a spawn command to open the org roam url to capture the
# current page. Needed to do this through a userscript because the
# "open javascript:..." method changed the title, and I tried
# everything to get it to not change the title but couldn't get it
# working. The main issue was getting the title working: opening in a
# new tab meant the title in "document.title" was lost, and using the
# "{title}" variable usually included a space which broke the
# "javascript:..." url. Also binding "spawn" to a keybind didn't work
# because the command interpolation "$(jq ...)" needs an actual
# shell. The jq command to url encode the variables is from
# https://stackoverflow.com/a/34407620/14703577
echo "spawn xdg-open \"org-protocol://roam-ref?template=r&ref=$(jq -rn --arg x $QUTE_URL '$x|@uri')&title=$(jq -rn --arg x $QUTE_TITLE '$x|@uri')\"" >> "$QUTE_FIFO"
