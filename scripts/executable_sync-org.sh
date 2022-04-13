#!/bin/bash

# - To setup, run `crontab -e` and append `*/1 * * * * ~/scripts/push-org.sh` (runs the script every minute)

# Uncomment to enable logging
# exec &>> ~/scripts/sync-org.log

echo `date`

# Only push if changes have been made
if ~/scripts/check-org-changes.sh
then
    echo "Pushing changes"
    # Update org mobile files, starting an emacs server if necessary
    if ! emacsclient --socket-name orgsync --eval "(calum/org-mobile-sync)"
    then
        echo "Starting emacs daemon for syncing changes"
        emacs --daemon=orgsync
        emacsclient --socket-name orgsync --eval "(calum/org-mobile-sync)"
    fi
    echo "Done pushing changes"
else
    echo "No changes, not pushing"
fi
