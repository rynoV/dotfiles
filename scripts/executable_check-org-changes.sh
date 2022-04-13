#!/bin/bash

# Script from here
# https://blog.cadena-it.com/linux-tips-how-to/how-to-detect-changes-in-a-directory-with-bash/
# to check for changes in the folder

DIR_TO_CHECK="$HOME/org"
OLD_SUM_FILE="/tmp/prev-sum.txt"
if [ -e $OLD_SUM_FILE ]
then
    OLD_SUM=`cat $OLD_SUM_FILE`
else
    OLD_SUM=""
fi
NEW_SUM=`find $DIR_TO_CHECK -name '*.org*' -print0 | xargs -0 du -b --time | sort -k4,4 | sha1sum | awk '{print $1}'`
if [ "$OLD_SUM" != "$NEW_SUM" ]
then
    echo $NEW_SUM > $OLD_SUM_FILE
    exit 0
fi

exit 1
