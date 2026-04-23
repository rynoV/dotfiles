# Combination of
# https://github.com/github/platform-samples/blob/17d2a3ac382a1aff3d33079db7df4d243c380488/pre-receive-hooks/block_unsigned_commits.sh
# and
# Source - https://stackoverflow.com/a/75864276
# Retrieved 2026-04-23, License - CC BY-SA 4.0

#!/bin/sh

zero_commit="0000000000000000000000000000000000000000"

while read local_ref local_sha remote_ref remote_sha
do
  # Branch/tag deleted
  if [ "$local_sha" = "$zero_commit" ]; then
    continue
  fi

  # Exclude all commits present on the remote
  if [ "$remote_sha" = "$zero_commit" ]; then
    # This is a new branch/tag
    span=`git rev-list $local_sha --not --remotes`
  else
    span=`git rev-list $remote_sha..$local_sha --not --remotes`
  fi

  for commit in $span
  do
    if ! git verify-commit $commit >/dev/null 2>&1
    then
      >&2 echo Commit $commit was not signed, rejecting push
      exit 1
    fi
  done
done

