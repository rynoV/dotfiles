#!/bin/sh
# Starts dbhub for the auspicedb MCP server. Run by pi (see ../mcp.json) as `sh <this> <config>`.
# The connection details come from the environment when set (containers, where the launcher
# passes them in). Otherwise they are read from the "Auspice DB" entry with one `rbw` call.
# Doing this here, rather than in mcp.json's env block, keeps pi's input responsive: pi runs
# this process asynchronously, but evaluates env values synchronously at startup.
# A locked rbw would prompt for the master password, so check first and fail with a message.
if [ -z "$AUSPICE_DB_USER" ]; then
  if ! rbw unlocked >/dev/null 2>&1; then
    echo "rbw is locked: run 'rbw unlock', then reconnect auspicedb with /mcp" >&2
    exit 1
  fi
  eval "$(rbw get 'Auspice DB' --raw | jq -r '"export AUSPICE_DB_USER=\(.data.username|@sh) AUSPICE_DB_PASSWORD=\(.data.password|@sh) AUSPICE_DB_HOST=\((.fields[] | select(.name == "host").value)|@sh) AUSPICE_DB_DATABASE=\((.fields[] | select(.name == "database").value)|@sh)"')"
fi
exec npx -y @bytebase/dbhub@2.0.0 --transport stdio --config "$1"
