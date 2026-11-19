#!/bin/sh
# Starts dbhub for the auspicedb MCP server. Run by pi (see ../mcp.json) as `sh <this> <config>`.
# The connection details come from the environment when set (containers, where the launcher
# passes them in). Otherwise they are read from the "Auspice DB" Bitwarden item in one `bw` call.
# Doing this here, rather than in mcp.json's env block, keeps pi's input responsive: pi runs
# this process asynchronously, but evaluates env values synchronously at startup.
if [ -z "$AUSPICE_DB_USER" ]; then
  eval "$(bw get item 'Auspice DB' | jq -r '"export AUSPICE_DB_USER=\(.login.username|@sh) AUSPICE_DB_PASSWORD=\(.login.password|@sh) AUSPICE_DB_HOST=\((.fields[] | select(.name == "host").value)|@sh) AUSPICE_DB_DATABASE=\((.fields[] | select(.name == "database").value)|@sh)"')"
fi
exec npx -y @bytebase/dbhub@2.0.0 --transport stdio --config "$1"
