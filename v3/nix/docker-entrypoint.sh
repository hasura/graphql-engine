#!/bin/sh
# Vendored from hasura/lux:scripts/docker-entrypoint/docker-entrypoint.sh.
#
# Reads JSON files from /secrets/, exports their key-value pairs as environment
# variables, and execs the wrapped command. When ENABLE_AUTO_RESTART_ON_SECRET_CHANGE
# is "true", also watches /secrets for changes and signals PID 1 to restart.
#
# Source of truth lives in hasura/lux; keep this copy in sync if the upstream
# script changes.
set -e
SECRETS_DIR="/secrets"
AUTO_RESTART_ON_SECRET_CHANGE="${ENABLE_AUTO_RESTART_ON_SECRET_CHANGE:-false}"

parse_json_and_export() {
  local json_file="$1"
  keys=$(jq -r 'keys[]' "$json_file")
  for key in $keys; do
    # Get value directly using the key
    value=$(jq -r --arg k "$key" '.[$k]' "$json_file")
    # Export the key-value pair
    export "$key"="$value"
  done
}

# Iterate over each JSON file in the directory
for json_file in "$SECRETS_DIR"/*.json; do
  if [ -f "$json_file" ]; then
    parse_json_and_export "$json_file"
  fi
done

# Start watcher in background


if [ "$AUTO_RESTART_ON_SECRET_CHANGE" = "true" ]; then
  # Calculate checksums for all JSON files in the secrets directory
  calculate_checksums() {
    local checksums=""
    for json_file in "$SECRETS_DIR"/*.json; do
      if [ -f "$json_file" ]; then
        # Use jq to get a hash of the content
        if [ -s "$json_file" ]; then
          # Get a hash of the entire JSON content, sorted to ensure consistent output
          file_hash=$(jq -cS '.' "$json_file" 2>/dev/null || echo "error")
          # If that fails, just check if jq can parse it
          if [ "$file_hash" = "error" ]; then
            if jq '.' "$json_file" >/dev/null 2>&1; then
              file_hash="valid_json"
            else
              file_hash="invalid_json"
            fi
          fi
        else
          file_hash="empty"
        fi
        checksums="$checksums$json_file:$file_hash\n"
      fi
    done
    echo "$checksums"
  }

  # Read and export all valid secrets
  read_and_export_secrets() {
    local valid_secrets_found=false
    for json_file in "$SECRETS_DIR"/*.json; do
      if [ -f "$json_file" ] && [ -s "$json_file" ]; then
        if jq -e 'length > 0' "$json_file" >/dev/null 2>&1; then
          parse_json_and_export "$json_file"
          valid_secrets_found=true
        fi
      fi
    done
    echo "$valid_secrets_found"
  }

  # Handle changes in secrets
  handle_secret_changes() {
    echo "Secret files changed, updating environment..."
    # Read and export secrets, get result
    valid_secrets_found=$(read_and_export_secrets)
    # Only restart if valid secrets were found
    if [ "$valid_secrets_found" = "true" ]; then
      echo "Secret content changed with valid secrets, restarting container..."
      kill -TERM 1
    else
      echo "No valid secrets found, skipping restart..."
    fi
  }

  # Monitor secrets directory for changes
  monitor_secrets_directory() {
    echo "Polling for changes in secrets directory..."
    local previous_checksums=""
    while inotifywait --quiet -e modify,create,delete,move "$SECRETS_DIR"; do
      echo "notification received from inotify for a change in secrets directory..."
      # Calculate current checksums
      local current_checksums=$(calculate_checksums)
      # check only if we have a previous checksum which will confirm
      # that we have already read the secrets at least once
      # and then compare with current checksums
      if [ -n "$previous_checksums" ] && [ "$previous_checksums" != "$current_checksums" ]; then
        handle_secret_changes
      else
        echo "First run or no meaningful changes detected, continuing..."
      fi
      # Store current checksums for next comparison
      previous_checksums="$current_checksums"
    done
  }

  # Start watcher in background
  (
    monitor_secrets_directory
  ) &
fi


# Execute the passed command with all environment variables
exec "$@"
