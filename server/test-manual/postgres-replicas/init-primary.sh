#!/usr/bin/env bash

set -e
set -x

# Allow all users to replicate the database.
# This is very insecure.

auth="$(postgres -C password_encryption)"
echo "host replication all all ${auth}" >> "$PGDATA/pg_hba.conf"
