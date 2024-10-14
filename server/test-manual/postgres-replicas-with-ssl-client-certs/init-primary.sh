#!/usr/bin/env bash

set -e
set -x

# Allow all users to replicate the database.
# This is very insecure.

cd $PGDATA

auth="$(postgres -C password_encryption)"

# pg_hba records are matched eagerly first-to-last. So we need to have our
# strict no-tcp-without-ssl policy appear first to have any effect.

# We want to only permit connections that use ssl with client certificate
# validation, except for the replica.

#hostssl all all all trust clientcert=verify-full
cat /dev/fd/3 3<< END pg_hba.conf >> tmp
hostssl all all all cert
hostssl replication all all ${auth}
host all all all reject
END

mv tmp pg_hba.conf

cp /certificates/{server,root}.* .
chmod 400 *.key *.crt

cat << END >> postgresql.conf

ssl on
ssl_ca_file = 'root.crt'
ssl_cert_file = 'server.crt' 
ssl_key_file = 'server.key'

END
