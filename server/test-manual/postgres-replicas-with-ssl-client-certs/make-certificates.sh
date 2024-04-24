#!/usr/bin/env bash

set -x
set -e
set -u

rm -r certificates && true
mkdir -p certificates

hostname_cn='localhost'

# To support CA verification when connecting to the database servers via a
# public tunnel, adjust the 'hostname' variable accordingly.
#
# A working example with ngrok tunnels in the eu region is:
#
#   hostname_cn='*.tcp.eu.ngrok.io'

pushd certificates

# AS per PG docs:

# To create a server certificate whose identity can be validated by clients,
# first create a certificate signing request (CSR) and a public/private key
# file:

openssl req -new -nodes -text -out root.csr \
  -keyout root.key -subj "/CN=my-own-ca"

# Then, sign the request with the key to create a root certificate authority
# (using the default OpenSSL configuration file location on Linux):

openssl x509 -req -in root.csr -text -days 3650 \
  -extfile ../openssl.cnf -extensions v3_ca \
  -signkey root.key -out root.crt

# Finally, create a server certificate signed by the new root certificate
# authority:

openssl req -new -nodes -text -out server.csr \
  -keyout server.key -subj "/CN=$hostname_cn"


openssl x509 -req -in server.csr -text -days 365 \
  -CA root.crt -CAkey root.key -CAcreateserial \
  -out server.crt


# Also, client certificate:

openssl req -new -nodes -text -out client.csr \
  -keyout client.key -subj "/CN=postgres" # <-- This "postgres" refers to the database user name.



openssl x509 -req -in client.csr -text -days 365 \
  -CA root.crt -CAkey root.key -CAcreateserial \
  -out client.crt

popd

# Permissions are different in docker containers, so they are managed by init scripts instead.
chmod 777 -R certificates/{root,server}*
