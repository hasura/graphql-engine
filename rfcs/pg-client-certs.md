This document outlines the current implementation of postgres client
certs on Main and the changes we plan on making to it.

# Cert Storage

Client Cert data is currently stored in Vault with the Tenant Config:

    {
      "adminSecret": "my_secret",
      "certificates": {
        "test_db": {
          "backend": "postgres",
          "client-key-password": "my_password", (OPTIONAL)
          "client-cert": "LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS0tLS0tCk1JSURpekNDQW5PZ0F3SU...",
          "client-key": "LS0tLS1CRUdJTiBSU0EgUFJJVkFURSBLRVktLS0tLQpNSUlFcFFJQkFBS0NB...",
          "server-ca": "LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS0tLS0tCk1JSURmekNDQW1lZ0F3SU..."
        }
      }
      ...
    }

Cert sets are key’d by the connection’s Source name. This name can be
changed via the console and makes this implementation rather
brittle. We would have to ensure that Vault is updated any time the
Source name is changed in Console.

# Cert Injection

[This](https://github.com/hasura/graphql-engine-mono/blob/main/pro/server/multitenant/Main.hs#L380-L405) `forever` loop is known as the multitenant lifecycle loop. It
handles the setup and teardown of tenant instances. We poll for config
data from the Controller and pass the old and new configs into the
[applyConfig subroutine](https://github.com/hasura/graphql-engine-mono/blob/main/pro/server/multitenant/Main.hs#L412) where we align on the configs to determine if
we are creating, updating, or deleting a tenant.

When creating or updating a tenant, we fetch the tenant config and the
client certs from Vault. We immediately write the certs to disk and
then hold onto the filepaths for injecting into the Graphql Engine
instance.

Per Tenant, cert paths are stored as a `Map Text (Map Text Text)`. The
outer map is from source name to cert map, and the inner map is
cert-type (server-ca, client-key, client-cert) to cert-filepath.

After writing the cert data to disk and generating the cert path map,
we spawn the pro server instance with the cert path map in the
`ProServeCtx`. The construction of the final connection strings occurs
in [getSourceResolver](https://github.com/hasura/graphql-engine-mono/blob/main/pro/server/src/HasuraPro/App.hs#L702).

First we [check for cert map](https://github.com/hasura/graphql-engine-mono/blob/main/pro/server/src/HasuraPro/App.hs#L725) data for the given postgres source. Then
[if there is no cert data](https://github.com/hasura/graphql-engine-mono/blob/main/pro/server/src/HasuraPro/App.hs#L756-L759) for the source we return the base url from
the `PostgresSourceConnInfo`. Otherwise we [attempt to parse the URI
and concat on the cert query params](https://github.com/hasura/graphql-engine-mono/blob/main/pro/server/src/HasuraPro/App.hs#L747-L754). If the parse fails then there was
either a bad connection string or it used the key-val format. In
either case we log the parse failure and return the unmodified
connection string. The actual parsing and concatenation happens in
[addCertsToURI](https://github.com/hasura/graphql-engine-mono/blob/main/pro/server/src/HasuraPro/PostgresCerts.hs#L97). We use `Network.URI.parseURI` to do the parsing.

# Cert Cleanup

We delete certificate data from disk in two cases:

1.  In `applyConfig` we delete all certificate data from disk when a Tenant is taken down.
2.  When updating a Tenant we also delete all certificate data before
    fetching new data from Vault. It would be preferable to just
    erase the delta.

# Proposed new behavior

<https://github.com/hasura/graphql-engine-mono/issues/1143>

We want to use Env Vars to correlate certificate data to postgres sources rather than using Source Name.
We want to allow users to add sources with client certs via the metadata api:

    {
      "type": "pg_add_source",
      "args": {
        "name": "pg1",
        "configuration": {
          "connection_info": {
            "database_url": {
               "from_env": "<DB_URL_ENV_VAR>"
             },
            "ssl_configuration": {
              "sslmode": "verify-ca",
              "sslrootcert": {
                "from_env": "<SSL_ROOT_CERT_ENV_VAR>"
              },
              "sslcert": {
                "from_env": "<SSL_CERT_ENV_VAR>"
              },
              "sslkey": {
                "from_env": "<SSL_KEY_ENV_VAR>"
              },
              "sslpassword": {
                "from_env": "<SSL_PASSWORD_ENV_VAR>"
              }
            }
          }
        }
      }
    }

Connection Strings with client certificates will be constructed by the
Metadata API using URL Templating. Eg.,

`postgres://postgres:password@localhost?sslmode={{SSL_MODE}}&sslkey={{CLIENT_KEY}&sslcert={{CLIENT_CERT}}&sslrootcert={{SERVER_CA}}`

# Implementation plan for new behavior

We don’t have access to metadata until we launch the Engine
instance. So we will defer writing cert files to disk until we are
generating the connection pool.

In [getSourceResolver](https://github.com/hasura/graphql-engine-mono/blob/main/pro/server/src/HasuraPro/App.hs#L702) we will fetch the metadata and lookup all cert
env vars and use a hash of the cert data to check for the file on
disk. Cert files will be stored under a tenant specific file path such
as `~/.postgres/<tenant-id>/<cert-hash>`. Any certs not present will
be written to disk at this point. We can then construct a `Map EnvVar
FilePath` to use with [renderURLTemplate](https://github.com/hasura/graphql-engine-mono/blob/feature%2Fclient-cert-env/server/src-lib/Data/URL/Template.hs#L71) to perform the connection
string interpolation.

We will handle cleanup in the multitenant lifecycle loop by deleting
the tenant cert folder.
