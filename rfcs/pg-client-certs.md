This document outlines our support for Postgres client certificates in
multitenant.

# API
<https://github.com/hasura/graphql-engine-mono/issues/1143>

We want to use Env Vars to correlate certificate data to postgres
sources.  We want to allow users to add sources with client certs via
the metadata api:

```json
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
```

Cert Data is stored on disk in a path provided by the flag
`--tenantCertPath`. If this flag is not provided it will be stored in a unique generated folder.

# Implementation

The `PostgresSourceConnInfo` type has been extended to include an
optional set of `PGClientCerts` which itself contains the Env Vars
provided to the Metadata API:

```
data PostgresSourceConnInfo
  = PostgresSourceConnInfo
  { _psciDatabaseUrl           :: !UrlConf
  , _psciPoolSettings          :: !(Maybe PostgresPoolSettings)
  , _psciUsePreparedStatements :: !Bool
  , _psciIsolationLevel        :: !Q.TxIsolation
  , _psciSslConfiguration      :: !(Maybe (PGClientCerts CertVar CertVar))
```

When resolving a `PostgresConnConfiguration` in `getSourceResolver` we
check for the existence of `PGClientCerts`. If it is present, the env
vars are then queried in the environment and the cert data they
contain is written to disk at the `tenantCertPath` in a per tenant
sub-folder.

The filepaths are then merged into the postgres connection string as
query parameters. The module `HasuraPro.PGClientCerts` encapsulates
all the cert merging and writing operations.

Cleanup of cert data is handled in the multitenant lifecycle
loop. When a tenant is taken down or updated, the entire tenant
subfolder of `tenantCertPath` is deleted. This ensures that cert data
does not linger longer then necessary and that during updates we dont
end up with stale cert data.
