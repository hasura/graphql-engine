import { adaptPostgresConnection } from './adaptResponse';

describe('adaptPostgresConnection works for ', () => {
  it('simple pg connection', () => {
    const result = adaptPostgresConnection({
      name: 'chinook',
      kind: 'postgres',
      tables: [],
      configuration: {
        connection_info: {
          database_url:
            'postgres://postgres:test@host.docker.internal:6001/chinook',
          isolation_level: 'read-committed',
          use_prepared_statements: false,
        },
      },
    });
    expect(result).toMatchInlineSnapshot(`
      {
        "configuration": {
          "connectionInfo": {
            "databaseUrl": {
              "connectionType": "databaseUrl",
              "url": "postgres://postgres:test@host.docker.internal:6001/chinook",
            },
            "isolationLevel": "read-committed",
            "poolSettings": {
              "connectionLifetime": undefined,
              "idleTimeout": undefined,
              "poolTimeout": undefined,
              "retries": undefined,
              "totalMaxConnections": undefined,
            },
            "sslSettings": {
              "sslCert": undefined,
              "sslKey": undefined,
              "sslMode": undefined,
              "sslPassword": undefined,
              "sslRootCert": undefined,
            },
            "usePreparedStatements": false,
          },
          "extensionSchema": undefined,
          "readReplicas": [],
        },
        "customization": {
          "namingConvention": undefined,
          "rootFields": {
            "namespace": undefined,
            "prefix": undefined,
            "suffix": undefined,
          },
          "typeNames": {
            "prefix": undefined,
            "suffix": undefined,
          },
        },
        "name": "chinook",
      }
    `);
  });
  it('pg connection with ssl settings (partially filled)', () => {
    const result = adaptPostgresConnection({
      name: 'pg_db_with_partial_ssl_settings',
      kind: 'postgres',
      tables: [],
      configuration: {
        connection_info: {
          database_url: {
            from_env: 'MY_DB_URL',
          },
          isolation_level: 'read-committed',
          ssl_configuration: {
            sslcert: {
              from_env: 'SSL_CERT_GCP',
            },
            sslkey: {
              from_env: 'SSL_KEY_GCP',
            },
            sslmode: 'verify-ca',
            sslrootcert: {
              from_env: 'SSL_ROOTCERT_GCP',
            },
          },
          use_prepared_statements: false,
        },
      },
    });
    expect(result).toMatchInlineSnapshot(`
      {
        "configuration": {
          "connectionInfo": {
            "databaseUrl": {
              "connectionType": "envVar",
              "envVar": "MY_DB_URL",
            },
            "isolationLevel": "read-committed",
            "poolSettings": {
              "connectionLifetime": undefined,
              "idleTimeout": undefined,
              "poolTimeout": undefined,
              "retries": undefined,
              "totalMaxConnections": undefined,
            },
            "sslSettings": {
              "sslCert": "SSL_CERT_GCP",
              "sslKey": "SSL_KEY_GCP",
              "sslMode": "verify-ca",
              "sslPassword": undefined,
              "sslRootCert": "SSL_ROOTCERT_GCP",
            },
            "usePreparedStatements": false,
          },
          "extensionSchema": undefined,
          "readReplicas": [],
        },
        "customization": {
          "namingConvention": undefined,
          "rootFields": {
            "namespace": undefined,
            "prefix": undefined,
            "suffix": undefined,
          },
          "typeNames": {
            "prefix": undefined,
            "suffix": undefined,
          },
        },
        "name": "pg_db_with_partial_ssl_settings",
      }
    `);
  });
});
