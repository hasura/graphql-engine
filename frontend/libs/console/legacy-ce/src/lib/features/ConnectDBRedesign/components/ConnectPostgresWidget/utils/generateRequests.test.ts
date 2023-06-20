import { generatePostgresRequestPayload } from './generateRequests';

describe('[generatePostgresRequestPayload] generates the correct payload when', () => {
  it('only the connection info is filled in (basic case - connection params)', () => {
    const result = generatePostgresRequestPayload({
      driver: 'postgres',
      values: {
        name: 'chinook',
        configuration: {
          connectionInfo: {
            databaseUrl: {
              connectionType: 'connectionParams',
              username: 'myusername',
              password: 'password123',
              database: 'chinook',
              host: 'localhost',
              port: 5432,
            },
          },
        },
      },
    });
    expect(result).toMatchInlineSnapshot(`
      {
        "details": {
          "configuration": {
            "connection_info": {
              "database_url": {
                "connection_parameters": {
                  "database": "chinook",
                  "host": "localhost",
                  "password": "password123",
                  "port": 5432,
                  "username": "myusername",
                },
              },
            },
          },
          "name": "chinook",
        },
        "driver": "postgres",
      }
    `);
  });
  it('only the connection info is filled in (basic case - database url)', () => {
    const result = generatePostgresRequestPayload({
      driver: 'postgres',
      values: {
        name: 'chinook',
        configuration: {
          connectionInfo: {
            databaseUrl: {
              connectionType: 'databaseUrl',
              url: 'postgresql://myusername:password123@localhost:5432/chinook',
            },
          },
        },
      },
    });
    expect(result).toMatchInlineSnapshot(`
      {
        "details": {
          "configuration": {
            "connection_info": {
              "database_url": "postgresql://myusername:password123@localhost:5432/chinook",
            },
          },
          "name": "chinook",
        },
        "driver": "postgres",
      }
    `);
  });
  it('only the connection info is filled in (basic case - env var)', () => {
    const result = generatePostgresRequestPayload({
      driver: 'postgres',
      values: {
        name: 'chinook',
        configuration: {
          connectionInfo: {
            databaseUrl: {
              connectionType: 'envVar',
              envVar: 'MY_SECRET_ENV_VAR',
            },
          },
        },
      },
    });
    expect(result).toMatchInlineSnapshot(`
      {
        "details": {
          "configuration": {
            "connection_info": {
              "database_url": {
                "from_env": "MY_SECRET_ENV_VAR",
              },
            },
          },
          "name": "chinook",
        },
        "driver": "postgres",
      }
    `);
  });
  it('connection info and pool settings are filled in', () => {
    const result = generatePostgresRequestPayload({
      driver: 'postgres',
      values: {
        name: 'chinook',
        configuration: {
          connectionInfo: {
            databaseUrl: {
              connectionType: 'connectionParams',
              username: 'myusername',
              password: 'password123',
              database: 'chinook',
              host: 'localhost',
              port: 5432,
            },
            poolSettings: {
              totalMaxConnections: 100,
              idleTimeout: 100,
              retries: 100,
              poolTimeout: 100,
              connectionLifetime: 100100,
            },
            isolationLevel: 'read-committed',
          },
          extensionSchema: 'public_schema',
        },
      },
    });
    expect(result).toMatchInlineSnapshot(`
      {
        "details": {
          "configuration": {
            "connection_info": {
              "database_url": {
                "connection_parameters": {
                  "database": "chinook",
                  "host": "localhost",
                  "password": "password123",
                  "port": 5432,
                  "username": "myusername",
                },
              },
              "isolation_level": "read-committed",
              "pool_settings": {
                "connection_lifetime": 100100,
                "idle_timeout": 100,
                "pool_timeout": 100,
                "retries": 100,
                "total_max_connections": 100,
              },
            },
            "extensions_schema": "public_schema",
          },
          "name": "chinook",
        },
        "driver": "postgres",
      }
    `);
  });
  it('connection info and GQL customization settings are filled in', () => {
    const result = generatePostgresRequestPayload({
      driver: 'postgres',
      values: {
        name: 'chinook',
        configuration: {
          connectionInfo: {
            databaseUrl: {
              connectionType: 'connectionParams',
              username: 'myusername',
              password: 'password123',
              database: 'chinook',
              host: 'localhost',
              port: 5432,
            },
          },
        },
        customization: {
          rootFields: {
            namespace: 'root_field_namespace',
            prefix: 'root_field_prefix',
            suffix: 'root_field_suffix',
          },
          typeNames: {
            prefix: 'type_names_prefix',
            suffix: 'type_names_suffix',
          },
        },
      },
    });
    expect(result).toMatchInlineSnapshot(`
      {
        "details": {
          "configuration": {
            "connection_info": {
              "database_url": {
                "connection_parameters": {
                  "database": "chinook",
                  "host": "localhost",
                  "password": "password123",
                  "port": 5432,
                  "username": "myusername",
                },
              },
            },
          },
          "customization": {
            "root_fields": {
              "namespace": "root_field_namespace",
              "prefix": "root_field_prefix",
              "suffix": "root_field_suffix",
            },
            "type_names": {
              "prefix": "type_names_prefix",
              "suffix": "type_names_suffix",
            },
          },
          "name": "chinook",
        },
        "driver": "postgres",
      }
    `);
  });
  it('all the input fields are filled in', () => {
    const result = generatePostgresRequestPayload({
      driver: 'postgres',
      values: {
        name: 'chinook',
        configuration: {
          connectionInfo: {
            databaseUrl: {
              connectionType: 'connectionParams',
              username: 'myusername',
              password: 'password123',
              database: 'chinook',
              host: 'localhost',
              port: 5432,
            },
            poolSettings: {
              totalMaxConnections: 100,
              idleTimeout: 100,
              retries: 100,
              poolTimeout: 100,
              connectionLifetime: 100100,
            },
            isolationLevel: 'read-committed',
          },
          extensionSchema: 'public_schema',
        },
        customization: {
          rootFields: {
            namespace: 'root_field_namespace',
            prefix: 'root_field_prefix',
            suffix: 'root_field_suffix',
          },
          typeNames: {
            prefix: 'type_names_prefix',
            suffix: 'type_names_suffix',
          },
        },
      },
    });
    expect(result).toMatchInlineSnapshot(`
      {
        "details": {
          "configuration": {
            "connection_info": {
              "database_url": {
                "connection_parameters": {
                  "database": "chinook",
                  "host": "localhost",
                  "password": "password123",
                  "port": 5432,
                  "username": "myusername",
                },
              },
              "isolation_level": "read-committed",
              "pool_settings": {
                "connection_lifetime": 100100,
                "idle_timeout": 100,
                "pool_timeout": 100,
                "retries": 100,
                "total_max_connections": 100,
              },
            },
            "extensions_schema": "public_schema",
          },
          "customization": {
            "root_fields": {
              "namespace": "root_field_namespace",
              "prefix": "root_field_prefix",
              "suffix": "root_field_suffix",
            },
            "type_names": {
              "prefix": "type_names_prefix",
              "suffix": "type_names_suffix",
            },
          },
          "name": "chinook",
        },
        "driver": "postgres",
      }
    `);
  });
});
