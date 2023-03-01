import {
  IsolationLevelOptions,
  MetadataDataSource,
} from '../../../../../metadata/types';
import { Driver } from '../../../../../dataSources/index';
import {
  AddSourceArg,
  dataSourceIsEqual,
  getReadReplicaDBUrlInfo,
} from '../utils';

describe('dataSourceIsEqual works', () => {
  it('works for postgres source', () => {
    const metadataSource = {
      name: 'custom_pg',
      kind: 'postgres' as Driver,
      tables: [],
      configuration: {
        connection_info: {
          use_prepared_statements: false,
          database_url:
            'postgresql://postgres:postgres@host.docker.internal:5432/postgres',
          isolation_level: 'read-committed' as IsolationLevelOptions,
          pool_settings: {
            connection_lifetime: 600,
          },
          ssl_configuration: {},
        },
      },
    };
    const request: AddSourceArg = {
      name: 'custom_pg',
      configuration: {
        connection_info: {
          database_url:
            'postgresql://postgres:postgres@host.docker.internal:5432/postgres',
          pool_settings: {
            connection_lifetime: 600,
          },
          isolation_level: 'read-committed' as IsolationLevelOptions,
          use_prepared_statements: false,
          ssl_configuration: {},
        },
        extensions_schema: null,
        read_replicas: null,
      },
      replace_configuration: true,
    };
    expect(dataSourceIsEqual(metadataSource, request)).toEqual(true);
  });
  it('works for BigQuery source', () => {
    const metadataSource = {
      name: 'BigQuery',
      kind: 'bigquery' as Driver,
      tables: [],
      configuration: {
        service_account: {
          project_id: 'project_id',
          client_email: 'email@bigquery_account.com',
          private_key: '-----BEGIN PRIVATE KEY--',
        },
        project_id: 'project_id',
        datasets: ['dataset1'],
        global_select_limit: 1000,
      },
    };
    const request = {
      name: 'BigQuery',
      configuration: {
        service_account: {
          project_id: 'project_id',
          private_key: '-----BEGIN PRIVATE KEY--',
          client_email: 'email@bigquery_account.com',
        },
        project_id: 'project_id',
        global_select_limit: 1000,
        datasets: ['dataset1'],
      },
      replace_configuration: false,
    };

    expect(dataSourceIsEqual(metadataSource, request)).toEqual(true);
  });
  it('works for MSSQL', () => {
    const request = {
      name: 'MsSQL',
      configuration: {
        connection_info: {
          connection_string:
            'DRIVER={ODBC Driver 18 for SQL Server};SERVER=172.17.0.1;DATABASE=master;Uid=SA;Pwd=reallyStrongPwd123;Encrypt=optional',
          pool_settings: {},
        },
        read_replicas: [
          {
            connection_string:
              'DRIVER={ODBC Driver 18 for SQL Server};SERVER=172.16.238.1,1502;Database=agtestdb;Uid=sa;Pwd=Password1;ApplicationIntent=ReadOnly;Encrypt=optional',
            pool_settings: { idle_timeout: 5, max_connections: 50 },
          },
        ],
      },
      replace_configuration: false,
    };
    const metadataSource: MetadataDataSource = {
      name: 'MsSQL',
      kind: 'mssql' as Driver,
      tables: [],
      configuration: {
        connection_info: {
          connection_string:
            'DRIVER={ODBC Driver 18 for SQL Server};SERVER=172.17.0.1;DATABASE=master;Uid=SA;Pwd=reallyStrongPwd123;Encrypt=optional',
          pool_settings: {
            idle_timeout: 5,
            max_connections: 50,
          },
        },
      },
    };
    expect(dataSourceIsEqual(metadataSource, request)).toEqual(false);
  });
});

describe('getReadReplicaDBUrlInfo gives the correct result', () => {
  it('for postgres read replicas with db urls', () => {
    const res = getReadReplicaDBUrlInfo(
      {
        use_prepared_statements: false,
        database_url: 'postgres://postgres:test@172.17.0.1:6001/chinook',
        isolation_level: 'read-committed',
        pool_settings: {
          connection_lifetime: 600,
        },
      },
      'postgres'
    );
    expect(res).toMatchInlineSnapshot(`
      {
        "connectionType": "DATABASE_URL",
        "databaseURLState": {
          "datasets": "",
          "dbURL": "postgres://postgres:test@172.17.0.1:6001/chinook",
          "global_select_limit": 1000,
          "projectId": "",
          "serviceAccount": "",
        },
      }
    `);
  });
  it('for mssql read replicas with db urls', () => {
    const res = getReadReplicaDBUrlInfo(
      {
        use_prepared_statements: false,
        connection_string: 'postgres://postgres:test@172.17.0.1:6001/chinook',
        isolation_level: 'read-committed',
        pool_settings: {
          connection_lifetime: 600,
        },
      },
      'mssql'
    );
    expect(res).toMatchInlineSnapshot(`
      {
        "connectionType": "DATABASE_URL",
        "databaseURLState": {
          "datasets": "",
          "dbURL": "postgres://postgres:test@172.17.0.1:6001/chinook",
          "global_select_limit": 1000,
          "projectId": "",
          "serviceAccount": "",
        },
      }
    `);
  });
  it('for postgres read replicas with env vars', () => {
    const res = getReadReplicaDBUrlInfo(
      {
        use_prepared_statements: false,
        database_url: {
          from_env: 'HASURA_GRAPHQL_DATABASE_URL',
        },
        isolation_level: 'read-committed',
        pool_settings: {
          connection_lifetime: 600,
        },
      },
      'postgres'
    );
    expect(res).toMatchInlineSnapshot(`
      {
        "connectionType": "ENVIRONMENT_VARIABLES",
        "envVarState": {
          "envVar": "HASURA_GRAPHQL_DATABASE_URL",
        },
      }
    `);
  });
  it('for mssql read replicas with env vars', () => {
    const res = getReadReplicaDBUrlInfo(
      {
        use_prepared_statements: false,
        connection_string: {
          from_env: 'HASURA_GRAPHQL_DATABASE_URL',
        },
        isolation_level: 'read-committed',
        pool_settings: {
          connection_lifetime: 600,
        },
      },
      'mssql'
    );
    expect(res).toMatchInlineSnapshot(`
      {
        "connectionType": "ENVIRONMENT_VARIABLES",
        "envVarState": {
          "envVar": "HASURA_GRAPHQL_DATABASE_URL",
        },
      }
    `);
  });
});
