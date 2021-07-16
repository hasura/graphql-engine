import {
  IsolationLevelOptions,
  MetadataDataSource,
} from '../../../../../metadata/types';
import { Driver } from '../../../../../dataSources/index';
import { AddSourceArg, dataSourceIsEqual } from '../utils';

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
            'DRIVER={ODBC Driver 17 for SQL Server};SERVER=172.17.0.1;DATABASE=master;Uid=SA;Pwd=reallyStrongPwd123',
          pool_settings: {},
        },
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
            'DRIVER={ODBC Driver 17 for SQL Server};SERVER=172.17.0.1;DATABASE=master;Uid=SA;Pwd=reallyStrongPwd123',
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
