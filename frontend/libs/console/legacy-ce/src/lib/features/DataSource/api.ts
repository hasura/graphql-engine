import { AxiosInstance, AxiosResponseHeaders } from 'axios';
import {
  NativeDrivers,
  Source,
  SupportedDrivers,
} from '../hasura-metadata-types';
import { isPostgres } from '../../metadata/dataSource.utils';
export { runMetadataQuery, exportMetadata } from '../hasura-metadata-api';
export interface NetworkArgs {
  httpClient: AxiosInstance;
}

type RunSqlArgs = {
  source: Pick<Source, 'kind' | 'name'>;
  sql: string;
  readOnly?: boolean;
};

export type RunSQLSelectResponse = {
  result_type: 'TuplesOk';
  result: string[][];
};

export type RunSQLCommandResponse = {
  result_type: 'CommandOk';
  result: null;
};

export type RunSQLResponse = RunSQLSelectResponse | RunSQLCommandResponse;

const getRunSqlType = (driver: NativeDrivers) => {
  if (isPostgres(driver)) {
    return 'run_sql';
  }

  return `${driver}_run_sql`;
};

// We need to type this better
export type RunSQLAPIError = Record<string, any>;

export const runQuery = async <ResponseType>({
  body,
  httpClient,
}: { body: Record<string, any> } & NetworkArgs) => {
  /**
   * Use v2 query instead of v1 because it supports other <db>_run_sql commands
   */
  const result = await httpClient.post<ResponseType, RunSQLAPIError>(
    'v2/query',
    body
  );
  return result.data;
};

export const runGraphQL = async ({
  operationName,
  query,
  httpClient,
  headers,
}: {
  operationName: string;
  query: string;
  headers?: AxiosResponseHeaders;
} & NetworkArgs) => {
  try {
    const result = await httpClient.post('v1/graphql', {
      query,
      operationName,
      headers,
    });
    // Throw the first GraphQL Error
    // We do this because response.status is 200 even if there are errors
    if (result.data.errors?.length) {
      throw new Error(result.data.errors[0].message || 'Unexpected');
    }
    return result.data;
  } catch (err) {
    throw err;
  }
};

export const runSQL = async ({
  source,
  sql,
  readOnly,
  httpClient,
}: RunSqlArgs & NetworkArgs): Promise<RunSQLResponse> => {
  const type = getRunSqlType(source.kind as NativeDrivers);

  const readOnlyArg =
    readOnly === null || readOnly === undefined ? {} : { read_only: readOnly };

  const result = await runQuery<RunSQLResponse>({
    httpClient,
    body: {
      type,
      args: {
        sql,
        source: source.name,
        ...readOnlyArg,
      },
    },
  });
  return result;
};

export const runIntrospectionQuery = async ({ httpClient }: NetworkArgs) => {
  return runGraphQL({
    operationName: 'IntrospectionQuery',
    query: `query IntrospectionQuery {
      __schema {
        queryType {
          name
        }
        mutationType {
          name
        }
        subscriptionType {
          name
        }
        types {
          ...FullType
        }
        directives {
          name
          description
          locations
          args {
            ...InputValue
          }
        }
      }
    }
    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }
    fragment InputValue on __InputValue {
      name
      description
      type {
        ...TypeRef
      }
      defaultValue
    }
    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        }
      }
    }`,
    httpClient,
  });
};

export const runReadOnlySQLBulk = async ({
  source,
  sqlQueries,
  httpClient,
}: {
  source: Pick<Source, 'kind' | 'name'>;
  sqlQueries: string[];
} & NetworkArgs): Promise<RunSQLResponse[]> => {
  const type = getRunSqlType(source.kind as NativeDrivers);

  const result = await runQuery<RunSQLResponse>({
    httpClient,
    body: {
      type: 'concurrent_bulk',
      source: source.name,
      args: sqlQueries.map(sql => ({
        type,
        args: {
          sql,
          source: source.name,
        },
      })),
    },
  });
  return result;
};

export const getDriverPrefix = (driver: SupportedDrivers) =>
  isPostgres(driver as NativeDrivers) ? 'pg' : driver;
