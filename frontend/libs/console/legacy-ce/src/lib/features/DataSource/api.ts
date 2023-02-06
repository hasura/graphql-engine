import { AxiosInstance, AxiosResponseHeaders } from 'axios';
import {
  Metadata,
  NativeDrivers,
  Source,
  SupportedDrivers,
} from '@/features/hasura-metadata-types';
import { isPostgres } from '@/metadata/dataSource.utils';

export interface NetworkArgs {
  httpClient: AxiosInstance;
}

export const exportMetadata = async ({
  httpClient,
}: NetworkArgs): Promise<Metadata> => {
  return (
    await httpClient.post('/v1/metadata', {
      type: 'export_metadata',
      version: 2,
      args: {},
    })
  ).data;
};

export const runMetadataQuery = async <ResponseType>({
  httpClient,
  body,
}: { body: Record<string, any> } & NetworkArgs): Promise<ResponseType> => {
  return (await httpClient.post('/v1/metadata', body)).data;
};

type RunSqlArgs = {
  source: Pick<Source, 'kind' | 'name'>;
  sql: string;
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

export const runQuery = async <ResponseType>({
  body,
  httpClient,
}: { body: Record<string, any> } & NetworkArgs) => {
  /**
   * Use v2 query instead of v1 because it supports other <db>_run_sql commands
   */
  const result = await httpClient.post<ResponseType>('v2/query', body);
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
  httpClient,
}: RunSqlArgs & NetworkArgs): Promise<RunSQLResponse> => {
  const type = getRunSqlType(source.kind as NativeDrivers);
  const result = await runQuery<RunSQLResponse>({
    httpClient,
    body: {
      type,
      args: {
        sql,
        source: source.name,
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

export const getDriverPrefix = (driver: SupportedDrivers) =>
  isPostgres(driver as NativeDrivers) ? 'pg' : driver;
