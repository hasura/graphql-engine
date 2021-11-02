import { QueryType } from './../integration/validators/validators';
import { ADMIN_SECRET_HEADER_KEY } from './constants';

export const baseUrl = Cypress.config('baseUrl');

export const getDbRoute = (sourceName = 'default') => `/data/${sourceName}`;

export const getIndexRoute = (sourceName = 'default', schemaName = 'public') =>
  `${getDbRoute(sourceName)}/schema/${schemaName}/`;

export const createVolatileFunction = (name: string) => {
  return {
    type: 'run_sql',
    args: {
      sql: `CREATE OR REPLACE  FUNCTION public.${name}()
            RETURNS SETOF text_result
            LANGUAGE sql
            AS $function$
              SELECT * FROM text_result;
            $function$`,
      cascade: false,
    },
  };
};

export const dataTypes = [
  'serial',
  'bigserial',
  'integer',
  'bigint',
  'uuid',
  'text',
  'numeric',
  'date',
  'timestamptz',
  'timetz',
  'boolean',
];

export const typeDefaults: { [key: string]: string } = {
  integer: '5555',
  bigint: '5555555555',
  uuid: 'gen_random_uuid()',
  text: 'test-text',
  numeric: '0.55555',
  date: 'now()',
  timestamptz: 'now()',
  timetz: 'now()',
  boolean: 'false',
};

export const queryTypes: QueryType[] = ['insert', 'select', 'update', 'delete'];

export const getColName = (i: number) => `Apic_test_column_${i}`;

export const getTableName = (i: number, testName = '') =>
  `Apic_test_table_${testName}_${i}`;

export const getElementFromAlias = (alias: string) => `[data-test="${alias}"]`;

export const getElementFromClassName = (cn: string) => `.${cn}`;

export const tableColumnTypeSelector = (alias: string) => {
  cy.get(`${getElementFromAlias(alias)}`)
    .children('div')
    .click()
    .find('input')
    .focus();
};

export const makeDataAPIUrl = (
  dataApiUrl: string,
  queryEndpoint: QueryEndpoint = 'query'
) => {
  if (queryEndpoint === 'query') {
    return `${dataApiUrl}/v2/query`;
  }
  return `${dataApiUrl}/v1/metadata`;
};

interface APIPayload {
  [key: string]: any;
}

export const queryEndpoints = {
  query: 'query',
  metadata: 'metadata',
} as const;

export type QueryEndpoint = keyof typeof queryEndpoints;

export const makeDataAPIOptions = (
  dataApiUrl: string,
  key: string,
  body: APIPayload,
  queryType: QueryEndpoint = 'query'
) => ({
  method: 'POST',
  url: makeDataAPIUrl(dataApiUrl, queryType),
  headers: {
    [ADMIN_SECRET_HEADER_KEY]: key,
  },
  body,
  failOnStatusCode: false,
});

export const testCustomFunctionDefinition = (
  i: string
) => `create function search_posts${`_${i}`} (search text) returns setof post as $$ select * from post where title ilike ('%' || search || '%') or content ilike ('%' || search || '%') $$ language sql stable;
`;

export const getCustomFunctionName = (i: number) => `search_posts${`_${i}`}`;

export const getCreateTestFunctionQuery = (i: number) => {
  return {
    type: 'run_sql',
    args: {
      sql: `CREATE OR REPLACE FUNCTION public.search_posts_${i}(search text)\n RETURNS SETOF post\n LANGUAGE sql\n STABLE\nAS $function$\n          select *\n          from post\n          where\n          title ilike ('%' || search || '%') or\n          content ilike ('%' || search || '%')\n      $function$\n`,
      cascade: false,
    },
  };
};

export const getTrackTestFunctionQuery = (i: number) => {
  return {
    type: 'pg_track_function',
    args: {
      function: `search_posts_${i}`,
      schema: 'public',
      source: 'default',
    },
  };
};

export const testCustomFunctionSQLWithSessArg = (
  name = 'customFunctionWithSessionArg'
) => {
  return {
    type: 'run_sql',
    args: {
      sql: `CREATE OR REPLACE FUNCTION ${name}(
            hasura_session json, name text
          ) RETURNS SETOF text_result LANGUAGE sql STABLE AS $$
          SELECT
            q.*
          FROM
            (
              VALUES
                (hasura_session ->> 'x-hasura-role')
            ) q $$`,
      cascade: false,
    },
  };
};

export const createUntrackedFunctionSQL = (
  fnName: string,
  tableName: string
) => {
  return {
    type: 'run_sql',
    args: {
      sql: `
          CREATE OR REPLACE FUNCTION ${fnName}(table_row "${tableName}")
           RETURNS int
           LANGUAGE sql
           STABLE
          AS $function$
            SELECT table_row.id
          $function$
          `,
      cascade: false,
    },
  };
};

export const dropUntrackedFunctionSQL = (fnName: string) => {
  return {
    type: 'run_sql',
    args: {
      sql: `
          DROP FUNCTION public.${fnName};
          `,
      cascade: false,
    },
  };
};

export const getTrackFnPayload = (name = 'customfunctionwithsessionarg') => ({
  type: 'pg_track_function',
  args: {
    function: name,
    source: 'default',
    schema: 'public',
  },
});

// has to go to query
export const createFunctionTable = () => {
  return {
    type: 'run_sql',
    args: {
      sql: 'create table post (id serial PRIMARY KEY,title TEXT,content TEXT);',
      cascade: false,
    },
  };
};
// has to go to metadata
export const trackCreateFunctionTable = () => {
  return {
    type: 'pg_track_table',
    args: {
      table: {
        name: 'post',
        schema: 'public',
      },
    },
  };
};

export const createSampleTable = () => ({
  type: 'run_sql',
  source: 'default',
  args: {
    sql: 'CREATE TABLE text_result(result text);',
    cascade: false,
  },
});

export const dropTableIfExists = (
  table: { name: string; schema: string },
  source = 'default'
) => ({
  type: 'run_sql',
  source,
  args: {
    sql: `DROP TABLE IF EXISTS "${table.schema}"."${table.name}";`,
    cascade: false,
  },
});

export const getTrackSampleTableQuery = () => {
  return {
    type: 'pg_track_table',
    source: 'default',
    args: {
      table: {
        name: 'text_result',
        schema: 'public',
      },
    },
  };
};
export const dropTable = (table = 'post', cascade = false) => {
  return {
    type: 'run_sql',
    args: {
      sql: `DROP table "public"."${table}"${cascade ? ' CASCADE;' : ';'}`,
      cascade,
    },
  };
};

export const getSchema = () => 'public';
