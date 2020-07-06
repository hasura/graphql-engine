import { QueryType } from './../integration/validators/validators';
import { ADMIN_SECRET_HEADER_KEY } from '../../src/constants';

export const baseUrl = Cypress.config('baseUrl');

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

export const getElementFromAlias = (alias: string) => `[data-test=${alias}]`;

export const getElementFromClassName = (cn: string) => `.${cn}`;

export const tableColumnTypeSelector = (alias: string) => {
  cy.get(`${getElementFromAlias(alias)}`)
    .children('div')
    .click()
    .find('input')
    .focus();
};

export const makeDataAPIUrl = (dataApiUrl: string) => `${dataApiUrl}/v1/query`;

interface APIPayload {
  [key: string]: any;
}

export const makeDataAPIOptions = (
  dataApiUrl: string,
  key: string,
  body: APIPayload
) => ({
  method: 'POST',
  url: makeDataAPIUrl(dataApiUrl),
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

export const testCustomFunctionSQL = (i: number) => {
  return {
    type: 'bulk',
    args: [
      {
        type: 'run_sql',
        args: {
          sql: `CREATE OR REPLACE FUNCTION public.search_posts_${i}(search text)\n RETURNS SETOF post\n LANGUAGE sql\n STABLE\nAS $function$\n          select *\n          from post\n          where\n          title ilike ('%' || search || '%') or\n          content ilike ('%' || search || '%')\n      $function$\n`,
          cascade: false,
        },
      },
      {
        type: 'track_function',
        args: {
          name: `search_posts_${i}`,
          schema: 'public',
        },
      },
    ],
  };
};

export const testCustomFunctionSQLWithSessArg = (
  name = 'customFunctionWithSessionArg'
) => {
  return {
    type: 'bulk',
    args: [
      {
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
      },
    ],
  };
};
export const getTrackFnPayload = (name = 'customfunctionwithsessionarg') => ({
  type: 'bulk',
  args: [
    {
      type: 'track_function',
      args: {
        name,
        schema: 'public',
      },
    },
  ],
});
export const createTable = () => {
  return {
    type: 'bulk',
    args: [
      {
        type: 'run_sql',
        args: {
          sql:
            'create table post (\n        id serial PRIMARY KEY,\n        title TEXT,\n        content TEXT\n )',
          cascade: false,
        },
      },
      {
        type: 'add_existing_table_or_view',
        args: {
          name: 'post',
          schema: 'public',
        },
      },
    ],
  };
};
export const createTableSessVar = () => {
  return {
    type: 'bulk',
    args: [
      {
        type: 'run_sql',
        args: {
          sql: `CREATE TABLE text_result(
              result text
            );`,
          cascade: false,
        },
      },
      {
        type: 'add_existing_table_or_view',
        args: {
          name: 'text_result',
          schema: 'public',
        },
      },
    ],
  };
};
export const dropTable = (table = 'post', cascade = false) => {
  return {
    type: 'bulk',
    args: [
      {
        type: 'run_sql',
        args: {
          sql: `DROP table ${table}${cascade ? ' CASCADE;' : ';'}`,
          cascade,
        },
      },
    ],
  };
};

export const getSchema = () => 'public';
