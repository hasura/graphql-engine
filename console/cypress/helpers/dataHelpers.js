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
export const typeDefaults = {
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
export const queryTypes = ['insert', 'select', 'update', 'delete'];
export const getColName = i => `Apic_test_column_${i}`;
export const getTableName = (i, testName = '') =>
  `Apic_test_table_${testName}_${i}`;
export const getElementFromAlias = alias => `[data-test=${alias}]`;
export const getElementFromClassName = cn => `.${cn}`;
export const tableColumnTypeSelector = alias => {
  cy.get(`${getElementFromAlias(alias)}`)
    .children('div')
    .click()
    .find('input')
    .focus();
};
export const makeDataAPIUrl = dataApiUrl => `${dataApiUrl}/v1/query`;
export const makeDataAPIOptions = (dataApiUrl, key, body) => ({
  method: 'POST',
  url: makeDataAPIUrl(dataApiUrl),
  headers: {
    'x-hasura-admin-secret': key,
  },
  body,
  failOnStatusCode: false,
});

export const testCustomFunctionDefinition = i => `create function search_posts${'_' +
  i} (search text) returns setof post as $$ select * from post where title ilike ('%' || search || '%') or content ilike ('%' || search || '%') $$ language sql stable;
`;

export const getCustomFunctionName = i => `search_posts${'_' + i}`;

export const testCustomFunctionSQL = i => {
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

export const dropTable = () => {
  return {
    type: 'bulk',
    args: [
      {
        type: 'run_sql',
        args: {
          sql: 'DROP table post;',
          cascade: false,
        },
      },
    ],
  };
};

export const getSchema = () => 'public';
