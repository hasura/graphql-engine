import { setupServer } from 'msw/node';
import { generateGqlQueryFromTable } from './utils';
import { handlers } from '../../../features/Permissions/PermissionsForm/mocks/handlers.mock';
import { Table } from '../../../dataSources/types';

const table: Table = {
  table_name: 'test',
  table_schema: 'public',
  table_type: 'TABLE',
  columns: [
    {
      column_default: "nextval('test_id_seq'::regclass)",
      column_name: 'id',
      comment: null,
      data_type: 'integer',
      data_type_name: 'int4',
      identity_generation: null,
      is_generated: false,
      is_identity: false,
      is_nullable: 'NO',
      ordinal_position: 1,
      table_name: 'test',
      table_schema: 'public',
    },
    {
      column_default: null,
      column_name: 'name',
      comment: null,
      data_type: 'text',
      data_type_name: 'text',
      identity_generation: null,
      is_generated: false,
      is_identity: false,
      is_nullable: 'YES',
      ordinal_position: 2,
      table_name: 'test',
      table_schema: 'public',
    },
  ],
  comment: null,
  primary_key: {
    columns: ['id'],
    constraint_name: 'test_pkey',
    table_name: 'test',
    table_schema: 'public',
  },
  is_table_tracked: true,
  relationships: [],
  remote_relationships: [],
  view_info: null,
  unique_constraints: [],
  permissions: [],
  opp_foreign_key_constraints: [],
  foreign_key_constraints: [],
  check_constraints: [],
  computed_fields: [],
  is_enum: false,
};

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('generateGqlQueryFromTable', () => {
  beforeEach(() => {
    server.use(...handlers());
  });

  test('When generateGqlQueryFromTable is used with a query and table Then it should genrate a operation', async () => {
    const operationType = 'query';
    const { query, variables } = generateGqlQueryFromTable(
      operationType,
      table
    );

    // need snapshot to exactly match the query
    expect(query).toMatchInlineSnapshot(`
      "query GetTest {
        test {
          id
      		name
        }
      }
          "
    `);
    expect(variables).toBe(undefined);
  });
  test('When generateGqlQueryFromTable is used with a mutation and table Then it should genrate a operation', async () => {
    const operationType = 'mutation';
    const { query, variables } = generateGqlQueryFromTable(
      operationType,
      table
    );

    // need snapshot to exactly match the mutation
    expect(query).toMatchInlineSnapshot(`
      "mutation InsertTest($name: String) {
        insert_test(objects: {name: $name}) {
          affected_rows
          returning {
            id
      			name
          }
        }
      }
          "
    `);
    expect(variables).toEqual({ name: '' });
  });
  test('When generateGqlQueryFromTable is used with a streaming subscription and table Then it should genrate a operation', async () => {
    const operationType = 'streaming_subscription';
    const { query, variables } = generateGqlQueryFromTable(
      operationType,
      table
    );

    // need snapshot to exactly match the streaming_subscription
    expect(query).toMatchInlineSnapshot(`
      "subscription GetTestStreamingSubscription {
        test_stream(batch_size: 10, cursor: {initial_value: {id: 0}}) {
          id
      		name
        }
      }
          "
    `);
    expect(variables).toBe(undefined);
  });
  test('When generateGqlQueryFromTable is used with a subscription and table Then it should genrate a operation', async () => {
    const operationType = 'subscription';
    const { query, variables } = generateGqlQueryFromTable(
      operationType,
      table
    );

    // need snapshot to exactly match the subscription
    expect(query).toMatchInlineSnapshot(`
      "subscription GetTestStreamingSubscription {
        test {
          id
      		name
        }
      }
          

          "
    `);
    expect(variables).toBe(undefined);
  });
});
