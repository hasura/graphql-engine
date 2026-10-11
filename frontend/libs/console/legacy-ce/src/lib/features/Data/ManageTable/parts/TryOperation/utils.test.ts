import { buildSchema } from 'graphql';
import type { TableColumn } from '@hasura/metadata/data-source';
import {
  generateGqlQueryFromTable,
  getTableRootFields,
  toGraphQLLiteral,
} from './utils';

const sdl = (prefix = '') => `
  type ${prefix}test {
    id: Int!
    name: String
    created_at: String!
    rel: [${prefix}test!]!
  }
  input ${prefix}test_insert_input { id: Int name: String created_at: String }
  type ${prefix}test_mutation_response { affected_rows: Int! returning: [${prefix}test!]! }
  input ${prefix}test_stream_cursor_input { initial_value: ${prefix}test_stream_cursor_value_input! }
  input ${prefix}test_stream_cursor_value_input { id: Int created_at: String }
`;

const schema = buildSchema(`
  ${sdl()}
  type query_root { test: [test!]! }
  type mutation_root { insert_test(objects: [test_insert_input!]!): test_mutation_response }
  type subscription_root {
    test: [test!]!
    test_stream(batch_size: Int!, cursor: [test_stream_cursor_input]!): [test!]!
  }
  schema { query: query_root mutation: mutation_root subscription: subscription_root }
`);

const namespacedSchema = buildSchema(`
  ${sdl('my_')}
  type ns_query { my_test: [my_test!]! }
  type query_root { ns: ns_query! }
  schema { query: query_root }
`);

const columns: TableColumn[] = [
  {
    name: 'id',
    dataType: 'integer',
    consoleDataType: 'integer',
    nullable: false,
    defaultValue: "nextval('test_id_seq'::regclass)",
    isPrimaryKey: true,
  },
  { name: 'name', dataType: 'text', consoleDataType: 'text', nullable: true },
  {
    name: 'created_at',
    dataType: 'text',
    consoleDataType: 'text',
    nullable: false,
    defaultValue: null,
  },
];

const rootFields = getTableRootFields({ schema, defaultQueryRoot: 'test' });

const generate = (
  operationType: Parameters<
    typeof generateGqlQueryFromTable
  >[0]['operationType'],
) =>
  generateGqlQueryFromTable({
    operationType,
    tableName: 'test',
    rootFields,
    columns,
  });

describe('generateGqlQueryFromTable', () => {
  it('generates a query selecting only scalar fields', () => {
    expect(generate('query')).toMatchInlineSnapshot(`
      "query GetTest {
        test {
          id
          name
          created_at
        }
      }"
    `);
  });

  it('generates an insert mutation with the required columns', () => {
    expect(generate('mutation')).toMatchInlineSnapshot(`
      "mutation InsertTest {
        insert_test(objects: [{created_at: ""}]) {
          affected_rows
          returning {
            id
            name
            created_at
          }
        }
      }"
    `);
  });

  it('generates a live query subscription', () => {
    expect(generate('subscription')).toMatchInlineSnapshot(`
      "subscription GetTest {
        test {
          id
          name
          created_at
        }
      }"
    `);
  });

  it('generates a streaming subscription using the integer primary key as cursor', () => {
    expect(generate('streaming_subscription')).toMatchInlineSnapshot(`
      "subscription GetTestStreamingSubscription {
        test_stream(batch_size: 10, cursor: {initial_value: {id: 0}}) {
          id
          name
          created_at
        }
      }"
    `);
  });

  it('returns undefined for streaming when no column can be used as cursor', () => {
    expect(
      generateGqlQueryFromTable({
        operationType: 'streaming_subscription',
        tableName: 'test',
        rootFields,
        columns: [columns[1]],
      }),
    ).toBeUndefined();
  });

  it('respects source namespace and prefix customizations, and returns undefined for missing operations', () => {
    const fields = getTableRootFields({
      schema: namespacedSchema,
      defaultQueryRoot: 'test',
      sourceCustomization: {
        root_fields: { namespace: 'ns', prefix: 'my_' },
      },
    });

    expect(
      generateGqlQueryFromTable({
        operationType: 'query',
        tableName: 'test',
        rootFields: fields,
        columns,
      }),
    ).toMatchInlineSnapshot(`
      "query GetTest {
        ns {
          my_test {
            id
            name
            created_at
          }
        }
      }"
    `);
    expect(fields.insert).toBeUndefined();
    expect(fields.stream).toBeUndefined();
  });

  it('resolves camel-cased root fields for the graphql-default naming convention', () => {
    const camelSchema = buildSchema(`
      type Test { id: Int! }
      input TestInsertInput { id: Int }
      type TestMutationResponse { affectedRows: Int! }
      type query_root { test: [Test!]! }
      type mutation_root { insertTest(objects: [TestInsertInput!]!): TestMutationResponse }
      schema { query: query_root mutation: mutation_root }
    `);

    const fields = getTableRootFields({
      schema: camelSchema,
      defaultQueryRoot: 'test',
    });
    expect(fields.insert?.name).toBe('insertTest');
  });
});

describe('toGraphQLLiteral', () => {
  it('prints objects with unquoted keys', () => {
    expect(toGraphQLLiteral({ a: 1, b: 'x', c: { d: [true, null] } })).toBe(
      '{a: 1, b: "x", c: {d: [true, null]}}',
    );
  });
});
