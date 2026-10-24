import { buildSchema, parse, validate } from 'graphql';
import { generateGraphQLRowMutation } from './generateGraphQLRowMutation';

const hasuraDefaultSchema = buildSchema(`
  scalar jsonb

  input Int_comparison_exp { _eq: Int }
  input String_comparison_exp { _eq: String }

  input sales_stores_bool_exp {
    _and: [sales_stores_bool_exp!]
    store_id: Int_comparison_exp
    StoreName: String_comparison_exp
  }
  input sales_stores_insert_input { store_id: Int StoreName: String tags: jsonb }
  input sales_stores_set_input { StoreName: String tags: jsonb }

  type sales_stores { store_id: Int! StoreName: String tags: jsonb }
  type sales_stores_mutation_response {
    affected_rows: Int!
    returning: [sales_stores!]!
  }

  type query_root { sales_stores: [sales_stores!]! }
  type mutation_root {
    insert_sales_stores(
      objects: [sales_stores_insert_input!]!
    ): sales_stores_mutation_response
    update_sales_stores(
      _set: sales_stores_set_input
      where: sales_stores_bool_exp!
    ): sales_stores_mutation_response
    delete_sales_stores(
      where: sales_stores_bool_exp!
    ): sales_stores_mutation_response
  }
  schema { query: query_root mutation: mutation_root }
`);

// `graphql-default` naming convention, a root field namespace, a custom table
// name and a custom column name
const customizedSchema = buildSchema(`
  input IntComparisonExp { _eq: Int }

  input StoresBoolExp { storeId: IntComparisonExp }
  input StoresInsertInput { storeId: Int name: String }
  input StoresSetInput { name: String }

  type Stores { storeId: Int! name: String }
  type StoresMutationResponse { affectedRows: Int! returning: [Stores!]! }

  type shop_mutation_frontend {
    insertStores(objects: [StoresInsertInput!]!): StoresMutationResponse
    updateStores(_set: StoresSetInput, where: StoresBoolExp!): StoresMutationResponse
    deleteStores(where: StoresBoolExp!): StoresMutationResponse
  }
  type query_root { noop: Int }
  type mutation_root { shop: shop_mutation_frontend }
  schema { query: query_root mutation: mutation_root }
`);

const customizedTable = {
  custom_name: 'stores',
  column_config: { StoreName: { custom_name: 'name' } },
};

const expectValid = (
  schema: typeof hasuraDefaultSchema,
  query: string | undefined,
) => {
  expect(query).toBeDefined();
  expect(validate(schema, parse(query ?? ''))).toEqual([]);
};

describe('generateGraphQLRowMutation', () => {
  it('generates an insert mutation returning the requested columns', () => {
    const mutation = generateGraphQLRowMutation({
      schema: hasuraDefaultSchema,
      defaultQueryRoot: 'sales_stores',
      operation: 'insert',
      tableCustomization: undefined,
      sourceCustomization: undefined,
      objects: [{ StoreName: 'a', tags: { x: 1 } }],
      returning: ['store_id', 'StoreName'],
    });

    expectValid(hasuraDefaultSchema, mutation?.query);
    expect(mutation?.query).toContain(
      'insert_sales_stores(objects: $objects) { affected_rows returning { store_id StoreName } }',
    );
    expect(mutation?.variables).toEqual({
      objects: [{ StoreName: 'a', tags: { x: 1 } }],
    });
    expect(mutation?.resultPath).toEqual(['insert_sales_stores']);
    expect(mutation?.affectedRowsField).toBe('affected_rows');
    expect(mutation?.returningField).toBe('returning');
    expect(mutation?.columnNames).toEqual({
      store_id: 'store_id',
      StoreName: 'StoreName',
    });
  });

  it('generates update and delete mutations with a where variable', () => {
    const update = generateGraphQLRowMutation({
      schema: hasuraDefaultSchema,
      defaultQueryRoot: 'sales_stores',
      operation: 'update',
      tableCustomization: undefined,
      sourceCustomization: undefined,
      set: { StoreName: 'b' },
      where: { store_id: { _eq: 1 } },
    });
    expectValid(hasuraDefaultSchema, update?.query);
    expect(update?.variables).toEqual({
      _set: { StoreName: 'b' },
      where: { store_id: { _eq: 1 } },
    });
    expect(update?.returningField).toBeUndefined();

    const remove = generateGraphQLRowMutation({
      schema: hasuraDefaultSchema,
      defaultQueryRoot: 'sales_stores',
      operation: 'delete',
      tableCustomization: undefined,
      sourceCustomization: undefined,
      where: { store_id: { _eq: 1 } },
    });
    expectValid(hasuraDefaultSchema, remove?.query);
    expect(remove?.variables).toEqual({ where: { store_id: { _eq: 1 } } });
  });

  it('resolves customized and camel-cased names from the schema', () => {
    const sourceCustomization = { root_fields: { namespace: 'shop' } };

    const insert = generateGraphQLRowMutation({
      schema: customizedSchema,
      defaultQueryRoot: 'sales_stores',
      operation: 'insert',
      tableCustomization: customizedTable,
      sourceCustomization,
      objects: [{ store_id: 1, StoreName: 'a' }],
      returning: ['store_id', 'StoreName'],
    });
    expectValid(customizedSchema, insert?.query);
    expect(insert?.variables).toEqual({
      objects: [{ storeId: 1, name: 'a' }],
    });
    expect(insert?.resultPath).toEqual(['shop', 'insertStores']);
    expect(insert?.affectedRowsField).toBe('affectedRows');
    expect(insert?.columnNames).toEqual({
      storeId: 'store_id',
      name: 'StoreName',
    });

    const update = generateGraphQLRowMutation({
      schema: customizedSchema,
      defaultQueryRoot: 'sales_stores',
      operation: 'update',
      tableCustomization: customizedTable,
      sourceCustomization,
      set: { StoreName: 'b' },
      where: { store_id: { _eq: 1 } },
    });
    expectValid(customizedSchema, update?.query);
    expect(update?.variables).toEqual({
      _set: { name: 'b' },
      where: { storeId: { _eq: 1 } },
    });
  });

  it('sends enum table values as the enum names of the naming convention', () => {
    // `graphql-default` upper-cases the values of enum table based enums
    const enumSchema = buildSchema(`
      enum StatusEnum { ACTIVE ARCHIVED }
      input StatusEnumComparisonExp { _eq: StatusEnum _in: [StatusEnum!] }
      input TasksBoolExp { _or: [TasksBoolExp!] status: StatusEnumComparisonExp }
      input TasksInsertInput { title: String status: StatusEnum }
      input TasksSetInput { status: StatusEnum }

      type Tasks { title: String status: StatusEnum }
      type TasksMutationResponse { affectedRows: Int! returning: [Tasks!]! }

      type query_root { tasks: [Tasks!]! }
      type mutation_root {
        insertTasks(objects: [TasksInsertInput!]!): TasksMutationResponse
        updateTasks(_set: TasksSetInput, where: TasksBoolExp!): TasksMutationResponse
      }
      schema { query: query_root mutation: mutation_root }
    `);

    const insert = generateGraphQLRowMutation({
      schema: enumSchema,
      defaultQueryRoot: 'tasks',
      operation: 'insert',
      tableCustomization: undefined,
      sourceCustomization: undefined,
      objects: [{ title: 'active', status: 'active' }],
    });
    expectValid(enumSchema, insert?.query);
    // only the enum field is converted
    expect(insert?.variables).toEqual({
      objects: [{ title: 'active', status: 'ACTIVE' }],
    });

    const update = generateGraphQLRowMutation({
      schema: enumSchema,
      defaultQueryRoot: 'tasks',
      operation: 'update',
      tableCustomization: undefined,
      sourceCustomization: undefined,
      set: { status: 'archived' },
      where: {
        _or: [
          { status: { _eq: 'active' } },
          { status: { _in: ['ARCHIVED', 'unknown'] } },
        ],
      } as never,
    });
    expectValid(enumSchema, update?.query);
    expect(update?.variables).toEqual({
      _set: { status: 'ARCHIVED' },
      // an unknown value is kept for the server to report
      where: {
        _or: [
          { status: { _eq: 'ACTIVE' } },
          { status: { _in: ['ARCHIVED', 'unknown'] } },
        ],
      },
    });
  });

  it('returns undefined when the schema has no mutation for the table', () => {
    expect(
      generateGraphQLRowMutation({
        schema: hasuraDefaultSchema,
        defaultQueryRoot: 'untracked',
        operation: 'delete',
        tableCustomization: undefined,
        sourceCustomization: undefined,
        where: { id: { _eq: 1 } },
      }),
    ).toBeUndefined();
  });
});
