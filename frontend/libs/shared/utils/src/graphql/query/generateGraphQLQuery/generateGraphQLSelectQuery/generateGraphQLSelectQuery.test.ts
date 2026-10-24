import { buildSchema } from 'graphql';
import { MetadataTable, Where } from '@hasura/shared/types';
import { generateGraphQLSelectQuery } from './generateGraphQLSelectQuery';

const hasuraDefaultSchema = buildSchema(`
  scalar jsonb

  enum order_by { asc asc_nulls_first asc_nulls_last desc desc_nulls_first desc_nulls_last }

  input Int_comparison_exp { _eq: Int _neq: Int _gt: Int _in: [Int!] }
  input String_comparison_exp { _eq: String _ilike: String }
  input jsonb_comparison_exp { _contains: jsonb }

  input sales_stores_bool_exp {
    _and: [sales_stores_bool_exp!]
    _or: [sales_stores_bool_exp!]
    _not: sales_stores_bool_exp
    store_id: Int_comparison_exp
    StoreName: String_comparison_exp
    tags: jsonb_comparison_exp
  }

  input sales_stores_order_by { store_id: order_by StoreName: order_by tags: order_by }

  type sales_stores { store_id: Int! StoreName: String tags: jsonb }

  type query_root {
    sales_stores(
      where: sales_stores_bool_exp
      order_by: [sales_stores_order_by!]
      limit: Int
      offset: Int
    ): [sales_stores!]!
  }

  schema { query: query_root }
`);

const columns = ['store_id', 'store_name', 'tags'];

// `store_name` is exposed as `StoreName` through a custom column name
const tableCustomization: MetadataTable['configuration'] = {
  column_config: { store_name: { custom_name: 'StoreName' } },
};

describe('generateGraphQLSelectQuery', () => {
  it('selects the exposed columns of a table', () => {
    const result = generateGraphQLSelectQuery({
      schema: hasuraDefaultSchema,
      defaultQueryRoot: 'sales_stores',
      columns,
      tableCustomization,
      sourceCustomization: undefined,
    });

    expect(result).toEqual({
      query: `query TableRows {
  sales_stores {
    store_id
    StoreName
    tags
  }
}`,
      variables: {},
      resultPath: ['sales_stores'],
      columnNames: {
        store_id: 'store_id',
        StoreName: 'store_name',
        tags: 'tags',
      },
    });
  });

  it('honours the deprecated custom_column_names', () => {
    const result = generateGraphQLSelectQuery({
      schema: hasuraDefaultSchema,
      defaultQueryRoot: 'sales_stores',
      columns,
      tableCustomization: { custom_column_names: { store_name: 'StoreName' } },
      sourceCustomization: undefined,
    });

    expect(result?.columnNames['StoreName']).toBe('store_name');
  });

  it('skips the columns the schema does not expose', () => {
    const result = generateGraphQLSelectQuery({
      schema: hasuraDefaultSchema,
      defaultQueryRoot: 'sales_stores',
      columns: ['store_id', 'hidden'],
      tableCustomization,
      sourceCustomization: undefined,
    });

    expect(result?.columnNames).toEqual({ store_id: 'store_id' });
  });

  it('passes where, order_by, limit and offset as variables', () => {
    const result = generateGraphQLSelectQuery({
      schema: hasuraDefaultSchema,
      defaultQueryRoot: 'sales_stores',
      columns,
      tableCustomization,
      sourceCustomization: undefined,
      options: {
        // jsonb operands are objects, which `WhereClause` doesn't model
        where: {
          _and: [
            { store_id: { _neq: 1 } },
            { store_name: { _ilike: 'say "hi" \\o/' } },
            { tags: { _contains: { featured: true } } },
            { store_id: { _in: '[1, 2]' } },
          ],
        } as unknown as Where,
        order_by: [
          { column: 'store_name', type: 'desc', nulls: 'last' },
          { column: 'store_id', type: 'asc' },
        ],
        limit: 10,
        offset: 20,
      },
    });

    expect(result?.query)
      .toBe(`query TableRows($where: sales_stores_bool_exp, $order_by: [sales_stores_order_by!], $limit: Int, $offset: Int) {
  sales_stores(where: $where, order_by: $order_by, limit: $limit, offset: $offset) {
    store_id
    StoreName
    tags
  }
}`);
    expect(result?.variables).toEqual({
      where: {
        _and: [
          { store_id: { _neq: 1 } },
          { StoreName: { _ilike: 'say "hi" \\o/' } },
          { tags: { _contains: { featured: true } } },
          { store_id: { _in: [1, 2] } },
        ],
      },
      order_by: [{ StoreName: 'desc_nulls_last' }, { store_id: 'asc' }],
      limit: 10,
      offset: 20,
    });
  });

  it('treats a list of where clauses as a conjunction', () => {
    const result = generateGraphQLSelectQuery({
      schema: hasuraDefaultSchema,
      defaultQueryRoot: 'sales_stores',
      columns,
      tableCustomization,
      sourceCustomization: undefined,
      options: { where: [{ store_id: { _gt: 4 } }] },
    });

    expect(result?.variables).toEqual({
      where: { _and: [{ store_id: { _gt: 4 } }] },
    });
  });

  it('resolves table and source root field customizations', () => {
    const schema = buildSchema(`
      type stores { store_id: Int! }
      type sales_namespace { pre_CustomStores_suf(limit: Int): [stores!]! }
      type query_root { sales_namespace: sales_namespace! }
      schema { query: query_root }
    `);

    const result = generateGraphQLSelectQuery({
      schema,
      defaultQueryRoot: 'sales_stores',
      columns: ['store_id'],
      tableCustomization: { custom_root_fields: { select: 'CustomStores' } },
      sourceCustomization: {
        root_fields: {
          namespace: 'sales_namespace',
          prefix: 'pre_',
          suffix: '_suf',
        },
      },
      options: { limit: 5 },
    });

    expect(result?.query).toBe(`query TableRows($limit: Int) {
  sales_namespace { pre_CustomStores_suf(limit: $limit) {
    store_id
  } }
}`);
    expect(result?.resultPath).toEqual([
      'sales_namespace',
      'pre_CustomStores_suf',
    ]);
  });

  it('supports the graphql-default naming convention', () => {
    const schema = buildSchema(`
      enum OrderBy { ASC ASC_NULLS_FIRST DESC DESC_NULLS_FIRST }
      input IntComparisonExp { _eq: Int }
      input SalesStoresBoolExp { storeId: IntComparisonExp }
      input SalesStoresOrderBy { storeId: OrderBy }
      type SalesStores { storeId: Int! }
      type query_root {
        salesStores(where: SalesStoresBoolExp, orderBy: [SalesStoresOrderBy!]): [SalesStores!]!
      }
      schema { query: query_root }
    `);

    const result = generateGraphQLSelectQuery({
      schema,
      defaultQueryRoot: 'sales_stores',
      columns: ['store_id'],
      tableCustomization: undefined,
      sourceCustomization: undefined,
      options: {
        where: { store_id: { _eq: 1 } },
        order_by: [{ column: 'store_id', type: 'desc', nulls: 'first' }],
      },
    });

    expect(result?.query)
      .toBe(`query TableRows($where: SalesStoresBoolExp, $orderBy: [SalesStoresOrderBy!]) {
  salesStores(where: $where, orderBy: $orderBy) {
    storeId
  }
}`);
    expect(result?.variables).toEqual({
      where: { storeId: { _eq: 1 } },
      orderBy: [{ storeId: 'DESC_NULLS_FIRST' }],
    });
    expect(result?.columnNames).toEqual({ storeId: 'store_id' });
  });

  it('returns undefined when the table has no select field', () => {
    expect(
      generateGraphQLSelectQuery({
        schema: hasuraDefaultSchema,
        defaultQueryRoot: 'public_stores',
        columns,
        tableCustomization: undefined,
        sourceCustomization: undefined,
      }),
    ).toBeUndefined();
  });
});
