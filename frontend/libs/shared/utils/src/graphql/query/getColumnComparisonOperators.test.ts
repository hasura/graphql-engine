import { buildSchema } from 'graphql';
import { getColumnComparisonOperators } from './getColumnComparisonOperators';

const schema = buildSchema(`
  scalar jsonb

  input String_cast_exp { String: String_comparison_exp }
  input String_comparison_exp {
    _cast: String_cast_exp
    _eq: String
    _in: [String!]
    _is_null: Boolean
    _like: String
  }
  input Int_comparison_exp { _eq: Int _gt: Int _is_null: Boolean }
  input jsonb_comparison_exp { _contains: jsonb _has_key: String }

  input albums_bool_exp {
    _and: [albums_bool_exp!]
    id: Int_comparison_exp
    name: String_comparison_exp
    data: jsonb_comparison_exp
  }

  type albums { id: Int! name: String data: jsonb }
  type query_root { albums(where: albums_bool_exp): [albums!]! }
  schema { query: query_root }
`);

// `graphql-default` naming convention
const camelCaseSchema = buildSchema(`
  input IntComparisonExp { _eq: Int _isNull: Boolean }
  input AlbumsBoolExp { albumId: IntComparisonExp }
  type Albums { albumId: Int! }
  type query_root { albums(where: AlbumsBoolExp): [Albums!]! }
  schema { query: query_root }
`);

const names = (operators: Record<string, { name: string }[]>) =>
  Object.fromEntries(
    Object.entries(operators).map(([column, columnOperators]) => [
      column,
      columnOperators.map((operator) => operator.name),
    ]),
  );

describe('getColumnComparisonOperators', () => {
  const albumOperators = () =>
    getColumnComparisonOperators({
      schema,
      defaultQueryRoot: 'albums',
      columns: ['id', 'title', 'data'],
      // `title` is exposed as `name`
      tableCustomization: {
        column_config: { title: { custom_name: 'name' } },
      },
      sourceCustomization: undefined,
    });

  it('returns the operand type of each operator', () => {
    const operators = albumOperators();
    expect(operators['title']).toContainEqual({
      name: '_in',
      type: 'String',
      isList: true,
    });
    expect(operators['title']).toContainEqual({
      name: '_is_null',
      type: 'Boolean',
      isList: false,
    });
    expect(operators['data']).toEqual([
      { name: '_contains', type: 'jsonb', isList: false },
      { name: '_has_key', type: 'String', isList: false },
    ]);
  });

  it('returns the operators each column type supports', () => {
    expect(names(albumOperators())).toEqual({
      // no `_like` on a number
      id: ['_eq', '_gt', '_is_null'],
      // no `_gt` on text, nor the nested `_cast` expression
      title: ['_eq', '_in', '_is_null', '_like'],
      data: ['_contains', '_has_key'],
    });
  });

  it('follows the naming convention of the schema', () => {
    expect(
      names(
        getColumnComparisonOperators({
          schema: camelCaseSchema,
          defaultQueryRoot: 'albums',
          columns: ['album_id'],
          tableCustomization: undefined,
          sourceCustomization: undefined,
        }),
      ),
    ).toEqual({ album_id: ['_eq', '_isNull'] });
  });

  it('returns nothing for a table the schema does not expose', () => {
    expect(
      getColumnComparisonOperators({
        schema,
        defaultQueryRoot: 'untracked',
        columns: ['id'],
        tableCustomization: undefined,
        sourceCustomization: undefined,
      }),
    ).toEqual({});
  });
});
