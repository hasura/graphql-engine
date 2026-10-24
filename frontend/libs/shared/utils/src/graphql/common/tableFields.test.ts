import { buildSchema, GraphQLInputObjectType } from 'graphql';
import { toGraphQLInputValue } from './tableFields';

const schema = buildSchema(`
  scalar _text
  scalar jsonb

  input items_insert_input {
    tags: [String!]
    labels: _text
    data: jsonb
  }

  type query_root { noop: Int }
  schema { query: query_root }
`);

const insertInput = schema.getType(
  'items_insert_input',
) as GraphQLInputObjectType;

describe('toGraphQLInputValue built-in scalars', () => {
  const comparisonSchema = buildSchema(`
    scalar bigint
    input comparison {
      _is_null: Boolean
      _gt: Int
      _lt: Float
      _eq: String
      _gte: bigint
    }
    type query_root { noop: Int }
    schema { query: query_root }
  `);
  const comparison = comparisonSchema.getType(
    'comparison',
  ) as GraphQLInputObjectType;

  it('parses filter text for Boolean, Int and Float inputs', () => {
    expect(
      toGraphQLInputValue(comparison, {
        _is_null: 'true',
        _gt: ' 5 ',
        _lt: '1.5',
        _eq: '5',
        // a custom scalar keeps its text, e.g. a bigint beyond JS precision
        _gte: '9007199254740993',
      }),
    ).toEqual({
      _is_null: true,
      _gt: 5,
      _lt: 1.5,
      _eq: '5',
      _gte: '9007199254740993',
    });
  });

  it('keeps text that is not a value of the type, for the server to report', () => {
    expect(
      toGraphQLInputValue(comparison, {
        _is_null: 'yes',
        _gt: '1.5',
        _lt: 'abc',
      }),
    ).toEqual({ _is_null: 'yes', _gt: '1.5', _lt: 'abc' });
  });
});

describe('toGraphQLInputValue arrays', () => {
  it('sends Postgres arrays as JSON arrays, whatever the field type', () => {
    expect(
      toGraphQLInputValue(insertInput, {
        tags: ['a b', 'c'],
        labels: ['a b', 'c'],
        data: ['a b', 'c'],
      }),
    ).toEqual({
      tags: ['a b', 'c'],
      labels: ['a b', 'c'],
      data: ['a b', 'c'],
    });
  });
});
