import { comparatorsFromSchema } from './comparatorsFromSchema';
import { NamedTypeNode, parseType, typeFromAST } from 'graphql';
import { schema } from '../__tests__/fixtures/graphql';

describe('comparatorsFromSchema', () => {
  it('should return comparators from schema', () => {
    const result = comparatorsFromSchema(schema);
    const operator = result['number_SQLite_comparison_exp'];

    expect(operator?.operators).toEqual([
      {
        name: '_eq',
        operator: '_eq',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: '_gt',
        operator: '_gt',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: '_gte',
        operator: '_gte',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: '_in',
        operator: '_in',
        type: typeFromAST(schema, parseType('[number!]') as NamedTypeNode),
      },
      {
        name: '_is_null',
        operator: '_is_null',
        type: typeFromAST(schema, parseType('Boolean') as NamedTypeNode),
      },
      {
        name: '_lt',
        operator: '_lt',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: '_lte',
        operator: '_lte',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: '_neq',
        operator: '_neq',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: '_nin',
        operator: '_nin',
        type: typeFromAST(schema, parseType('[number!]') as NamedTypeNode),
      },
    ]);
  });
});
