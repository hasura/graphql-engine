import { comparatorsFromSchema } from './comparatorsFromSchema';
import { NamedTypeNode, parseType, typeFromAST } from 'graphql';
import { schema } from '../__tests__/fixtures/graphql';

describe('comparatorsFromSchema', () => {
  it('should return comparators from schema', () => {
    const result = comparatorsFromSchema(schema);
    const operator = result['number_SQLite_comparison_exp'];
    expect(operator?.operators).toEqual([
      {
        name: 'equals',
        operator: '_eq',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: '>',
        operator: '_gt',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: '>=',
        operator: '_gte',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: 'in',
        operator: '_in',
        type: typeFromAST(schema, parseType('[number!]') as NamedTypeNode),
      },
      {
        name: 'is null',
        operator: '_is_null',
        type: typeFromAST(schema, parseType('Boolean') as NamedTypeNode),
      },
      {
        name: '<',
        operator: '_lt',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: '<=',
        operator: '_lte',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: 'not equals',
        operator: '_neq',
        type: typeFromAST(schema, parseType('number') as NamedTypeNode),
      },
      {
        name: 'not in',
        operator: '_nin',
        type: typeFromAST(schema, parseType('[number!]') as NamedTypeNode),
      },
    ]);
  });
});
