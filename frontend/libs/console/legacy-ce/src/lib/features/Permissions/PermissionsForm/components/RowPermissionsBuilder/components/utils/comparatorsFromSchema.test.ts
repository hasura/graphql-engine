import { comparatorsFromSchema } from './comparatorsFromSchema';
import { NamedTypeNode, parseType, typeFromAST } from 'graphql';
import { schema } from '../__tests__/fixtures/graphql';

describe('comparatorsFromSchema', () => {
  it('should return comparators from schema', () => {
    const result = comparatorsFromSchema(schema);
    const operator = result['number_SQLite'];

    expect(operator?.operators).toEqual([
      {
        name: '_eq',
        operator: '_eq',
        graphqlType: typeFromAST(schema, parseType('number') as NamedTypeNode),
        inputType: undefined,
        inputStructure: 'object',
        type: 'comparision',
      },
      {
        name: '_gt',
        operator: '_gt',
        graphqlType: typeFromAST(schema, parseType('number') as NamedTypeNode),
        inputType: undefined,
        inputStructure: 'object',
        type: 'comparision',
      },
      {
        name: '_gte',
        operator: '_gte',
        graphqlType: typeFromAST(schema, parseType('number') as NamedTypeNode),
        inputType: undefined,
        inputStructure: 'object',
        type: 'comparision',
      },
      {
        name: '_in',
        operator: '_in',
        inputType: undefined,
        inputStructure: 'array',
        type: 'comparision',
        graphqlType: typeFromAST(
          schema,
          parseType('[number!]') as NamedTypeNode
        ),
      },
      {
        name: '_is_null',
        operator: '_is_null',
        graphqlType: typeFromAST(schema, parseType('Boolean') as NamedTypeNode),
        inputType: 'boolean',
        inputStructure: 'object',
        type: 'is_null',
      },
      {
        name: '_lt',
        operator: '_lt',
        graphqlType: typeFromAST(schema, parseType('number') as NamedTypeNode),
        inputType: undefined,
        inputStructure: 'object',
        type: 'comparision',
      },
      {
        name: '_lte',
        operator: '_lte',
        graphqlType: typeFromAST(schema, parseType('number') as NamedTypeNode),
        inputType: undefined,
        inputStructure: 'object',
        type: 'comparision',
      },
      {
        name: '_neq',
        operator: '_neq',
        graphqlType: typeFromAST(schema, parseType('number') as NamedTypeNode),
        inputType: undefined,
        inputStructure: 'object',
        type: 'comparision',
      },
      {
        name: '_nin',
        operator: '_nin',
        inputType: undefined,
        inputStructure: 'array',
        type: 'comparision',
        graphqlType: typeFromAST(
          schema,
          parseType('[number!]') as NamedTypeNode
        ),
      },
      {
        name: '_ceq',
        operator: '_ceq',
      },
      {
        name: '_cne',
        operator: '_cne',
      },
      {
        name: '_cgt',
        operator: '_cgt',
      },
      {
        name: '_clt',
        operator: '_clt',
      },
      {
        name: '_cgte',
        operator: '_cgte',
      },
      {
        name: '_clte',
        operator: '_clte',
      },
    ]);
  });
});
