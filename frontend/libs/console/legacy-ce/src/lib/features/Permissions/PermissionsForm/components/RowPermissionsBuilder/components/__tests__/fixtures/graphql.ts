import {
  buildClientSchema,
  GraphQLSchema,
  IntrospectionQuery,
  NamedTypeNode,
  parseType,
  typeFromAST,
} from 'graphql';
import { introspection } from './introspection';

export const schema = new GraphQLSchema(
  buildClientSchema(
    introspection.data as unknown as IntrospectionQuery
  ).toConfig()
);

export function createType(typeName: string) {
  const type = typeFromAST(schema, parseType(typeName) as NamedTypeNode);
  if (!type) {
    throw new Error(`Type ${typeName} not found in schema`);
  }
  return type;
}

export const StringType = createType('String');

export const BooleanType = createType('Boolean');

export const IntType = createType('Int');

export const FloatType = createType('Float');

export const numberType = createType('number');

export const stringType = createType('string');
