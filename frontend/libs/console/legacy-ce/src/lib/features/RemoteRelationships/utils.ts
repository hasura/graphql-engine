import { GraphQLSchema, isObjectType } from 'graphql';
import { matchAll } from '@hasura/shared/utils';

export const getTypesFromIntrospection = (data: GraphQLSchema) => {
  return Object.entries(data.getTypeMap())
    .filter(
      ([typeName, i]) =>
        isObjectType(i) &&
        !typeName.startsWith('__') &&
        !['Mutation', 'Subscription'].includes(typeName),
    )
    .map(([typeName, x]) => ({
      typeName,

      fields: Object.keys((x as any)._fields || {}),
    }));
};

type rsType = {
  typeName: string;
  fields: string[];
};

export const getFieldTypesFromType = (
  types: rsType[],
  selectedTypeName: string,
) => {
  if (types && types?.length) {
    return types.find((i) => i.typeName === selectedTypeName)?.fields ?? [];
  }
  return [];
};

export const generateLhsFields = (resultSet: Record<string, unknown>) => {
  const regexp = /([$])\w+/g;
  const str = JSON.stringify(resultSet, null, 2);
  const lhs_fieldSet = new Set<string>();

  const results = matchAll(regexp, str);
  results.forEach((i) => lhs_fieldSet.add(i.substring(1))); // remove $ symbol from the string to pass as lhs_fields
  return Array.from(lhs_fieldSet);
};
