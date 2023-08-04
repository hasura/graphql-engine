import { GraphQLSchema, isObjectType } from 'graphql';

export const getTypesFromIntrospection = (data: GraphQLSchema) => {
  return Object.entries(data.getTypeMap())
    .filter(
      ([typeName, i]) =>
        isObjectType(i) &&
        !typeName.startsWith('__') &&
        !['Mutation', 'Subscription'].includes(typeName)
    )
    .map(([typeName, x]) => ({
      typeName,
      // eslint-disable-next-line no-underscore-dangle
      fields: Object.keys((x as any)._fields || {}),
    }));
};

type rsType = {
  typeName: string;
  fields: string[];
};

export const getFieldTypesFromType = (
  types: rsType[],
  selectedTypeName: string
) => {
  if (types && types?.length) {
    return types.find(i => i.typeName === selectedTypeName)?.fields ?? [];
  }
  return [];
};

const matchAll = (re: RegExp, str: string) => {
  let match;
  const matches = [];
  // eslint-disable-next-line no-cond-assign
  while ((match = re.exec(str)) !== null) {
    // ref : https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/exec
    matches.push(match[0]);
  }

  return matches;
};

export const generateLhsFields = (resultSet: Record<string, unknown>) => {
  const regexp = /([$])\w+/g;
  const str = JSON.stringify(resultSet, null, 2);
  const lhs_fieldSet = new Set<string>();

  const results = matchAll(regexp, str);
  results.forEach(i => lhs_fieldSet.add(i.substring(1))); // remove $ symbol from the string to pass as lhs_fields
  return Array.from(lhs_fieldSet);
};
