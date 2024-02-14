import { QualifiedFunction } from '../../../hasura-metadata-types';

export const adaptFunctionName = (
  qualifiedFunction: QualifiedFunction
): string[] => {
  if (Array.isArray(qualifiedFunction)) return qualifiedFunction;

  // This is a safe assumption to make because the only native database that supports functions is postgres( and variants)
  if (typeof qualifiedFunction === 'string')
    return ['public', qualifiedFunction];

  const { schema, name } = qualifiedFunction as {
    schema: string;
    name: string;
  };

  return [schema, name];
};

export const search = <T extends { qualifiedFunction: QualifiedFunction }>(
  functions: T[],
  searchText: string
) => {
  if (!searchText.length) return functions;

  return functions.filter(fn =>
    adaptFunctionName(fn.qualifiedFunction)
      .join(' / ')
      .toLowerCase()
      .includes(searchText.toLowerCase())
  );
};

export const functionDisplayName = ({
  qualifiedFunction,
  dataSourceName,
}: {
  qualifiedFunction: QualifiedFunction;
  dataSourceName?: string;
}) => {
  const functionName = adaptFunctionName(qualifiedFunction);

  const name = dataSourceName
    ? `${dataSourceName} / ${functionName.join(' / ')}`
    : functionName.join(' / ');

  return name;
};
