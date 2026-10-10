import { QualifiedFunction, TableFunction } from '@hasura/shared/types';
import { areTablesEqual, areTablesEqualCoalesce } from '../table/predicate';

export const adaptFunction = (
  qualifiedFunction: TableFunction,
): QualifiedFunction => {
  if (Array.isArray(qualifiedFunction)) {
    if (qualifiedFunction.length === 1) {
      return {
        schema: 'public',
        name: qualifiedFunction[0],
      };
    }

    return {
      schema: qualifiedFunction[0],
      name: qualifiedFunction[1],
    };
  }

  // This is a safe assumption to make because the only native database that supports functions is postgres( and variants)
  if (typeof qualifiedFunction === 'string') {
    return {
      schema: 'public',
      name: qualifiedFunction,
    };
  }

  const { schema, name } = qualifiedFunction as {
    schema: string;
    name: string;
  };

  return { schema, name };
};

export const search = <T extends { qualifiedFunction: TableFunction }>(
  functions: T[],
  searchText: string,
) => {
  if (!searchText.length) return functions;

  return functions.filter((fn) => {
    const func = adaptFunction(fn.qualifiedFunction);
    return `${func.schema} / ${func.name}`
      .toLowerCase()
      .includes(searchText.toLowerCase());
  });
};

export const functionDisplayName = ({
  qualifiedFunction,
  dataSourceName,
  separator = ' / ',
}: {
  qualifiedFunction: TableFunction;
  dataSourceName?: string;
  separator?: string;
}) => {
  const func = adaptFunction(qualifiedFunction);
  const functionName = `${func.schema}${separator}${func.name}`;
  const name = dataSourceName
    ? `${dataSourceName}${separator}${functionName}`
    : functionName;

  return name;
};

export const areFunctionsEqual = areTablesEqual;
export const areFunctionsEqualCoalesce = areTablesEqualCoalesce;
