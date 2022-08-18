import get from 'lodash.get';
import { IntrospectedTable, Property, Ref, TableColumn } from '../types';
import { RunSQLResponse } from '../api';

export const isProperty = (
  value: Ref | Property | { oneOf: (Property | Ref)[] }
): value is Property => {
  return 'type' in value;
};

export const isRef = (
  value: Ref | Property | { oneOf: (Property | Ref)[] }
): value is Ref => {
  return Object.keys(value).includes('$ref');
};

export const isOneOf = (
  value: Ref | Property | { oneOf: (Property | Ref)[] }
): value is { oneOf: (Property | Ref)[] } => {
  return Object.keys(value).includes('oneOf');
};

export const getProperty = (
  value: Ref | Property,
  otherSchemas: Record<string, Property>
) => {
  if (isRef(value)) {
    const ref = value.$ref;
    return get(otherSchemas, ref.split('/').slice(2).join('.'));
  }

  return value;
};

export const adaptIntrospectedTables = (
  runSqlResponse: RunSQLResponse
): IntrospectedTable[] => {
  /* 
    The `slice(1)` on the result is done because the first item of the result is always the columns names from the SQL output.
    It is not required for the final result and should be avoided 
  */
  const adaptedResponse = runSqlResponse?.result
    ?.slice(1)
    .map((row: string[]) => ({
      name: `${row[1]}.${row[0]}`,
      table: {
        name: row[0],
        schema: row[1],
      },
      type: row[2],
    }));

  return adaptedResponse ?? [];
};

export const adaptTableColumns = (
  result: RunSQLResponse['result']
): TableColumn[] => {
  if (!result) return [];

  return result.slice(1).map(row => ({
    name: row[0],
    dataType: row[1],
  }));
};
