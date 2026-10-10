import { WhereClause } from '@hasura/shared/types';
import { TableColumn, ValidateInputRowValuesFunction } from '../types';

type ValidationEncoders = {
  array: (columnName: string, input: unknown) => unknown;
};

export const getValidateInputRowValuesFunction =
  (encoders: ValidationEncoders): ValidateInputRowValuesFunction =>
  (data): Record<string, unknown> => {
    return Object.entries(data.values).reduce(
      (acc, [key, value]) => {
        const columnInfo = data.columns.find((col) => col.name === key);
        if (!columnInfo) {
          // ignore unknown fields.
          return acc;
        }

        const columnKey =
          data.graphqlMode && columnInfo.graphQLProperties?.name
            ? columnInfo.graphQLProperties.name
            : key;
        acc[columnKey] = validateInputValue(encoders, columnInfo, value);

        return acc;
      },
      {} as Record<string, any>,
    );
  };

function validateInputValue(
  encoders: ValidationEncoders,
  column: TableColumn,
  rawValue: unknown,
): unknown {
  switch (column.consoleDataType) {
    case 'boolean':
      return rawValue === null || rawValue === undefined
        ? null
        : rawValue === 'true' || rawValue === true;
    case 'integer':
      return typeof rawValue === 'string'
        ? Number.parseInt(rawValue)
        : rawValue;
    case 'number':
      return typeof rawValue === 'string'
        ? Number.parseFloat(rawValue)
        : rawValue;
    case 'json':
      try {
        return typeof rawValue === 'string' ? JSON.parse(rawValue) : rawValue;
      } catch (e) {
        throw new Error(
          `${column.name} ' :: could not read ${rawValue} as a valid JSON'`,
        );
      }
    case 'array':
      if (!rawValue) {
        return null;
      }

      return encoders.array(column.name, rawValue);
    default:
      return rawValue;
  }
}

export const validateInputRowValues = getValidateInputRowValuesFunction({
  array: (_, value) => value,
});

export const transformWhereBoolExp = (
  where: Record<string, unknown>,
  columns: TableColumn[],
  graphqlMode: boolean,
) => {
  return Object.entries(where).reduce(
    (acc, [key, value]) => {
      const columnInfo = columns.find((col) => col.name === key);
      if (!columnInfo) {
        // ignore unknown fields.
        return acc;
      }

      // only support the equal boolean expression.
      const columnKey =
        graphqlMode && columnInfo.graphQLProperties?.name
          ? columnInfo.graphQLProperties.name
          : key;
      acc[columnKey] = {
        _eq: value,
      };

      return acc;
    },
    {} as Record<string, any>,
  );
};

export const transformSimpleWhereBoolExp = (
  where: Record<string, unknown>,
): WhereClause => {
  return Object.entries(where).reduce(
    (acc, [key, value]) => {
      acc[key] = {
        _eq: value,
      };

      return acc;
    },
    {} as Record<string, any>,
  );
};
