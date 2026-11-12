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
    case 'float':
      return typeof rawValue === 'string'
        ? Number.parseFloat(rawValue)
        : rawValue;
    case 'json':
      try {
        return typeof rawValue === 'string' ? JSON.parse(rawValue) : rawValue;
      } catch (e) {
        throw Object.assign(
          new Error(
            `${column.name} ' :: could not read ${rawValue} as a valid JSON'`,
          ),
          { cause: e },
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

/**
 * Transforms `{ column: value }` into `{ column: { _eq: value } }`.
 *
 * `_and`/`_or` take a list of boolean expressions whose column values are
 * already comparisons, e.g. `{ _or: [{ id: { _eq: 1 } }, { id: { _eq: 2 } }] }`
 * to delete the selected rows. Unknown columns are ignored, as are branches
 * left empty by that.
 */
export const transformWhereBoolExp = (
  where: Record<string, unknown>,
  columns: TableColumn[],
  graphqlMode: boolean,
) => {
  const getColumnKey = (key: string) => {
    const columnInfo = columns.find((col) => col.name === key);
    if (!columnInfo) return undefined;

    return graphqlMode && columnInfo.graphQLProperties?.name
      ? columnInfo.graphQLProperties.name
      : key;
  };

  const transformComparisons = (exp: Record<string, unknown>) =>
    Object.entries(exp).reduce(
      (acc, [key, comparison]) => {
        const columnKey = getColumnKey(key);
        // a comparison with an undefined operand is dropped by JSON and
        // would match every row
        const isComplete =
          comparison !== undefined &&
          !(
            comparison &&
            typeof comparison === 'object' &&
            Object.values(comparison).includes(undefined)
          );
        if (columnKey && isComplete) acc[columnKey] = comparison;
        return acc;
      },
      {} as Record<string, any>,
    );

  return Object.entries(where).reduce(
    (acc, [key, value]) => {
      if ((key === '_and' || key === '_or') && Array.isArray(value)) {
        const expressions = value
          .map(transformComparisons)
          .filter((exp) => Object.keys(exp).length);
        if (expressions.length) acc[key] = expressions;
        return acc;
      }

      const columnKey = getColumnKey(key);
      // only support the equal boolean expression. An undefined value, e.g. a
      // key column missing from the row, would be dropped by JSON and match
      // every row, so leave it out.
      if (columnKey && value !== undefined) acc[columnKey] = { _eq: value };

      return acc;
    },
    {} as Record<string, any>,
  );
};
