import {
  getNamedType,
  GraphQLField,
  GraphQLFieldMap,
  GraphQLInputFieldMap,
  GraphQLInputType,
  isEnumType,
  isInputObjectType,
  isLeafType,
  isListType,
  isNonNullType,
  isScalarType,
} from 'graphql';
import { camelize } from 'inflection';
import { MetadataTable, Where } from '@hasura/shared/types';

export type BoolExp = Record<string, unknown>;

// a filter value typed as text, e.g. `true` for `_is_null` or `5` for `_gt`
const toBuiltInScalarValue = (typeName: string, value: string): unknown => {
  const text = value.trim();
  if (typeName === 'Boolean' && (text === 'true' || text === 'false'))
    return text === 'true';
  if (typeName === 'Int' && /^[+-]?\d+$/.test(text)) {
    const number = Number(text);
    return Number.isSafeInteger(number) ? number : value;
  }
  if (typeName === 'Float' && text !== '' && Number.isFinite(Number(text)))
    return Number(text);
  return value;
};

/**
 * Converts a variable value to its GraphQL input type:
 * - the values of an enum table column are matched to the enum value names,
 *   which the `graphql-default` naming convention upper-cases
 *   (`active` -> `ACTIVE`);
 * - text sent to a built-in `Boolean`, `Int` or `Float` input is parsed.
 * Anything else, e.g. a Postgres array as a JSON array, is kept as-is so the
 * server reports an invalid value.
 */
export const toGraphQLInputValue = (
  type: GraphQLInputType,
  value: unknown,
): unknown => {
  if (value === null || value === undefined) return value;
  if (isNonNullType(type)) return toGraphQLInputValue(type.ofType, value);

  if (isScalarType(type) && typeof value === 'string')
    return toBuiltInScalarValue(type.name, value);

  if (isListType(type))
    return Array.isArray(value)
      ? value.map((item) => toGraphQLInputValue(type.ofType, item))
      : toGraphQLInputValue(type.ofType, value);

  if (isEnumType(type) && typeof value === 'string') {
    const values = type.getValues();
    return (
      values.find((v) => v.name === value)?.name ??
      values.find((v) => v.name.toLowerCase() === value.toLowerCase())?.name ??
      value
    );
  }

  if (isInputObjectType(type) && typeof value === 'object') {
    const fields = type.getFields();
    return Object.fromEntries(
      Object.entries(value).map(([key, fieldValue]) => [
        key,
        fields[key]
          ? toGraphQLInputValue(fields[key].type, fieldValue)
          : fieldValue,
      ]),
    );
  }

  return value;
};

const toGraphQLOperand = (operator: string, value: unknown) => {
  // list operands may come from a text input, e.g. `[1, 2]`
  if ((operator === '_in' || operator === '_nin') && typeof value === 'string')
    try {
      const parsed = JSON.parse(value);
      if (Array.isArray(parsed)) return parsed;
    } catch {
      // not JSON, let the server report the invalid value
    }

  return value;
};

/**
 * Returns the GraphQL field name candidates of a column, from the most to the
 * least specific. Custom names win, `graphql-default` camel-cases the rest.
 */
const getColumnFieldCandidates = (
  column: string,
  configuration: MetadataTable['configuration'],
) => [
  configuration?.column_config?.[column]?.custom_name,
  configuration?.custom_column_names?.[column],
  column,
  camelize(column, true),
];

/**
 * Returns a resolver of the GraphQL field name of a table column among the
 * given (output or input) type fields.
 */
export const createColumnFieldResolver =
  (
    fields: GraphQLFieldMap<unknown, unknown> | GraphQLInputFieldMap,
    configuration: MetadataTable['configuration'],
  ) =>
  (column: string) =>
    getColumnFieldCandidates(column, configuration).find(
      (name): name is string =>
        !!name && !!fields[name] && isLeafType(getNamedType(fields[name].type)),
    );

export const getArg = (field: GraphQLField<unknown, unknown>, name: string) =>
  field.args.find((arg) => arg.name === name) ??
  field.args.find((arg) => arg.name === camelize(name, true));

/**
 * Returns a converter of a column keyed filter into a GraphQL boolean
 * expression. Filters on a column that isn't exposed are sent as-is so the
 * server reports them.
 */
export const createBoolExpConverter = (
  getFieldName: (column: string) => string | undefined,
) => {
  const toBoolExp = (where: Where | Where[]): BoolExp => {
    if (Array.isArray(where)) return { _and: where.map(toBoolExp) };

    return Object.fromEntries(
      Object.entries(where).map(([key, value]) => {
        if (key === '_and' || key === '_or')
          return [key, (value as Where[]).map(toBoolExp)];
        if (key === '_not') return [key, toBoolExp(value as unknown as Where)];

        return [
          getFieldName(key) ?? key,
          Object.fromEntries(
            Object.entries(value).map(([operator, operand]) => [
              operator,
              toGraphQLOperand(operator, operand),
            ]),
          ),
        ];
      }),
    );
  };

  return toBoolExp;
};
