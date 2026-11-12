import {
  getNamedType,
  getNullableType,
  GraphQLSchema,
  isInputObjectType,
  isLeafType,
  isListType,
  isObjectType,
} from 'graphql';
import { MetadataTable, Source } from '@hasura/shared/types';
import { getQueryRoot } from './getQueryRoot';
import { findRootField, getRootFieldMap } from './getRootField';
import { createColumnFieldResolver, getArg } from '../common/tableFields';

export type ComparisonOperator = {
  name: string;
  /** the GraphQL type name of the operand, e.g. `String` or `jsonb` */
  type: string;
  /** whether the operand is a list, e.g. of `_in` */
  isList: boolean;
};

/**
 * Returns the comparison operators each table column supports in filters,
 * keyed by column name, e.g. `{ title: [{ name: '_eq', type: 'String' }] }`.
 * They are the fields of the column's comparison type in the select `where`
 * argument, so they depend on the column type and the driver, and honour
 * naming conventions (e.g. `_isNull`). Nested expressions (e.g. `_cast`) are
 * left out.
 */
export const getColumnComparisonOperators = ({
  schema,
  defaultQueryRoot,
  columns,
  tableCustomization,
  sourceCustomization,
}: {
  schema: GraphQLSchema;
  defaultQueryRoot: string;
  /** table column names */
  columns: string[];
  tableCustomization: MetadataTable['configuration'];
  sourceCustomization: Source['customization'];
}): Record<string, ComparisonOperator[]> => {
  const namespace = sourceCustomization?.root_fields?.namespace || undefined;
  const rootField = findRootField(
    getRootFieldMap(schema.getQueryType(), namespace),
    getQueryRoot({
      defaultQueryRoot,
      operation: 'select',
      configuration: tableCustomization,
      sourceCustomization,
    }),
  );
  const rowType = rootField && getNamedType(rootField.type);
  const whereArg = rootField && getArg(rootField, 'where');
  const boolExpType = whereArg && getNamedType(whereArg.type);
  if (!isObjectType(rowType) || !isInputObjectType(boolExpType)) return {};

  const getFieldName = createColumnFieldResolver(
    rowType.getFields(),
    tableCustomization,
  );
  const boolExpFields = boolExpType.getFields();

  return Object.fromEntries(
    columns.flatMap((column) => {
      const fieldName = getFieldName(column);
      const comparisonType =
        fieldName &&
        boolExpFields[fieldName] &&
        getNamedType(boolExpFields[fieldName].type);
      if (!isInputObjectType(comparisonType)) return [];

      const operators = Object.values(comparisonType.getFields())
        .filter((field) => isLeafType(getNamedType(field.type)))
        .map((field) => ({
          name: field.name,
          type: getNamedType(field.type).name,
          isList: isListType(getNullableType(field.type)),
        }));
      return [[column, operators]];
    }),
  );
};
