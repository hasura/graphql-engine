import { getNamedType, GraphQLSchema, isEnumType, isObjectType } from 'graphql';
import { MetadataTable, Source } from '@hasura/shared/types';
import { getQueryRoot } from './getQueryRoot';
import { findRootField, getRootFieldMap } from './getRootField';
import { createColumnFieldResolver } from '../common/tableFields';

/**
 * Returns the allowed values of the table columns that reference an enum
 * table, keyed by column name. They are the GraphQL enum value names, so they
 * match what the API returns and accepts under any naming convention.
 */
export const getEnumColumnValues = ({
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
}): Record<string, string[]> => {
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
  if (!isObjectType(rowType)) return {};

  const fields = rowType.getFields();
  const getFieldName = createColumnFieldResolver(fields, tableCustomization);

  return Object.fromEntries(
    columns.flatMap((column) => {
      const fieldName = getFieldName(column);
      const type = fieldName && getNamedType(fields[fieldName].type);
      return isEnumType(type)
        ? [[column, type.getValues().map((value) => value.name)]]
        : [];
    }),
  );
};
