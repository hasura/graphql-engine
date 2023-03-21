import { MetadataTable } from '../../../../../hasura-metadata-types';
import {
  GraphQLFieldMap,
  GraphQLInputFieldMap,
  GraphQLInputType,
  GraphQLOutputType,
  GraphQLSchema,
  isInputObjectType,
  isListType,
  isNonNullType,
  isObjectType,
  isScalarType,
} from 'graphql';

export type GetTypeResult = {
  type: string;
  isList?: boolean;
  isObject?: boolean;
};

const getType = (
  type: GraphQLOutputType | GraphQLInputType
): GetTypeResult | null => {
  if (isScalarType(type)) {
    return { type: type.name };
  }

  if (isNonNullType(type)) {
    return getType(type.ofType);
  }

  if (isObjectType(type)) {
    if (type.name.includes('aggregate')) {
      return null;
    }
    return { type: type.name, isObject: true };
  }

  if (isListType(type)) {
    const listType = getType(type.ofType);
    if (listType?.type) {
      return { type: listType.type, isList: true };
    }
    return null;
  }

  return null;
};

export const getFields = (tableName: string, schema: GraphQLSchema) => {
  const type = schema.getType(tableName);

  if (isObjectType(type) || isInputObjectType(type)) {
    const fields = type.getFields();

    return fields;
  }

  return {};
};

const createFieldObject = (
  fields: GraphQLFieldMap<any, any> | GraphQLInputFieldMap
) => {
  return Object.values(fields).map(field => {
    const type = getType(field.type);

    return { name: field.name, field, type };
  });
};

interface GetColumnOperatorsArgs {
  tableName: string;
  columnName: string;
  schema: GraphQLSchema;
  tableConfig: MetadataTable['configuration'];
}

export const getColumnOperators = ({
  tableName,
  columnName,
  schema,
  tableConfig,
}: GetColumnOperatorsArgs) => {
  const fields = getFields(`${tableName}_bool_exp`, schema);

  const customName =
    tableConfig?.column_config?.[columnName]?.custom_name ?? columnName;
  const col = fields?.[customName];

  if (col?.type && !isListType(col?.type)) {
    const colType = schema.getType(col.type.name);
    if (isInputObjectType(colType)) {
      const operators = colType.getFields();
      return Object.entries(operators).map(([name, field]) => ({
        name,
        type: getType(field.type),
      }));
    }
  }

  return [];
};

export const getColumns = (
  fields:
    | GraphQLInputFieldMap
    | GraphQLFieldMap<
        any,
        any,
        {
          [key: string]: any;
        }
      >
) => {
  const fieldsObject = createFieldObject(fields);

  return fieldsObject.filter(
    field => field.type && !field.type.isList && !field.type.isObject
  );
};

export const getRelationships = (
  fields:
    | GraphQLInputFieldMap
    | GraphQLFieldMap<
        any,
        any,
        {
          [key: string]: any;
        }
      >
) => {
  const fieldsObject = createFieldObject(fields);
  return fieldsObject.filter(
    field => field.type && (field.type.isList || field.type.isObject)
  );
};

export const getBoolOperators = () => {
  return ['_and', '_or', '_not'];
};

const getExistOperators = () => {
  return ['_exists'];
};

interface Args {
  tableName: string;
  schema: GraphQLSchema;
  tableConfig: MetadataTable['configuration'];
}

const getOriginalTableNameFromCustomName = (
  tableConfig: MetadataTable['configuration'],
  columnName: string
) => {
  const columnConfig = tableConfig?.column_config;

  const matchingEntry = Object.entries(columnConfig ?? {}).find(value => {
    return value?.[1]?.custom_name === columnName;
  });

  return matchingEntry?.[0] ?? columnName;
};
/**
 *
 * Returns a list of all the boolOperators, columns and relationships for the selected table
 */
export const getAllColumnsAndOperators = ({
  tableName,
  schema,
  tableConfig,
}: Args) => {
  const metadataTableName = tableConfig?.custom_name ?? tableName;
  const fields = getFields(metadataTableName, schema);
  const boolOperators = getBoolOperators();
  const existOperators = getExistOperators();
  const columns = getColumns(fields);
  const relationships = getRelationships(fields);

  const boolMap = boolOperators.map(boolOperator => ({
    name: boolOperator,
    kind: 'boolOperator',
    meta: null,
  }));
  const existMap = existOperators.map(existOperator => ({
    name: existOperator,
    kind: 'existOperator',
    meta: null,
  }));
  const colMap = columns.map(column => ({
    name: getOriginalTableNameFromCustomName(tableConfig, column.name),
    kind: 'column',
    meta: column,
  }));
  const relMap = relationships.map(relationship => ({
    name: relationship.name,
    kind: 'relationship',
    meta: relationship,
  }));
  return {
    boolOperators: boolMap,
    existOperators: existMap,
    columns: colMap,
    relationships: relMap,
  };
};
